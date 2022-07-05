{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Safe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.IO.Common
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-----------------------------------------------------------------------------

module System.Posix.IO.Common (
    -- * Input \/ Output

    -- ** Standard file descriptors
    stdInput, stdOutput, stdError,

    -- ** Opening and closing files
    OpenMode(..),
    OpenFileFlags(..), defaultFileFlags,
    openat_,
    closeFd,

    -- ** Reading\/writing data
    -- |Programmers using the 'fdRead' and 'fdWrite' API should be aware that
    -- EAGAIN exceptions may occur for non-blocking IO!

    fdReadBuf, fdWriteBuf,

    -- ** Seeking
    fdSeek,

    -- ** File options
    FdOption(..),
    queryFdOption,
    setFdOption,

    -- ** Locking
    FileLock,
    LockRequest(..),
    getLock,  setLock,
    waitToSetLock,

    -- ** Pipes
    createPipe,

    -- ** Duplicating file descriptors
    dup, dupTo,

    -- ** Converting file descriptors to\/from Handles
    handleToFd,
    fdToHandle,

  ) where

import System.IO
import System.IO.Error
import System.Posix.Types
import qualified System.Posix.Internals as Base

import Foreign
import Foreign.C

import GHC.IO.Handle.Internals
import GHC.IO.Handle.Types
import qualified GHC.IO.FD as FD
import qualified GHC.IO.Handle.FD as FD
import GHC.IO.Exception
import Data.Typeable (cast)

#if !defined(HAVE_PIPE)
import System.IO.Error ( ioeSetLocation )
import GHC.IO.Exception ( unsupportedOperation )
#endif

#include "HsUnix.h"

#if !defined(HAVE_PIPE)

createPipe :: IO (Fd, Fd)
{-# WARNING createPipe
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_PIPE@)" #-}
createPipe = ioError (ioeSetLocation unsupportedOperation "createPipe")

#else

-- -----------------------------------------------------------------------------
-- Pipes
-- |The 'createPipe' function creates a pair of connected file
-- descriptors. The first component is the fd to read from, the second
-- is the write end.  Although pipes may be bidirectional, this
-- behaviour is not portable and programmers should use two separate
-- pipes for this purpose.  May throw an exception if this is an
-- invalid descriptor.

createPipe :: IO (Fd, Fd)
createPipe =
  allocaArray 2 $ \p_fd -> do
    throwErrnoIfMinus1_ "createPipe" (c_pipe p_fd)
    rfd <- peekElemOff p_fd 0
    wfd <- peekElemOff p_fd 1
    return (Fd rfd, Fd wfd)

foreign import ccall unsafe "pipe"
   c_pipe :: Ptr CInt -> IO CInt

#endif // HAVE_PIPE

#if !defined(HAVE_DUP)

dup :: Fd -> IO Fd
{-# WARNING dup
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_DUP@)" #-}
dup _ = ioError (ioeSetLocation unsupportedOperation "dup")

dupTo :: Fd -> Fd -> IO Fd
{-# WARNING dupTo
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_DUP@)" #-}
dupTo _ _ = ioError (ioeSetLocation unsupportedOperation "dupTo")

#else

-- -----------------------------------------------------------------------------
-- Duplicating file descriptors

-- | May throw an exception if this is an invalid descriptor.
dup :: Fd -> IO Fd
dup (Fd fd) = do r <- throwErrnoIfMinus1 "dup" (c_dup fd); return (Fd r)

-- | May throw an exception if this is an invalid descriptor.
dupTo :: Fd -> Fd -> IO Fd
dupTo (Fd fd1) (Fd fd2) = do
  r <- throwErrnoIfMinus1 "dupTo" (c_dup2 fd1 fd2)
  return (Fd r)

foreign import ccall unsafe "dup"
   c_dup :: CInt -> IO CInt

foreign import ccall unsafe "dup2"
   c_dup2 :: CInt -> CInt -> IO CInt

#endif // HAVE_DUP

-- -----------------------------------------------------------------------------
-- Opening and closing files

stdInput, stdOutput, stdError :: Fd
stdInput   = Fd (#const STDIN_FILENO)
stdOutput  = Fd (#const STDOUT_FILENO)
stdError   = Fd (#const STDERR_FILENO)

data OpenMode = ReadOnly | WriteOnly | ReadWrite
              deriving (Read, Show, Eq, Ord)

-- |Correspond to some of the int flags from C's fcntl.h.
data OpenFileFlags =
 OpenFileFlags {
    append    :: Bool,           -- ^ O_APPEND
    exclusive :: Bool,           -- ^ O_EXCL, result is undefined if O_CREAT is False
                                 --
                                 -- __NOTE__: Result is undefined if 'creat' is 'Nothing'.
    noctty    :: Bool,           -- ^ O_NOCTTY
    nonBlock  :: Bool,           -- ^ O_NONBLOCK
    trunc     :: Bool,           -- ^ O_TRUNC
    nofollow  :: Bool,           -- ^ O_NOFOLLOW
                                 --
                                 -- @since 2.8.0.0
    creat     :: Maybe FileMode, -- ^ O_CREAT
                                 --
                                 -- @since 2.8.0.0
    cloexec   :: Bool,           -- ^ O_CLOEXEC
                                 --
                                 -- @since 2.8.0.0
    directory :: Bool,           -- ^ O_DIRECTORY
                                 --
                                 -- @since 2.8.0.0
    sync      :: Bool            -- ^ O_SYNC
                                 --
                                 -- @since 2.8.0.0
 }
 deriving (Read, Show, Eq, Ord)


-- | Default values for the 'OpenFileFlags' type.
--
-- Each field of 'OpenFileFlags' is either 'False' or 'Nothing'
-- respectively.
defaultFileFlags :: OpenFileFlags
defaultFileFlags =
 OpenFileFlags {
    append    = False,
    exclusive = False,
    noctty    = False,
    nonBlock  = False,
    trunc     = False,
    nofollow  = False,
    creat     = Nothing,
    cloexec   = False,
    directory = False,
    sync      = False
  }


-- |Open and optionally create a file relative to an optional
-- directory file descriptor.
openat_  :: Maybe Fd -- ^ Optional directory file descriptor
         -> CString -- ^ Pathname to open
         -> OpenMode -- ^ Read-only, read-write or write-only
         -> OpenFileFlags -- ^ Append, exclusive, etc.
         -> IO Fd
openat_ fdMay str how (OpenFileFlags appendFlag exclusiveFlag nocttyFlag
                                nonBlockFlag truncateFlag nofollowFlag
                                creatFlag cloexecFlag directoryFlag
                                syncFlag) =
    Fd <$> c_openat c_fd str all_flags mode_w
  where
    c_fd = maybe (#const AT_FDCWD) (\ (Fd fd) -> fd) fdMay
    all_flags  = creat .|. flags .|. open_mode

    flags =
       (if appendFlag       then (#const O_APPEND)    else 0) .|.
       (if exclusiveFlag    then (#const O_EXCL)      else 0) .|.
       (if nocttyFlag       then (#const O_NOCTTY)    else 0) .|.
       (if nonBlockFlag     then (#const O_NONBLOCK)  else 0) .|.
       (if truncateFlag     then (#const O_TRUNC)     else 0) .|.
       (if nofollowFlag     then (#const O_NOFOLLOW)  else 0) .|.
       (if cloexecFlag      then (#const O_CLOEXEC)   else 0) .|.
       (if directoryFlag    then (#const O_DIRECTORY) else 0) .|.
       (if syncFlag         then (#const O_SYNC)      else 0)

    (creat, mode_w) = case creatFlag of
                        Nothing -> (0,0)
                        Just x  -> ((#const O_CREAT), x)

    open_mode = case how of
                   ReadOnly  -> (#const O_RDONLY)
                   WriteOnly -> (#const O_WRONLY)
                   ReadWrite -> (#const O_RDWR)

foreign import capi unsafe "HsUnix.h openat"
   c_openat :: CInt -> CString -> CInt -> CMode -> IO CInt

-- |Close this file descriptor.  May throw an exception if this is an
-- invalid descriptor.

closeFd :: Fd -> IO ()
closeFd (Fd fd) = throwErrnoIfMinus1_ "closeFd" (c_close fd)
-- Here we don't to retry on EINTR because according to
--  http://pubs.opengroup.org/onlinepubs/9699919799/functions/close.html
-- "with errno set to [EINTR] [...] the state of fildes is unspecified"
-- and on Linux, already the first close() removes the FD from the process's
-- FD table so closing a second time is invalid
-- (see http://man7.org/linux/man-pages/man2/close.2.html#NOTES).

foreign import ccall unsafe "HsUnix.h close"
   c_close :: CInt -> IO CInt

-- -----------------------------------------------------------------------------
-- Converting file descriptors to/from Handles

-- | Extracts the 'Fd' from a 'Handle'.  This function has the side effect
-- of closing the 'Handle' (and flushing its write buffer, if necessary),
-- without closing the underlying 'Fd'.
--
-- __Warning:__ This means you take over ownership of the underlying 'Fd'.
-- 'hClose` on the 'Handle' will no longer have any effect.
-- This will break common patterns to avoid file descriptor leaks,
-- such as using 'hClose' in the cleanup action of @Control.Exception.bracket@,
-- making it a silent no-op.
-- Be sure to close the returned 'Fd' yourself to not leak it.
handleToFd :: Handle -> IO Fd

-- | Converts an 'Fd' into a 'Handle' that can be used with the
-- standard Haskell IO library (see "System.IO").
fdToHandle :: Fd -> IO Handle
fdToHandle fd = FD.fdToHandle (fromIntegral fd)

handleToFd h@(FileHandle _ m) = do
  withHandle' "handleToFd" h m $ handleToFd' h
handleToFd h@(DuplexHandle _ r w) = do
  _ <- withHandle' "handleToFd" h r $ handleToFd' h
  withHandle' "handleToFd" h w $ handleToFd' h
  -- for a DuplexHandle, make sure we mark both sides as closed,
  -- otherwise a finalizer will come along later and close the other
  -- side. (#3914)

handleToFd' :: Handle -> Handle__ -> IO (Handle__, Fd)
handleToFd' h h_@Handle__{haType=_,..} = do
  case cast haDevice of
    Nothing -> ioError (ioeSetErrorString (mkIOError IllegalOperation
                                           "handleToFd" (Just h) Nothing)
                        "handle is not a file descriptor")
    Just fd -> do
     -- converting a Handle into an Fd effectively means
     -- letting go of the Handle; it is put into a closed
     -- state as a result.
     flushWriteBuffer h_
     FD.release fd
     return (Handle__{haType=ClosedHandle,..}, Fd (FD.fdFD fd))


-- -----------------------------------------------------------------------------
-- Fd options

data FdOption = AppendOnWrite     -- ^O_APPEND
              | CloseOnExec       -- ^FD_CLOEXEC
              | NonBlockingRead   -- ^O_NONBLOCK
              | SynchronousWrites -- ^O_SYNC

fdOption2Int :: FdOption -> CInt
fdOption2Int CloseOnExec       = (#const FD_CLOEXEC)
fdOption2Int AppendOnWrite     = (#const O_APPEND)
fdOption2Int NonBlockingRead   = (#const O_NONBLOCK)
fdOption2Int SynchronousWrites = (#const O_SYNC)

-- | May throw an exception if this is an invalid descriptor.
queryFdOption :: Fd -> FdOption -> IO Bool
queryFdOption (Fd fd) opt = do
  r <- throwErrnoIfMinus1 "queryFdOption" (Base.c_fcntl_read fd flag)
  return ((r .&. fdOption2Int opt) /= 0)
 where
  flag    = case opt of
              CloseOnExec       -> (#const F_GETFD)
              _                 -> (#const F_GETFL)

-- | May throw an exception if this is an invalid descriptor.
setFdOption :: Fd -> FdOption -> Bool -> IO ()
setFdOption (Fd fd) opt val = do
  r <- throwErrnoIfMinus1 "setFdOption" (Base.c_fcntl_read fd getflag)
  let r' | val       = r .|. opt_val
         | otherwise = r .&. (complement opt_val)
  throwErrnoIfMinus1_ "setFdOption"
                      (Base.c_fcntl_write fd setflag (fromIntegral r'))
 where
  (getflag,setflag)= case opt of
              CloseOnExec       -> ((#const F_GETFD),(#const F_SETFD))
              _                 -> ((#const F_GETFL),(#const F_SETFL))
  opt_val = fdOption2Int opt

-- -----------------------------------------------------------------------------
-- Seeking

mode2Int :: SeekMode -> CInt
mode2Int AbsoluteSeek = (#const SEEK_SET)
mode2Int RelativeSeek = (#const SEEK_CUR)
mode2Int SeekFromEnd  = (#const SEEK_END)

-- | May throw an exception if this is an invalid descriptor.
fdSeek :: Fd -> SeekMode -> FileOffset -> IO FileOffset
fdSeek (Fd fd) mode off =
  throwErrnoIfMinus1 "fdSeek" (Base.c_lseek fd off (mode2Int mode))

-- -----------------------------------------------------------------------------
-- Locking

data LockRequest = ReadLock
                 | WriteLock
                 | Unlock

type FileLock = (LockRequest, SeekMode, FileOffset, FileOffset)

#if !defined(HAVE_F_GETLK)

getLock :: Fd -> FileLock -> IO (Maybe (ProcessID, FileLock))
{-# WARNING getLock
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_F_GETLK@)" #-}
getLock _ _ = ioError (ioeSetLocation unsupportedOperation "getLock")

setLock :: Fd -> FileLock -> IO ()
{-# WARNING setLock
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_F_GETLK@)" #-}
setLock _ _ = ioError (ioeSetLocation unsupportedOperation "setLock")

waitToSetLock :: Fd -> FileLock -> IO ()
{-# WARNING waitToSetLock
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_F_GETLK@)" #-}
waitToSetLock _ _ = ioError (ioeSetLocation unsupportedOperation "waitToSetLock")

#else

-- | May throw an exception if this is an invalid descriptor.
getLock :: Fd -> FileLock -> IO (Maybe (ProcessID, FileLock))
getLock (Fd fd) lock =
  allocaLock lock $ \p_flock -> do
    throwErrnoIfMinus1_ "getLock" (Base.c_fcntl_lock fd (#const F_GETLK) p_flock)
    result <- bytes2ProcessIDAndLock p_flock
    return (maybeResult result)
  where
    maybeResult (_, (Unlock, _, _, _)) = Nothing
    maybeResult x = Just x

allocaLock :: FileLock -> (Ptr Base.CFLock -> IO a) -> IO a
allocaLock (lockreq, mode, start, len) io =
  allocaBytes (#const sizeof(struct flock)) $ \p -> do
    (#poke struct flock, l_type)   p (lockReq2Int lockreq :: CShort)
    (#poke struct flock, l_whence) p (fromIntegral (mode2Int mode) :: CShort)
    (#poke struct flock, l_start)  p start
    (#poke struct flock, l_len)    p len
    io p

lockReq2Int :: LockRequest -> CShort
lockReq2Int ReadLock  = (#const F_RDLCK)
lockReq2Int WriteLock = (#const F_WRLCK)
lockReq2Int Unlock    = (#const F_UNLCK)

bytes2ProcessIDAndLock :: Ptr Base.CFLock -> IO (ProcessID, FileLock)
bytes2ProcessIDAndLock p = do
  req   <- (#peek struct flock, l_type)   p
  mode  <- (#peek struct flock, l_whence) p
  start <- (#peek struct flock, l_start)  p
  len   <- (#peek struct flock, l_len)    p
  pid   <- (#peek struct flock, l_pid)    p
  return (pid, (int2req req, int2mode mode, start, len))
 where
  int2req :: CShort -> LockRequest
  int2req (#const F_RDLCK) = ReadLock
  int2req (#const F_WRLCK) = WriteLock
  int2req (#const F_UNLCK) = Unlock
  int2req _ = error $ "int2req: bad argument"

  int2mode :: CShort -> SeekMode
  int2mode (#const SEEK_SET) = AbsoluteSeek
  int2mode (#const SEEK_CUR) = RelativeSeek
  int2mode (#const SEEK_END) = SeekFromEnd
  int2mode _ = error $ "int2mode: bad argument"

-- | May throw an exception if this is an invalid descriptor.
setLock :: Fd -> FileLock -> IO ()
setLock (Fd fd) lock = do
  allocaLock lock $ \p_flock ->
    throwErrnoIfMinus1_ "setLock" (Base.c_fcntl_lock fd (#const F_SETLK) p_flock)

-- | May throw an exception if this is an invalid descriptor.
waitToSetLock :: Fd -> FileLock -> IO ()
waitToSetLock (Fd fd) lock = do
  allocaLock lock $ \p_flock ->
    throwErrnoIfMinus1_ "waitToSetLock"
        (Base.c_fcntl_lock fd (#const F_SETLKW) p_flock)

#endif // HAVE_F_GETLK

-- -----------------------------------------------------------------------------
-- fd{Read,Write}Buf

-- | Read data from an 'Fd' into memory.  This is exactly equivalent
-- to the POSIX @read@ function.
fdReadBuf :: Fd
          -> Ptr Word8 -- ^ Memory in which to put the data
          -> ByteCount -- ^ Maximum number of bytes to read
          -> IO ByteCount -- ^ Number of bytes read (zero for EOF)
fdReadBuf _fd _buf 0 = return 0
fdReadBuf fd buf nbytes =
  fmap fromIntegral $
    throwErrnoIfMinus1Retry "fdReadBuf" $
      c_safe_read (fromIntegral fd) (castPtr buf) nbytes

foreign import ccall safe "read"
   c_safe_read :: CInt -> Ptr CChar -> CSize -> IO CSsize

-- | Write data from memory to an 'Fd'.  This is exactly equivalent
-- to the POSIX @write@ function.
fdWriteBuf :: Fd
           -> Ptr Word8    -- ^ Memory containing the data to write
           -> ByteCount    -- ^ Maximum number of bytes to write
           -> IO ByteCount -- ^ Number of bytes written
fdWriteBuf fd buf len =
  fmap fromIntegral $
    throwErrnoIfMinus1Retry "fdWriteBuf" $
      c_safe_write (fromIntegral fd) (castPtr buf) len

foreign import ccall safe "write"
   c_safe_write :: CInt -> Ptr CChar -> CSize -> IO CSsize
