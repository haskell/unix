{-# OPTIONS -fffi #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.IO
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX IO support.  These types and functions correspond to the unix
-- functions open(2), close(2), etc.  For more portable functions
-- which are more like fopen(3) and friends from stdio.h, see
-- 'System.IO'.
--
-----------------------------------------------------------------------------

module System.Posix.IO (
    -- * Input \/ Output

    -- ** Standard file descriptors
    stdInput, stdOutput, stdError,

    -- ** Opening and closing files
    OpenMode(..),
    OpenFileFlags(..), defaultFileFlags,
    openFd, createFile,
    closeFd,

    -- ** Reading\/writing data
    -- |Programmers using the 'fdRead' and 'fdWrite' API should be aware that
    -- EAGAIN exceptions may occur for non-blocking IO!

    fdRead, fdWrite,

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
import System.Posix.Error
import System.Posix.Internals

import Foreign
import Foreign.C
import Data.Bits

#ifdef __GLASGOW_HASKELL__
import GHC.IOBase
import GHC.Handle hiding (fdToHandle, openFd)
import qualified GHC.Handle
#endif

#ifdef __HUGS__
import Hugs.Prelude (IOException(..), IOErrorType(..))
import qualified Hugs.IO (handleToFd, openFd)
#endif

#include "HsUnix.h"

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

-- -----------------------------------------------------------------------------
-- Opening and closing files

stdInput, stdOutput, stdError :: Fd
stdInput   = Fd (#const STDIN_FILENO)
stdOutput  = Fd (#const STDOUT_FILENO)
stdError   = Fd (#const STDERR_FILENO)

data OpenMode = ReadOnly | WriteOnly | ReadWrite

-- |Correspond to some of the int flags from C's fcntl.h.
data OpenFileFlags =
 OpenFileFlags {
    append    :: Bool, -- ^ O_APPEND
    exclusive :: Bool, -- ^ O_EXCL
    noctty    :: Bool, -- ^ O_NOCTTY
    nonBlock  :: Bool, -- ^ O_NONBLOCK
    trunc     :: Bool  -- ^ O_TRUNC
 }


-- |Default values for the 'OpenFileFlags' type. False for each of
-- append, exclusive, noctty, nonBlock, and trunc.
defaultFileFlags :: OpenFileFlags
defaultFileFlags =
 OpenFileFlags {
    append    = False,
    exclusive = False,
    noctty    = False,
    nonBlock  = False,
    trunc     = False
  }


-- |Open and optionally create this file.  See 'System.Posix.Files'
-- for information on how to use the 'FileMode' type.
openFd :: FilePath
       -> OpenMode
       -> Maybe FileMode -- ^Just x => creates the file with the given modes, Nothing => the file must exist.
       -> OpenFileFlags
       -> IO Fd
openFd name how maybe_mode (OpenFileFlags append exclusive noctty
				nonBlock truncate) = do
   withCString name $ \s -> do
    fd <- throwErrnoPathIfMinus1 "openFd" name (c_open s all_flags mode_w)
    return (Fd fd)
  where
    all_flags  = creat .|. flags .|. open_mode

    flags =
       (if append    then (#const O_APPEND)   else 0) .|.
       (if exclusive then (#const O_EXCL)     else 0) .|.
       (if noctty    then (#const O_NOCTTY)   else 0) .|.
       (if nonBlock  then (#const O_NONBLOCK) else 0) .|.
       (if truncate  then (#const O_TRUNC)    else 0)

    (creat, mode_w) = case maybe_mode of 
			Nothing -> (0,0)
			Just x  -> ((#const O_CREAT), x)

    open_mode = case how of
		   ReadOnly  -> (#const O_RDONLY)
		   WriteOnly -> (#const O_WRONLY)
		   ReadWrite -> (#const O_RDWR)

-- |Create and open this file in WriteOnly mode.  A special case of
-- 'openFd'.  See 'System.Posix.Files' for information on how to use
-- the 'FileMode' type.

createFile :: FilePath -> FileMode -> IO Fd
createFile name mode
  = openFd name WriteOnly (Just mode) defaultFileFlags{ trunc=True } 

-- |Close this file descriptor.  May throw an exception if this is an
-- invalid descriptor.

closeFd :: Fd -> IO ()
closeFd (Fd fd) = throwErrnoIfMinus1_ "closeFd" (c_close fd)

-- -----------------------------------------------------------------------------
-- Converting file descriptors to/from Handles

-- | Extracts the 'Fd' from a 'Handle'.  This function has the side effect
-- of closing the 'Handle' and flushing its write buffer, if necessary.
handleToFd :: Handle -> IO Fd

-- | Converts an 'Fd' into a 'Handle' that can be used with the
-- standard Haskell IO library (see "System.IO").  
--
-- GHC only: this function has the side effect of putting the 'Fd'
-- into non-blocking mode (@O_NONBLOCK@) due to the way the standard
-- IO library implements multithreaded I\/O.
--
fdToHandle :: Fd -> IO Handle

#ifdef __GLASGOW_HASKELL__
handleToFd h = withHandle "handleToFd" h $ \ h_ -> do
  -- converting a Handle into an Fd effectively means
  -- letting go of the Handle; it is put into a closed
  -- state as a result. 
  let fd = haFD h_
  flushWriteBufferOnly h_
  unlockFile (fromIntegral fd)
    -- setting the Handle's fd to (-1) as well as its 'type'
    -- to closed, is enough to disable the finalizer that
    -- eventually is run on the Handle.
  return (h_{haFD= (-1),haType=ClosedHandle}, Fd (fromIntegral fd))

fdToHandle fd = GHC.Handle.fdToHandle (fromIntegral fd)
#endif

#ifdef __HUGS__
handleToFd h = do
  fd <- Hugs.IO.handleToFd h
  return (fromIntegral fd)

fdToHandle fd = do
  mode <- fdGetMode (fromIntegral fd)
  Hugs.IO.openFd (fromIntegral fd) False mode True
#endif

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
  r <- throwErrnoIfMinus1 "queryFdOption" (c_fcntl_read fd flag)
  return ((r .&. fdOption2Int opt) /= 0)
 where
  flag    = case opt of
	      CloseOnExec       -> (#const F_GETFD)
	      other		-> (#const F_GETFL)

-- | May throw an exception if this is an invalid descriptor.
setFdOption :: Fd -> FdOption -> Bool -> IO ()
setFdOption (Fd fd) opt val = do
  r <- throwErrnoIfMinus1 "setFdOption" (c_fcntl_read fd getflag)
  let r' | val       = r .|. opt_val
	 | otherwise = r .&. (complement opt_val)
  throwErrnoIfMinus1_ "setFdOption" (c_fcntl_write fd setflag r')
 where
  (getflag,setflag)= case opt of
	      CloseOnExec       -> ((#const F_GETFD),(#const F_SETFD)) 
	      other		-> ((#const F_GETFL),(#const F_SETFL))
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
  throwErrnoIfMinus1 "fdSeek" (c_lseek fd off (mode2Int mode))

-- -----------------------------------------------------------------------------
-- Locking

data LockRequest = ReadLock
                 | WriteLock
                 | Unlock

type FileLock = (LockRequest, SeekMode, FileOffset, FileOffset)

-- | May throw an exception if this is an invalid descriptor.
getLock :: Fd -> FileLock -> IO (Maybe (ProcessID, FileLock))
getLock (Fd fd) lock =
  allocaLock lock $ \p_flock -> do
    throwErrnoIfMinus1_ "getLock" (c_fcntl_lock fd (#const F_GETLK) p_flock)
    result <- bytes2ProcessIDAndLock p_flock
    return (maybeResult result)
  where
    maybeResult (_, (Unlock, _, _, _)) = Nothing
    maybeResult x = Just x

allocaLock :: FileLock -> (Ptr CFLock -> IO a) -> IO a
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

bytes2ProcessIDAndLock :: Ptr CFLock -> IO (ProcessID, FileLock)
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
    throwErrnoIfMinus1_ "setLock" (c_fcntl_lock fd (#const F_SETLK) p_flock)

-- | May throw an exception if this is an invalid descriptor.
waitToSetLock :: Fd -> FileLock -> IO ()
waitToSetLock (Fd fd) lock = do
  allocaLock lock $ \p_flock ->
    throwErrnoIfMinus1_ "waitToSetLock" 
	(c_fcntl_lock fd (#const F_SETLKW) p_flock)

-- -----------------------------------------------------------------------------
-- fd{Read,Write}

-- | May throw an exception if this is an invalid descriptor.
fdRead :: Fd
       -> ByteCount -- ^How many bytes to read
       -> IO (String, ByteCount) -- ^The bytes read, how many bytes were read.
fdRead _fd 0 = return ("", 0)
fdRead (Fd fd) nbytes = do
    allocaBytes (fromIntegral nbytes) $ \ bytes -> do
    rc    <-  throwErrnoIfMinus1Retry "fdRead" (c_read fd bytes nbytes)
    case fromIntegral rc of
      0 -> ioError (IOError Nothing EOF "fdRead" "EOF" Nothing)
      n -> do
       s <- peekCStringLen (bytes, fromIntegral n)
       return (s, n)

-- | May throw an exception if this is an invalid descriptor.
fdWrite :: Fd -> String -> IO ByteCount
fdWrite (Fd fd) str = withCStringLen str $ \ (strPtr,len) -> do
    rc <- throwErrnoIfMinus1Retry "fdWrite" (c_write fd strPtr (fromIntegral len))
    return (fromIntegral rc)
