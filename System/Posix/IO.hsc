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
-- POSIX IO support
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

    -- ** Reading/writing data
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
    handleToFd, fdToHandle,  

  ) where

import System.IO
import System.IO.Error
import System.Posix.Types

import Foreign
import Foreign.C
import Data.Bits

#ifdef __GLASGOW_HASKELL__
import GHC.IOBase
import GHC.Handle hiding (fdToHandle, openFd)
import qualified GHC.Handle
#endif

#include "HsUnix.h"

-- -----------------------------------------------------------------------------
-- Pipes

createPipe :: IO (Fd, Fd)
createPipe =
  allocaArray 2 $ \p_fd -> do
    throwErrnoIfMinus1_ "createPipe" (c_pipe p_fd)
    rfd <- peekElemOff p_fd 0
    wfd <- peekElemOff p_fd 1
    return (rfd, wfd)

foreign import ccall unsafe "pipe"
  c_pipe :: Ptr Fd -> IO CInt

-- -----------------------------------------------------------------------------
-- Duplicating file descriptors

dup :: Fd -> IO Fd
dup fd = do r <- throwErrnoIfMinus1 "dup" (c_dup fd); return (Fd r)

foreign import ccall unsafe "dup"
  c_dup :: Fd -> IO CInt

dupTo :: Fd -> Fd -> IO Fd
dupTo fd1 fd2 = throwErrnoIfMinus1 "dupTp" (c_dup2 fd1 fd2)

foreign import ccall unsafe "dup2"
  c_dup2 :: Fd -> Fd -> IO Fd

-- -----------------------------------------------------------------------------
-- Opening and closing files

stdInput, stdOutput, stdError :: Fd
stdInput   = Fd (#const STDIN_FILENO)
stdOutput  = Fd (#const STDOUT_FILENO)
stdError   = Fd (#const STDERR_FILENO)

data OpenMode = ReadOnly | WriteOnly | ReadWrite

data OpenFileFlags =
 OpenFileFlags {
    append    :: Bool,
    exclusive :: Bool,
    noctty    :: Bool,
    nonBlock  :: Bool,
    trunc     :: Bool
 }

defaultFileFlags :: OpenFileFlags
defaultFileFlags =
 OpenFileFlags {
    append    = False,
    exclusive = False,
    noctty    = False,
    nonBlock  = False,
    trunc     = False
  }

openFd :: FilePath
       -> OpenMode
       -> Maybe FileMode -- Just x => O_CREAT, Nothing => must exist
       -> OpenFileFlags
       -> IO Fd
openFd name how maybe_mode (OpenFileFlags append exclusive noctty
				nonBlock truncate) = do
   withCString name $ \s -> do
    fd <- throwErrnoIfMinus1 "openFd" (c_open s all_flags mode_w)
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

foreign import ccall unsafe "open"
   c_open :: CString -> CInt -> CMode -> IO CInt

createFile :: FilePath -> FileMode -> IO Fd
createFile name mode
  = openFd name WriteOnly (Just mode) defaultFileFlags{ trunc=True } 

closeFd :: Fd -> IO ()
closeFd (Fd fd) = throwErrnoIfMinus1_ "closeFd" (c_close fd)

foreign import ccall unsafe "close"
   c_close :: CInt -> IO CInt

-- -----------------------------------------------------------------------------
-- Converting file descriptors to/from Handles

#ifdef __GLASGOW_HASKELL__
handleToFd :: Handle -> IO Fd
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

fdToHandle :: Fd -> IO Handle
fdToHandle fd = GHC.Handle.fdToHandle (fromIntegral fd)
#endif

-- -----------------------------------------------------------------------------
-- Fd options

data FdOption = AppendOnWrite
	      | CloseOnExec
	      | NonBlockingRead
	      | SynchronousWrites

queryFdOption :: Fd -> FdOption -> IO Bool
queryFdOption fd opt = do
  r <- throwErrnoIfMinus1 "queryFdOption" (c_fcntl_read fd flag)
  return ((r .&. opt_val) /= 0)
 where
  flag    = case opt of
	      CloseOnExec       -> (#const F_GETFD)
	      other		-> (#const F_GETFL)

  opt_val = case opt of
	      CloseOnExec       -> (#const FD_CLOEXEC)
	      AppendOnWrite     -> (#const O_APPEND)
              NonBlockingRead   -> (#const O_NONBLOCK)
	      SynchronousWrites -> (#const O_SYNC)

foreign import ccall unsafe "fcntl"
   c_fcntl_read  :: Fd -> CInt -> IO CInt

setFdOption :: Fd -> FdOption -> Bool -> IO ()
setFdOption fd opt val = do
  r <- throwErrnoIfMinus1 "setFdOption" (c_fcntl_read fd flag)
  let r' | val       = r .|. opt_val
	 | otherwise = r .&. (complement opt_val) 
  throwErrnoIfMinus1_ "setFdOption" (c_fcntl_write fd flag r')
 where
  flag    = case opt of
	      CloseOnExec       -> (#const F_GETFD)
	      other		-> (#const F_GETFL)
  opt_val = case opt of
	      CloseOnExec       -> (#const FD_CLOEXEC)
	      AppendOnWrite     -> (#const O_APPEND)
              NonBlockingRead   -> (#const O_NONBLOCK)
	      SynchronousWrites -> (#const O_SYNC)

foreign import ccall unsafe "fcntl"
   c_fcntl_write :: Fd -> CInt -> CInt -> IO CInt

-- -----------------------------------------------------------------------------
-- Seeking 

mode2Int :: SeekMode -> CInt
mode2Int AbsoluteSeek = (#const SEEK_SET)
mode2Int RelativeSeek = (#const SEEK_CUR)
mode2Int SeekFromEnd  = (#const SEEK_END)

fdSeek :: Fd -> SeekMode -> FileOffset -> IO FileOffset
fdSeek fd mode off =
  throwErrnoIfMinus1 "fdSeek" (c_lseek fd off (mode2Int mode))

foreign import ccall unsafe "lseek"
  c_lseek :: Fd -> COff -> CInt -> IO COff

-- -----------------------------------------------------------------------------
-- Locking

data LockRequest = ReadLock
                 | WriteLock
                 | Unlock

type FileLock = (LockRequest, SeekMode, FileOffset, FileOffset)

type CFLock = ()

getLock :: Fd -> FileLock -> IO (Maybe (ProcessID, FileLock))
getLock fd lock =
  allocaLock lock $ \p_flock -> do
    throwErrnoIfMinus1_ "getLock" (c_fcntl_flock fd (#const F_GETLK) p_flock)
    result <- bytes2ProcessIDAndLock p_flock
    return (maybeResult result)
  where
    maybeResult (_, (Unlock, _, _, _)) = Nothing
    maybeResult x = Just x

foreign import ccall unsafe "fcntl"
  c_fcntl_flock :: Fd -> CInt -> Ptr CFLock -> IO CInt

allocaLock :: FileLock -> (Ptr CFLock -> IO a) -> IO a
allocaLock (lockreq, mode, start, len) io = 
  allocaBytes (#const sizeof(struct flock)) $ \p -> do
    (#poke struct flock, l_type)   p (lockReqToInt lockreq :: CShort)
    (#poke struct flock, l_whence) p (fromIntegral (mode2Int mode) :: CShort)
    (#poke struct flock, l_start)  p start
    (#poke struct flock, l_len)    p len
    io p

lockReqToInt :: LockRequest -> CShort
lockReqToInt ReadLock  = (#const F_RDLCK)
lockReqToInt WriteLock = (#const F_WRLCK)
lockReqToInt Unlock    = (#const F_UNLCK)

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

setLock :: Fd -> FileLock -> IO ()
setLock fd lock = do
  allocaLock lock $ \p_flock ->
    throwErrnoIfMinus1_ "setLock" (c_fcntl_flock fd (#const F_SETLK) p_flock)

waitToSetLock :: Fd -> FileLock -> IO ()
waitToSetLock fd lock = do
  allocaLock lock $ \p_flock ->
    throwErrnoIfMinus1_ "waitToSetLock" 
	(c_fcntl_flock fd (#const F_SETLKW) p_flock)

-- -----------------------------------------------------------------------------
-- fd{Read,Write}

fdRead :: Fd -> ByteCount -> IO (String, ByteCount)
fdRead _fd 0 = return ("", 0)
fdRead fd  nbytes = do
    allocaBytes (fromIntegral nbytes) $ \ bytes -> do
    rc    <-  throwErrnoIfMinus1Retry "fdRead" (c_read fd bytes nbytes)
    case rc of
      0  -> ioException (IOError Nothing EOF "fdRead" "EOF" Nothing)
      n | n == nbytes -> do
            s <- peekCStringLen (bytes, fromIntegral n)
	    return (s, n)
        | otherwise -> do
	    -- Let go of the excessively long ByteArray# by copying to a
	    -- shorter one.  Maybe we need a new primitive, shrinkCharArray#?
            allocaBytes (fromIntegral n) $ \ bytes' -> do
            c_memcpy bytes' bytes n
            s <- peekCStringLen (bytes', fromIntegral n)
	    return (s, n)

fdWrite :: Fd -> String -> IO ByteCount
fdWrite fd str = withCStringLen str $ \ (strPtr,len) -> do
    throwErrnoIfMinus1Retry "fdWrite" (c_write fd strPtr (fromIntegral len))

foreign import ccall unsafe "read"
  c_read :: Fd -> CString -> CSize -> IO CSize

foreign import ccall unsafe "write"
  c_write :: Fd -> CString -> CSize -> IO CSize

foreign import ccall unsafe "memcpy"
  c_memcpy :: Ptr dst -> Ptr src -> CSize -> IO (Ptr dst)