{-# OPTIONS -fffi #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Process
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX process support
--
-----------------------------------------------------------------------------

module System.Posix.Process (
    -- * Processes

    -- ** Forking and executing
#ifdef __GLASGOW_HASKELL__
    forkProcess,
#endif
    executeFile,
    
    -- ** Exiting
    exitImmediately,

    -- ** Process environment
    getProcessID,
    getParentProcessID,
    getProcessGroupID,

    -- ** Process groups
    createProcessGroup,
    joinProcessGroup,
    setProcessGroupID,

    -- ** Sessions
    createSession,

    -- ** Process times
    ProcessTimes(..),
    getProcessTimes,

    -- ** Scheduling priority
    nice,
    getProcessPriority,
    getProcessGroupPriority,
    getUserPriority,
    setProcessPriority,
    setProcessGroupPriority,
    setUserPriority,

    -- ** Process status
    ProcessStatus(..),
    getProcessStatus,
    getAnyProcessStatus,
    getGroupProcessStatus,

 ) where

#include "HsUnix.h"

import Foreign.C.Error
import Foreign.C.String ( CString, withCString )
import Foreign.C.Types ( CInt, CClock )
import Foreign.Marshal.Alloc ( alloca, allocaBytes )
import Foreign.Marshal.Array ( withArray0 )
import Foreign.Marshal.Utils ( withMany )
import Foreign.Ptr ( Ptr, nullPtr )
import Foreign.StablePtr ( StablePtr, newStablePtr, freeStablePtr )
import Foreign.Storable ( Storable(..) )
import System.IO
import System.IO.Error
import System.Exit
import System.Posix.Error
import System.Posix.Types
import System.Posix.Signals
import System.Process.Internals ( pPrPr_disableITimers, c_execvpe )
import Control.Monad

#ifdef __GLASGOW_HASKELL__
import GHC.TopHandler	( runIO )
#endif

#ifdef __HUGS__
{-# CFILES cbits/HsUnix.c  #-}
#endif

-- -----------------------------------------------------------------------------
-- Process environment

-- | 'getProcessID' calls @getpid@ to obtain the 'ProcessID' for
--   the current process.
getProcessID :: IO ProcessID
getProcessID = c_getpid

foreign import ccall unsafe "getpid"
   c_getpid :: IO CPid

-- | 'getProcessID' calls @getppid@ to obtain the 'ProcessID' for
--   the parent of the current process.
getParentProcessID :: IO ProcessID
getParentProcessID = c_getppid

foreign import ccall unsafe "getppid"
  c_getppid :: IO CPid

-- | 'getProcessGroupID' calls @getpgrp@ to obtain the
--   'ProcessGroupID' for the current process.
getProcessGroupID :: IO ProcessGroupID
getProcessGroupID = c_getpgrp

foreign import ccall unsafe "getpgrp"
  c_getpgrp :: IO CPid

-- | @'createProcessGroup' pid@ calls @setpgid@ to make
--   process @pid@ a new process group leader.
createProcessGroup :: ProcessID -> IO ProcessGroupID
createProcessGroup pid = do
  throwErrnoIfMinus1_ "createProcessGroup" (c_setpgid pid 0)
  return pid

-- | @'joinProcessGroup' pgid@ calls @setpgid@ to set the
--   'ProcessGroupID' of the current process to @pgid@.
joinProcessGroup :: ProcessGroupID -> IO ()
joinProcessGroup pgid =
  throwErrnoIfMinus1_ "joinProcessGroup" (c_setpgid 0 pgid)

-- | @'setProcessGroupID' pid pgid@ calls @setpgid@ to set the
--   'ProcessGroupID' for process @pid@ to @pgid@.
setProcessGroupID :: ProcessID -> ProcessGroupID -> IO ()
setProcessGroupID pid pgid =
  throwErrnoIfMinus1_ "setProcessGroupID" (c_setpgid pid pgid)

foreign import ccall unsafe "setpgid"
  c_setpgid :: CPid -> CPid -> IO CInt

-- | 'createSession' calls @setsid@ to create a new session
--   with the current process as session leader.
createSession :: IO ProcessGroupID
createSession = throwErrnoIfMinus1 "createSession" c_setsid

foreign import ccall unsafe "setsid"
  c_setsid :: IO CPid

-- -----------------------------------------------------------------------------
-- Process times

-- All times in clock ticks (see getClockTick)

data ProcessTimes
  = ProcessTimes { elapsedTime     :: ClockTick
  		 , userTime        :: ClockTick
		 , systemTime      :: ClockTick
		 , childUserTime   :: ClockTick
		 , childSystemTime :: ClockTick
		 }

-- | 'getProcessTimes' calls @times@ to obtain time-accounting
--   information for the current process and its children.
getProcessTimes :: IO ProcessTimes
getProcessTimes = do
   allocaBytes (#const sizeof(struct tms)) $ \p_tms -> do
     elapsed <- throwErrnoIfMinus1 "getProcessTimes" (c_times p_tms)
     ut  <- (#peek struct tms, tms_utime)  p_tms
     st  <- (#peek struct tms, tms_stime)  p_tms
     cut <- (#peek struct tms, tms_cutime) p_tms
     cst <- (#peek struct tms, tms_cstime) p_tms
     return (ProcessTimes{ elapsedTime     = elapsed,
	 		   userTime        = ut,
	 		   systemTime      = st,
	 		   childUserTime   = cut,
	 		   childSystemTime = cst
			  })

type CTms = ()

foreign import ccall unsafe "times"
  c_times :: Ptr CTms -> IO CClock

-- -----------------------------------------------------------------------------
-- Process scheduling priority

nice :: Int -> IO ()
nice prio = do
  resetErrno
  res <- c_nice (fromIntegral prio)
  when (res == -1) $ do
    err <- getErrno
    when (err /= eOK) (throwErrno "nice")

foreign import ccall unsafe "nice"
  c_nice :: CInt -> IO CInt

getProcessPriority      :: ProcessID      -> IO Int
getProcessGroupPriority :: ProcessGroupID -> IO Int
getUserPriority         :: UserID         -> IO Int

getProcessPriority pid = do
  r <- throwErrnoIfMinus1 "getProcessPriority" $
         c_getpriority (#const PRIO_PROCESS) (fromIntegral pid)
  return (fromIntegral r)

getProcessGroupPriority pid = do
  r <- throwErrnoIfMinus1 "getProcessPriority" $
         c_getpriority (#const PRIO_PGRP) (fromIntegral pid)
  return (fromIntegral r)

getUserPriority uid = do
  r <- throwErrnoIfMinus1 "getUserPriority" $
         c_getpriority (#const PRIO_USER) (fromIntegral uid)
  return (fromIntegral r)

foreign import ccall unsafe "getpriority"
  c_getpriority :: CInt -> CInt -> IO CInt

setProcessPriority      :: ProcessID      -> Int -> IO ()
setProcessGroupPriority :: ProcessGroupID -> Int -> IO ()
setUserPriority         :: UserID         -> Int -> IO ()

setProcessPriority pid val = 
  throwErrnoIfMinus1_ "setProcessPriority" $
    c_setpriority (#const PRIO_PROCESS) (fromIntegral pid) (fromIntegral val)

setProcessGroupPriority pid val =
  throwErrnoIfMinus1_ "setProcessPriority" $
    c_setpriority (#const PRIO_PGRP) (fromIntegral pid) (fromIntegral val)

setUserPriority uid val =
  throwErrnoIfMinus1_ "setUserPriority" $
    c_setpriority (#const PRIO_USER) (fromIntegral uid) (fromIntegral val)

foreign import ccall unsafe "setpriority"
  c_setpriority :: CInt -> CInt -> CInt -> IO CInt

-- -----------------------------------------------------------------------------
-- Forking, execution

#ifdef __GLASGOW_HASKELL__
{- | 'forkProcess' corresponds to the POSIX @fork@ system call.
The 'IO' action passed as an argument is executed in the child process; no other
threads will be copied to the child process.
On success, 'forkProcess' returns the child's 'ProcessID' to the parent process;
in case of an error, an exception is thrown.
-}

forkProcess :: IO () -> IO ProcessID
forkProcess action = do
  stable <- newStablePtr (runIO action)
  pid <- throwErrnoIfMinus1 "forkProcess" (forkProcessPrim stable)
  freeStablePtr stable
  return $ fromIntegral pid

foreign import ccall "forkProcess" forkProcessPrim :: StablePtr (IO ()) -> IO CPid
#endif /* __GLASGOW_HASKELL__ */

-- | @'executeFile' cmd args env@ calls one of the
--   @execv*@ family, depending on whether or not the current
--   PATH is to be searched for the command, and whether or not an
--   environment is provided to supersede the process's current
--   environment.  The basename (leading directory names suppressed) of
--   the command is passed to @execv*@ as @arg[0]@;
--   the argument list passed to 'executeFile' therefore 
--   begins with @arg[1]@.
executeFile :: FilePath			    -- ^ Command
            -> Bool			    -- ^ Search PATH?
            -> [String]			    -- ^ Arguments
            -> Maybe [(String, String)]	    -- ^ Environment
            -> IO ()
executeFile path search args Nothing = do
  withCString path $ \s ->
    withMany withCString (path:args) $ \cstrs ->
      withArray0 nullPtr cstrs $ \arr -> do
	pPrPr_disableITimers
	if search 
	   then throwErrnoPathIfMinus1_ "executeFile" path (c_execvp s arr)
	   else throwErrnoPathIfMinus1_ "executeFile" path (c_execv s arr)

executeFile path search args (Just env) = do
  withCString path $ \s ->
    withMany withCString (path:args) $ \cstrs ->
      withArray0 nullPtr cstrs $ \arg_arr ->
    let env' = map (\ (name, val) -> name ++ ('=' : val)) env in
    withMany withCString env' $ \cenv ->
      withArray0 nullPtr cenv $ \env_arr -> do
	pPrPr_disableITimers
	if search 
	   then throwErrnoPathIfMinus1_ "executeFile" path
		   (c_execvpe s arg_arr env_arr)
	   else throwErrnoPathIfMinus1_ "executeFile" path
		   (c_execve s arg_arr env_arr)

foreign import ccall unsafe "execvp"
  c_execvp :: CString -> Ptr CString -> IO CInt

foreign import ccall unsafe "execv"
  c_execv :: CString -> Ptr CString -> IO CInt

foreign import ccall unsafe "execve"
  c_execve :: CString -> Ptr CString -> Ptr CString -> IO CInt

-- -----------------------------------------------------------------------------
-- Waiting for process termination

data ProcessStatus = Exited ExitCode
                   | Terminated Signal
                   | Stopped Signal
		   deriving (Eq, Ord, Show)

-- | @'getProcessStatus' blk stopped pid@ calls @waitpid@, returning
--   @'Just' tc@, the 'ProcessStatus' for process @pid@ if it is
--   available, 'Nothing' otherwise.  If @blk@ is 'False', then
--   @WNOHANG@ is set in the options for @waitpid@, otherwise not.
--   If @stopped@ is 'True', then @WUNTRACED@ is set in the
--   options for @waitpid@, otherwise not.
getProcessStatus :: Bool -> Bool -> ProcessID -> IO (Maybe ProcessStatus)
getProcessStatus block stopped pid =
  alloca $ \wstatp -> do
    pid <- throwErrnoIfMinus1Retry "getProcessStatus"
		(c_waitpid pid wstatp (waitOptions block stopped))
    case pid of
      0  -> return Nothing
      _  -> do ps <- decipherWaitStatus wstatp
	       return (Just ps)

-- safe, because this call might block
foreign import ccall safe "waitpid"
  c_waitpid :: CPid -> Ptr CInt -> CInt -> IO CPid

-- | @'getGroupProcessStatus' blk stopped pgid@ calls @waitpid@,
--   returning @'Just' (pid, tc)@, the 'ProcessID' and
--   'ProcessStatus' for any process in group @pgid@ if one is
--   available, 'Nothing' otherwise.  If @blk@ is 'False', then
--   @WNOHANG@ is set in the options for @waitpid@, otherwise not.
--   If @stopped@ is 'True', then @WUNTRACED@ is set in the
--   options for @waitpid@, otherwise not.
getGroupProcessStatus :: Bool
                      -> Bool
                      -> ProcessGroupID
                      -> IO (Maybe (ProcessID, ProcessStatus))
getGroupProcessStatus block stopped pgid =
  alloca $ \wstatp -> do
    pid <- throwErrnoIfMinus1Retry "getGroupProcessStatus"
		(c_waitpid (-pgid) wstatp (waitOptions block stopped))
    case pid of
      0  -> return Nothing
      _  -> do ps <- decipherWaitStatus wstatp
	       return (Just (pid, ps))
-- | @'getAnyProcessStatus' blk stopped@ calls @waitpid@, returning
--   @'Just' (pid, tc)@, the 'ProcessID' and 'ProcessStatus' for any
--   child process if one is available, 'Nothing' otherwise.  If
--   @blk@ is 'False', then @WNOHANG@ is set in the options for
--   @waitpid@, otherwise not.  If @stopped@ is 'True', then
--   @WUNTRACED@ is set in the options for @waitpid@, otherwise not.
getAnyProcessStatus :: Bool -> Bool -> IO (Maybe (ProcessID, ProcessStatus))
getAnyProcessStatus block stopped = getGroupProcessStatus block stopped 1

waitOptions :: Bool -> Bool -> CInt
--             block   stopped
waitOptions False False = (#const WNOHANG)
waitOptions False True  = (#const (WNOHANG|WUNTRACED))
waitOptions True  False = 0
waitOptions True  True  = (#const WUNTRACED)

-- Turn a (ptr to a) wait status into a ProcessStatus

decipherWaitStatus :: Ptr CInt -> IO ProcessStatus
decipherWaitStatus wstatp = do
  wstat <- peek wstatp
  if c_WIFEXITED wstat /= 0
      then do
        let exitstatus = c_WEXITSTATUS wstat
        if exitstatus == 0
	   then return (Exited ExitSuccess)
	   else return (Exited (ExitFailure (fromIntegral exitstatus)))
      else do
        if c_WIFSIGNALED wstat /= 0
	   then do
		let termsig = c_WTERMSIG wstat
		return (Terminated (fromIntegral termsig))
	   else do
		if c_WIFSTOPPED wstat /= 0
		   then do
			let stopsig = c_WSTOPSIG wstat
			return (Stopped (fromIntegral stopsig))
		   else do
			ioError (mkIOError illegalOperationErrorType
				   "waitStatus" Nothing Nothing)

foreign import ccall unsafe "__hsunix_wifexited"
  c_WIFEXITED :: CInt -> CInt 

foreign import ccall unsafe "__hsunix_wexitstatus"
  c_WEXITSTATUS :: CInt -> CInt

foreign import ccall unsafe "__hsunix_wifsignaled"
  c_WIFSIGNALED :: CInt -> CInt

foreign import ccall unsafe "__hsunix_wtermsig"
  c_WTERMSIG :: CInt -> CInt 

foreign import ccall unsafe "__hsunix_wifstopped"
  c_WIFSTOPPED :: CInt -> CInt

foreign import ccall unsafe "__hsunix_wstopsig"
  c_WSTOPSIG :: CInt -> CInt

-- -----------------------------------------------------------------------------
-- Exiting

-- | @'exitImmediately' status@ calls @_exit@ to terminate the process
--   with the indicated exit @status@.
--   The operation never returns.
exitImmediately :: ExitCode -> IO ()
exitImmediately exitcode = c_exit (exitcode2Int exitcode)
  where
    exitcode2Int ExitSuccess = 0
    exitcode2Int (ExitFailure n) = fromIntegral n

foreign import ccall unsafe "exit"
  c_exit :: CInt -> IO ()

-- -----------------------------------------------------------------------------
