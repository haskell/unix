{-# OPTIONS -fffi #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Unistd
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX miscellaneous stuff, mostly from unistd.h
--
-----------------------------------------------------------------------------

module System.Posix.Unistd (
    -- * User environment
    -- * Querying user environment
    getRealUserID,
    getRealGroupID,
    getEffectiveUserID,
    getEffectiveGroupID,
#if !defined(cygwin32_TARGET_OS)
    getGroups,
#endif
    getLoginName,

    -- * Modifying the user environment
    setUserID,
    setGroupID,

    -- * Process environment
    -- ** Querying the process environment
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
    ProcessTimes(elapsedTime, systemTime, userTime,
		 childSystemTime, childUserTime),
    getProcessTimes,
	
    -- * System environment
    SystemID(..),
    getSystemID,

  {-
    ToDo from unistd.h:
      confstr
      dup, dup2, exec*, exit,  fork, fpathconf, fsync, ftruncate,
      gethostid, gethostname, getlogin, getopt, isatty, lockf,
      lseek, nice, pathconf, pipe, pread, pwrite, read, readlink,
      sleep, symlink, sysconf, tcgetpgrp, tcsetpgrp, truncate,
      ttyname(_r), ualarm, usleep, vfork, write

    SysVar(..),
    getSysVar,

    -- should be in System.Posix.Files?
    queryTerminal,
    getTerminalName,
#if !defined(cygwin32_TARGET_OS)
    getControllingTerminalName,
#endif

    -- should be in System.Posix.Time?
    epochTime,

    -- should be in System.Posix.Pwd?
    getEffectiveUserName,
-}
  ) where

import Foreign
import Foreign.C
import System.Posix.Types
import GHC.Posix

#include <unistd.h>
#include <sys/times.h>
#include <sys/utsname.h>

-- -----------------------------------------------------------------------------
-- user environemnt

getRealUserID :: IO UserID
getRealUserID = c_getuid

foreign import ccall unsafe "getuid"
  c_getuid :: IO CUid

getRealGroupID :: IO GroupID
getRealGroupID = c_getgid

foreign import ccall unsafe "getgid"
  c_getgid :: IO CGid

getEffectiveUserID :: IO UserID
getEffectiveUserID = c_geteuid

foreign import ccall unsafe "geteuid"
  c_geteuid :: IO CUid

getEffectiveGroupID :: IO GroupID
getEffectiveGroupID = c_getegid

foreign import ccall unsafe "getegid"
  c_getegid :: IO CGid

-- getgroups() is not supported in beta18 of
-- cygwin32
#if !defined(cygwin32_TARGET_OS)
getGroups :: IO [GroupID]
getGroups = do
    ngroups <- c_getgroups 0 nullPtr
    allocaArray (fromIntegral ngroups) $ \arr -> do
       throwErrnoIfMinus1_ "getGroups" (c_getgroups ngroups arr)
       groups <- peekArray (fromIntegral ngroups) arr
       return groups

foreign import ccall unsafe "getgroups"
  c_getgroups :: CInt -> Ptr CGid -> IO CInt
#endif

-- ToDo: use getlogin_r
getLoginName :: IO String
getLoginName =  do
    str <- throwErrnoIfNull "getLoginName" c_getlogin
    peekCString str

foreign import ccall unsafe "getlogin"
  c_getlogin :: IO CString

setUserID :: UserID -> IO ()
setUserID uid = throwErrnoIfMinus1_ "setUserID" (c_setuid uid)

foreign import ccall unsafe "setuid"
  c_setuid :: CUid -> IO CInt

setGroupID :: GroupID -> IO ()
setGroupID gid = throwErrnoIfMinus1_ "setGroupID" (c_setgid gid)

foreign import ccall unsafe "setgid"
  c_setgid :: CGid -> IO CInt

-- -----------------------------------------------------------------------------
-- Process environment

getProcessID :: IO ProcessID
getProcessID = c_getpid

getParentProcessID :: IO ProcessID
getParentProcessID = c_getppid

foreign import ccall unsafe "getppid"
  c_getppid :: IO CPid

getProcessGroupID :: IO ProcessGroupID
getProcessGroupID = c_getpgrp

foreign import ccall unsafe "getpgrp"
  c_getpgrp :: IO CPid

createProcessGroup :: ProcessID -> IO ProcessGroupID
createProcessGroup pid = do
  throwErrnoIfMinus1_ "createProcessGroup" (c_setpgid pid 0)
  return pid

joinProcessGroup :: ProcessGroupID -> IO ()
joinProcessGroup pgid =
  throwErrnoIfMinus1_ "joinProcessGroup" (c_setpgid 0 pgid)

setProcessGroupID :: ProcessID -> ProcessGroupID -> IO ()
setProcessGroupID pid pgid =
  throwErrnoIfMinus1_ "setProcessGroupID" (c_setpgid pid pgid)

foreign import ccall unsafe "setpgid"
  c_setpgid :: CPid -> CPid -> IO CInt

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

foreign import ccall unsafe "times"
  c_times :: Ptr CTms -> IO CClock

-- -----------------------------------------------------------------------------
-- System environment (uname())

data SystemID =
  SystemID { systemName :: String
  	   , nodeName   :: String
	   , release    :: String
	   , version    :: String
	   , machine    :: String
	   }

getSystemID :: IO SystemID
getSystemID = do
  allocaBytes (#const sizeof(struct utsname)) $ \p_sid -> do
    throwErrnoIfMinus1_ "getSystemID" (c_uname p_sid)
    sysN <- peekCString ((#ptr struct utsname, sysname) p_sid)
    node <- peekCString ((#ptr struct utsname, nodename) p_sid)
    rel  <- peekCString ((#ptr struct utsname, release) p_sid)
    ver  <- peekCString ((#ptr struct utsname, version) p_sid)
    mach <- peekCString ((#ptr struct utsname, machine) p_sid)
    return (SystemID { systemName = sysN,
		       nodeName   = node,
		       release    = rel,
		       version    = ver,
		       machine    = mach
		     })

{-	
data SysVar = ArgumentLimit
            | ChildLimit
            | ClockTick
            | GroupLimit
            | OpenFileLimit
            | PosixVersion
            | HasSavedIDs
            | HasJobControl

getSysVar :: SysVar -> IO Limit
getSysVar v =
    case v of
      ArgumentLimit -> sysconf ``_SC_ARG_MAX''
      ChildLimit    -> sysconf ``_SC_CHILD_MAX''
      ClockTick	    -> sysconf ``_SC_CLK_TCK''
      GroupLimit    -> sysconf ``_SC_NGROUPS_MAX''
      OpenFileLimit -> sysconf ``_SC_OPEN_MAX''
      PosixVersion  -> sysconf ``_SC_VERSION''
      HasSavedIDs   -> sysconf ``_SC_SAVED_IDS''
      HasJobControl -> sysconf ``_SC_JOB_CONTROL''
--  where

sysconf :: Int -> IO Limit
sysconf n = do
 rc <- _ccall_ sysconf n
 if rc /= (-1::Int)
    then return rc
    else ioException (IOError Nothing NoSuchThing
		          "getSysVar" 
		          "no such system limit or option"
			  Nothing)
-}
