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

    -- * System environment
    SystemID(..),
    getSystemID,

    SysVar(..),
    getSysVar,

    -- * Sleeping
    sleep, usleep,

  {-
    ToDo from unistd.h:
      confstr, 
      lots of sysconf variables

    -- use Network.BSD
    gethostid, gethostname

    -- should be in System.Posix.Files?
    pathconf, fpathconf,
    queryTerminal,
    getTerminalName,
#if !defined(cygwin32_TARGET_OS)
    getControllingTerminalName,
#endif

    -- System.Posix.Signals
    ualarm,

    -- System.Posix.Terminal
    isatty, tcgetpgrp, tcsetpgrp, ttyname(_r),

    -- System.Posix.IO
    read, write,

    -- should be in System.Posix.Time?
    epochTime,

    -- should be in System.Posix.User?
    getEffectiveUserName,
-}
  ) where

#include "config.h"

import Foreign
import Foreign.C
import System.Posix.Types
import GHC.Posix

#include <unistd.h>
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

-- -----------------------------------------------------------------------------
-- sleeping

sleep :: Int -> IO Int
sleep 0 = return 0
sleep secs = do r <- c_sleep (fromIntegral secs); return (fromIntegral r)

foreign import ccall unsafe "sleep"
  c_sleep :: CUInt -> IO CUInt

usleep :: Int -> IO ()
usleep 0 = return ()
#ifdef USLEEP_RETURNS_VOID
usleep usecs = c_usleep (fromIntegral usecs)
#else
usleep usecs = throwErrnoIfMinus1_ "usleep" (c_usleep (fromIntegral usecs))
#endif

#ifdef USLEEP_RETURNS_VOID
foreign import ccall unsafe "usleep"
  c_usleep :: CUInt -> IO ()
#else
foreign import ccall unsafe "usleep"
  c_usleep :: CUInt -> IO CInt
#endif

-- -----------------------------------------------------------------------------
-- System variables

data SysVar = ArgumentLimit
            | ChildLimit
            | ClockTick
            | GroupLimit
            | OpenFileLimit
            | PosixVersion
            | HasSavedIDs
            | HasJobControl
	-- ToDo: lots more

getSysVar :: SysVar -> IO Integer
getSysVar v =
    case v of
      ArgumentLimit -> sysconf (#const _SC_ARG_MAX)
      ChildLimit    -> sysconf (#const _SC_CHILD_MAX)
      ClockTick	    -> sysconf (#const _SC_CLK_TCK)
      GroupLimit    -> sysconf (#const _SC_NGROUPS_MAX)
      OpenFileLimit -> sysconf (#const _SC_OPEN_MAX)
      PosixVersion  -> sysconf (#const _SC_VERSION)
      HasSavedIDs   -> sysconf (#const _SC_SAVED_IDS)
      HasJobControl -> sysconf (#const _SC_JOB_CONTROL)

sysconf :: CInt -> IO Integer
sysconf n = do 
  r <- throwErrnoIfMinus1 "getSysVar" (c_sysconf n)
  return (fromIntegral r)

foreign import ccall unsafe "sysconf"
  c_sysconf :: CInt -> IO CLong
