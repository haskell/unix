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

    -- System.Posix.Signals
    ualarm,

    -- System.Posix.IO
    read, write,

    -- should be in System.Posix.User?
    getEffectiveUserName,
-}
  ) where

#include "HsUnix.h"

import Foreign.C.Error ( throwErrnoIfMinus1, throwErrnoIfMinus1_ )
import Foreign.C.String ( peekCString )
import Foreign.C.Types ( CInt, CUInt, CLong )
import Foreign.Marshal.Alloc ( allocaBytes )
import Foreign.Ptr ( Ptr, plusPtr )
import System.Posix.Types
import System.Posix.Internals

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

foreign import ccall unsafe "uname"
   c_uname :: Ptr CUtsname -> IO CInt

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
