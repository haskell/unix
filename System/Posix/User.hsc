{-# OPTIONS -fffi #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.User
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX user\/group support
--
-----------------------------------------------------------------------------

module System.Posix.User (
    -- * User environment
    -- ** Querying the user environment
    getRealUserID,
    getRealGroupID,
    getEffectiveUserID,
    getEffectiveGroupID,
    getGroups,
    getLoginName,
    getEffectiveUserName,

    -- *** The group database
    GroupEntry(..),
    getGroupEntryForID,
    getGroupEntryForName,

    -- *** The user database
    UserEntry(..),
    getUserEntryForID,
    getUserEntryForName,

    -- ** Modifying the user environment
    setUserID,
    setGroupID,

  ) where

#include "HsUnix.h"

#ifdef solaris_TARGET_OS
-- Solaris needs this in order to get the POSIX versions of getgrnam_r etc.
#define _POSIX_PTHREAD_SEMANTICS
#endif

import System.Posix.Types
import Foreign
import Foreign.C
import System.Posix.Internals	( CGroup, CPasswd )

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

getGroups :: IO [GroupID]
getGroups = do
    ngroups <- c_getgroups 0 nullPtr
    allocaArray (fromIntegral ngroups) $ \arr -> do
       throwErrnoIfMinus1_ "getGroups" (c_getgroups ngroups arr)
       groups <- peekArray (fromIntegral ngroups) arr
       return groups

foreign import ccall unsafe "getgroups"
  c_getgroups :: CInt -> Ptr CGid -> IO CInt

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
-- User names

getEffectiveUserName :: IO String
getEffectiveUserName = do
    euid <- getEffectiveUserID
    pw <- getUserEntryForID euid
    return (userName pw)

-- -----------------------------------------------------------------------------
-- The group database (grp.h)

data GroupEntry =
 GroupEntry {
  groupName    :: String,
  groupID      :: GroupID,
  groupMembers :: [String]
 }

getGroupEntryForID :: GroupID -> IO GroupEntry
#ifdef HAVE_GETGRGID_R
getGroupEntryForID gid = do
  allocaBytes (#const sizeof(struct group)) $ \pgr ->
    allocaBytes grBufSize $ \pbuf ->
      alloca $ \ ppgr -> do
        throwErrorIfNonZero_ "getGroupEntryForID" $
	     c_getgrgid_r gid pgr pbuf (fromIntegral grBufSize) ppgr
	unpackGroupEntry pgr


foreign import ccall unsafe "getgrgid_r"
  c_getgrgid_r :: CGid -> Ptr CGroup -> CString
		 -> CSize -> Ptr (Ptr CGroup) -> IO CInt
#else
getGroupEntryForID = error "System.Posix.User.getGroupEntryForID: not supported"
#endif


getGroupEntryForName :: String -> IO GroupEntry
#ifdef HAVE_GETGRNAM_R
getGroupEntryForName name = do
  allocaBytes (#const sizeof(struct group)) $ \pgr ->
    allocaBytes grBufSize $ \pbuf ->
      alloca $ \ ppgr -> 
	withCString name $ \ pstr -> do
          throwErrorIfNonZero_ "getGroupEntryForName" $
	     c_getgrnam_r pstr pgr pbuf (fromIntegral grBufSize) ppgr
	  unpackGroupEntry pgr

foreign import ccall unsafe "getgrnam_r"
  c_getgrnam_r :: CString -> Ptr CGroup -> CString
		 -> CSize -> Ptr (Ptr CGroup) -> IO CInt
#else
getGroupEntryForName = error "System.Posix.User.getGroupEntryForName: not supported"
#endif

#if defined(HAVE_GETGRGID_R) || defined(HAVE_GETGRNAM_R)
grBufSize :: Int
#if defined(HAVE_SYSCONF) && defined(HAVE_SC_GETGR_R_SIZE_MAX)
grBufSize = fromIntegral $ unsafePerformIO $
		c_sysconf (#const _SC_GETGR_R_SIZE_MAX)
#else
grBufSize = 1024	-- just assume some value
#endif
#endif

unpackGroupEntry :: Ptr CGroup -> IO GroupEntry
unpackGroupEntry ptr = do
   name    <- (#peek struct group, gr_name) ptr >>= peekCString
   gid     <- (#peek struct group, gr_gid) ptr
   mem     <- (#peek struct group, gr_mem) ptr
   members <- peekArray0 nullPtr mem >>= mapM peekCString
   return (GroupEntry name gid members)

-- -----------------------------------------------------------------------------
-- The user database (pwd.h)

data UserEntry =
 UserEntry {
   userName      :: String,
   userID        :: UserID,
   userGroupID   :: GroupID,
   homeDirectory :: String,
   userShell     :: String
 }

getUserEntryForID :: UserID -> IO UserEntry
#ifdef HAVE_GETPWUID_R
getUserEntryForID uid = do
  allocaBytes (#const sizeof(struct passwd)) $ \ppw ->
    allocaBytes pwBufSize $ \pbuf ->
      alloca $ \ pppw -> do
        throwErrorIfNonZero_ "getUserEntryForID" $
	     c_getpwuid_r uid ppw pbuf (fromIntegral pwBufSize) pppw
	unpackUserEntry ppw

foreign import ccall unsafe "getpwuid_r"
  c_getpwuid_r :: CUid -> Ptr CPasswd -> 
			CString -> CSize -> Ptr (Ptr CPasswd) -> IO CInt
#else
getUserEntryForID = error "System.Posix.User.getUserEntryForID: not supported"
#endif

getUserEntryForName :: String -> IO UserEntry
#if HAVE_GETPWNAM_R
getUserEntryForName name = do
  allocaBytes (#const sizeof(struct passwd)) $ \ppw ->
    allocaBytes pwBufSize $ \pbuf ->
      alloca $ \ pppw -> 
	withCString name $ \ pstr -> do
          throwErrorIfNonZero_ "getUserEntryForName" $
	       c_getpwnam_r pstr ppw pbuf (fromIntegral pwBufSize) pppw
	  unpackUserEntry ppw

foreign import ccall unsafe "getpwnam_r"
  c_getpwnam_r :: CString -> Ptr CPasswd -> 
			CString -> CSize -> Ptr (Ptr CPasswd) -> IO CInt
#else
getUserEntryForName = error "System.Posix.User.getUserEntryForName: not supported"
#endif

#if defined(HAVE_GETPWUID_R) || defined(HAVE_GETPWNAM_R)
pwBufSize :: Int
#if  defined(HAVE_SYSCONF) && defined(HAVE_SC_GETPW_R_SIZE_MAX)
pwBufSize = fromIntegral $ unsafePerformIO $
		c_sysconf (#const _SC_GETPW_R_SIZE_MAX)
#else
pwBufSize = 1024
#endif
#endif

#ifdef HAVE_SYSCONF
foreign import ccall unsafe "sysconf"
  c_sysconf :: CInt -> IO CLong
#endif

unpackUserEntry :: Ptr CPasswd -> IO UserEntry
unpackUserEntry ptr = do
   name   <- (#peek struct passwd, pw_name)  ptr >>= peekCString
   uid    <- (#peek struct passwd, pw_uid)   ptr
   gid    <- (#peek struct passwd, pw_gid)   ptr
   dir    <- (#peek struct passwd, pw_dir)   ptr >>= peekCString
   shell  <- (#peek struct passwd, pw_shell) ptr >>= peekCString
   return (UserEntry name uid gid dir shell)

-- Used when calling re-entrant system calls that signal their 'errno' 
-- directly through the return value.
throwErrorIfNonZero_ :: String -> IO CInt -> IO ()
throwErrorIfNonZero_ loc act = do
    rc <- act
    if (rc == 0) 
     then return ()
     else ioError (errnoToIOError loc (Errno (fromIntegral rc)) Nothing Nothing)

