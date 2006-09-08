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
    getAllGroupEntries,

    -- *** The user database
    UserEntry(..),
    getUserEntryForID,
    getUserEntryForName,
    getAllUserEntries,

    -- ** Modifying the user environment
    setUserID,
    setGroupID,

  ) where

#include "HsUnix.h"

import System.Posix.Types
import Foreign
import Foreign.C
import System.Posix.Internals	( CGroup, CPasswd )

#if !defined(HAVE_GETPWNAM_R) || !defined(HAVE_GETPWUID_R) || defined(HAVE_GETPWENT) || defined(HAVE_GETGRENT)
import Control.Concurrent.MVar  ( newMVar, withMVar )
#endif

-- -----------------------------------------------------------------------------
-- user environemnt

-- | @getRealUserID@ calls @getuid@ to obtain the real @UserID@
--   associated with the current process.
getRealUserID :: IO UserID
getRealUserID = c_getuid

foreign import ccall unsafe "getuid"
  c_getuid :: IO CUid

-- | @getRealGroupID@ calls @getgid@ to obtain the real @GroupID@
--   associated with the current process.
getRealGroupID :: IO GroupID
getRealGroupID = c_getgid

foreign import ccall unsafe "getgid"
  c_getgid :: IO CGid

-- | @getEffectiveUserID@ calls @geteuid@ to obtain the effective
--   @UserID@ associated with the current process.
getEffectiveUserID :: IO UserID
getEffectiveUserID = c_geteuid

foreign import ccall unsafe "geteuid"
  c_geteuid :: IO CUid

-- | @getEffectiveGroupID@ calls @getegid@ to obtain the effective
--   @GroupID@ associated with the current process.
getEffectiveGroupID :: IO GroupID
getEffectiveGroupID = c_getegid

foreign import ccall unsafe "getegid"
  c_getegid :: IO CGid

-- | @getGroups@ calls @getgroups@ to obtain the list of
--   supplementary @GroupID@s associated with the current process.
getGroups :: IO [GroupID]
getGroups = do
    ngroups <- c_getgroups 0 nullPtr
    allocaArray (fromIntegral ngroups) $ \arr -> do
       throwErrnoIfMinus1_ "getGroups" (c_getgroups ngroups arr)
       groups <- peekArray (fromIntegral ngroups) arr
       return groups

foreign import ccall unsafe "getgroups"
  c_getgroups :: CInt -> Ptr CGid -> IO CInt




-- | @getLoginName@ calls @getlogin@ to obtain the login name
--   associated with the current process.
getLoginName :: IO String
getLoginName =  do
    -- ToDo: use getlogin_r
    str <- throwErrnoIfNull "getLoginName" c_getlogin
    peekCString str

foreign import ccall unsafe "getlogin"
  c_getlogin :: IO CString

-- | @setUserID uid@ calls @setuid@ to set the real, effective, and
--   saved set-user-id associated with the current process to @uid@.
setUserID :: UserID -> IO ()
setUserID uid = throwErrnoIfMinus1_ "setUserID" (c_setuid uid)

foreign import ccall unsafe "setuid"
  c_setuid :: CUid -> IO CInt

-- | @setGroupID gid@ calls @setgid@ to set the real, effective, and
--   saved set-group-id associated with the current process to @gid@.
setGroupID :: GroupID -> IO ()
setGroupID gid = throwErrnoIfMinus1_ "setGroupID" (c_setgid gid)

foreign import ccall unsafe "setgid"
  c_setgid :: CGid -> IO CInt

-- -----------------------------------------------------------------------------
-- User names

-- | @getEffectiveUserName@ gets the name
--   associated with the effective @UserID@ of the process.
getEffectiveUserName :: IO String
getEffectiveUserName = do
    euid <- getEffectiveUserID
    pw <- getUserEntryForID euid
    return (userName pw)

-- -----------------------------------------------------------------------------
-- The group database (grp.h)

data GroupEntry =
 GroupEntry {
  groupName    :: String,       -- ^ The name of this group (gr_name)
  groupPassword :: String,      -- ^ The password for this group (gr_passwd)
  groupID      :: GroupID,      -- ^ The unique numeric ID for this group (gr_gid)
  groupMembers :: [String]      -- ^ A list of zero or more usernames that are members (gr_mem)
 } deriving (Show, Read, Eq)

-- | @getGroupEntryForID gid@ calls @getgrgid@ to obtain
--   the @GroupEntry@ information associated with @GroupID@
--   @gid@.
getGroupEntryForID :: GroupID -> IO GroupEntry
#ifdef HAVE_GETGRGID_R
getGroupEntryForID gid = do
  allocaBytes (#const sizeof(struct group)) $ \pgr ->
    allocaBytes grBufSize $ \pbuf ->
      alloca $ \ ppgr -> do
        throwErrorIfNonZero_ "getGroupEntryForID" $
	     c_getgrgid_r gid pgr pbuf (fromIntegral grBufSize) ppgr
	throwErrnoIfNull "getGroupEntryForID" $
	     peekElemOff ppgr 0
	unpackGroupEntry pgr


foreign import ccall unsafe "getgrgid_r"
  c_getgrgid_r :: CGid -> Ptr CGroup -> CString
		 -> CSize -> Ptr (Ptr CGroup) -> IO CInt
#else
getGroupEntryForID = error "System.Posix.User.getGroupEntryForID: not supported"
#endif

-- | @getGroupEntryForName name@ calls @getgrnam@ to obtain
--   the @GroupEntry@ information associated with the group called
--   @name@.
getGroupEntryForName :: String -> IO GroupEntry
#ifdef HAVE_GETGRNAM_R
getGroupEntryForName name = do
  allocaBytes (#const sizeof(struct group)) $ \pgr ->
    allocaBytes grBufSize $ \pbuf ->
      alloca $ \ ppgr -> 
	withCString name $ \ pstr -> do
          throwErrorIfNonZero_ "getGroupEntryForName" $
	     c_getgrnam_r pstr pgr pbuf (fromIntegral grBufSize) ppgr
	  throwErrnoIfNull "getGroupEntryForName" $
	     peekElemOff ppgr 0
	  unpackGroupEntry pgr

foreign import ccall unsafe "getgrnam_r"
  c_getgrnam_r :: CString -> Ptr CGroup -> CString
		 -> CSize -> Ptr (Ptr CGroup) -> IO CInt
#else
getGroupEntryForName = error "System.Posix.User.getGroupEntryForName: not supported"
#endif

-- | @getAllGroupEntries@ returns all group entries on the system by
--   repeatedly calling @getgrent@
getAllGroupEntries :: IO [GroupEntry]
#ifdef HAVE_GETGRENT
getAllGroupEntries =
    withMVar lock $ \_ -> worker []
    where worker accum =
              do resetErrno
                 ppw <- throwErrnoIfNullAndError "getAllGroupEntries" $ 
                        c_getgrent
                 if ppw == nullPtr
                     then return (reverse accum)
                     else do thisentry <- unpackGroupEntry ppw
                             worker (thisentry : accum)

foreign import ccall unsafe "getgrent"
  c_getgrent :: IO (Ptr CGroup)
#else
getAllGroupEntries = error "System.Posix.User.getAllGroupEntries: not supported"
#endif

#if defined(HAVE_GETGRGID_R) || defined(HAVE_GETGRNAM_R)
grBufSize :: Int
#if defined(HAVE_SYSCONF) && defined(HAVE_SC_GETGR_R_SIZE_MAX)
grBufSize = fromIntegral $ unsafePerformIO $
		c_sysconf (#const _SC_GETGR_R_SIZE_MAX)
#else
grBufSize = 2048	-- just assume some value (1024 is too small on OpenBSD)
#endif
#endif

unpackGroupEntry :: Ptr CGroup -> IO GroupEntry
unpackGroupEntry ptr = do
   name    <- (#peek struct group, gr_name) ptr >>= peekCString
   passwd  <- (#peek struct group, gr_passwd) ptr >>= peekCString
   gid     <- (#peek struct group, gr_gid) ptr
   mem     <- (#peek struct group, gr_mem) ptr
   members <- peekArray0 nullPtr mem >>= mapM peekCString
   return (GroupEntry name passwd gid members)

-- -----------------------------------------------------------------------------
-- The user database (pwd.h)

data UserEntry =
 UserEntry {
   userName      :: String,     -- ^ Textual name of this user (pw_name)
   userPassword  :: String,     -- ^ Password -- may be empty or fake if shadow is in use (pw_passwd)
   userID        :: UserID,     -- ^ Numeric ID for this user (pw_uid)
   userGroupID   :: GroupID,    -- ^ Primary group ID (pw_gid)
   userGecos     :: String,     -- ^ Usually the real name for the user (pw_gecos)
   homeDirectory :: String,     -- ^ Home directory (pw_dir)
   userShell     :: String      -- ^ Default shell (pw_shell)
 } deriving (Show, Read, Eq)

--
-- getpwuid and getpwnam leave results in a static object. Subsequent
-- calls modify the same object, which isn't threadsafe. We attempt to
-- mitigate this issue, on platforms that don't provide the safe _r versions
--
-- Also, getpwent/setpwent require a global lock since they maintain
-- an internal file position pointer.
#if !defined(HAVE_GETPWNAM_R) || !defined(HAVE_GETPWUID_R) || defined(HAVE_GETPWENT) || defined(HAVE_GETGRENT)
lock = unsafePerformIO $ newMVar ()
{-# NOINLINE lock #-}
#endif

-- | @getUserEntryForID gid@ calls @getpwuid@ to obtain
--   the @UserEntry@ information associated with @UserID@
--   @uid@.
getUserEntryForID :: UserID -> IO UserEntry
#ifdef HAVE_GETPWUID_R
getUserEntryForID uid = do
  allocaBytes (#const sizeof(struct passwd)) $ \ppw ->
    allocaBytes pwBufSize $ \pbuf ->
      alloca $ \ pppw -> do
        throwErrorIfNonZero_ "getUserEntryForID" $
	     c_getpwuid_r uid ppw pbuf (fromIntegral pwBufSize) pppw
	throwErrnoIfNull "getUserEntryForID" $
	     peekElemOff pppw 0
	unpackUserEntry ppw

foreign import ccall unsafe "getpwuid_r"
  c_getpwuid_r :: CUid -> Ptr CPasswd -> 
			CString -> CSize -> Ptr (Ptr CPasswd) -> IO CInt
#elif HAVE_GETPWUID
getUserEntryForID uid = do
  withMVar lock $ \_ -> do
    ppw <- throwErrnoIfNull "getUserEntryForID" $ c_getpwuid uid
    unpackUserEntry ppw

foreign import ccall unsafe "getpwuid" 
  c_getpwuid :: CUid -> IO (Ptr CPasswd)
#else
getUserEntryForID = error "System.Posix.User.getUserEntryForID: not supported"
#endif

-- | @getUserEntryForName name@ calls @getpwnam@ to obtain
--   the @UserEntry@ information associated with the user login
--   @name@.
getUserEntryForName :: String -> IO UserEntry
#if HAVE_GETPWNAM_R
getUserEntryForName name = do
  allocaBytes (#const sizeof(struct passwd)) $ \ppw ->
    allocaBytes pwBufSize $ \pbuf ->
      alloca $ \ pppw -> 
	withCString name $ \ pstr -> do
          throwErrorIfNonZero_ "getUserEntryForName" $
	       c_getpwnam_r pstr ppw pbuf (fromIntegral pwBufSize) pppw
	  throwErrnoIfNull "getUserEntryForName" $
		peekElemOff pppw 0
	  unpackUserEntry ppw

foreign import ccall unsafe "getpwnam_r"
  c_getpwnam_r :: CString -> Ptr CPasswd -> 
			CString -> CSize -> Ptr (Ptr CPasswd) -> IO CInt
#elif HAVE_GETPWNAM
getUserEntryForName name = do
  withCString name $ \ pstr -> do
    withMVar lock $ \_ -> do
      ppw <- throwErrnoIfNull "getUserEntryForName" $ c_getpwnam pstr
      unpackUserEntry ppw

foreign import ccall unsafe "getpwnam" 
  c_getpwnam :: CString -> IO (Ptr CPasswd)
#else
getUserEntryForName = error "System.Posix.User.getUserEntryForName: not supported"
#endif

-- | @getAllUserEntries@ returns all user entries on the system by 
--   repeatedly calling @getpwent@
getAllUserEntries :: IO [UserEntry]
#ifdef HAVE_GETPWENT
getAllUserEntries = 
    withMVar lock $ \_ -> worker []
    where worker accum = 
              do resetErrno
                 ppw <- throwErrnoIfNullAndError "getAllUserEntries" $ 
                        c_getpwent
                 if ppw == nullPtr
                     then return (reverse accum)
                     else do thisentry <- unpackUserEntry ppw
                             worker (thisentry : accum)

foreign import ccall unsafe "getpwent"
  c_getpwent :: IO (Ptr CPasswd)
#else
getAllUserEntries = error "System.Posix.User.getAllUserEntries: not supported"
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
   name   <- (#peek struct passwd, pw_name)   ptr >>= peekCString
   passwd <- (#peek struct passwd, pw_passwd) ptr >>= peekCString
   uid    <- (#peek struct passwd, pw_uid)    ptr
   gid    <- (#peek struct passwd, pw_gid)    ptr
   gecos  <- (#peek struct passwd, pw_gecos)  ptr >>= peekCString
   dir    <- (#peek struct passwd, pw_dir)    ptr >>= peekCString
   shell  <- (#peek struct passwd, pw_shell)  ptr >>= peekCString
   return (UserEntry name passwd uid gid gecos dir shell)

-- Used when calling re-entrant system calls that signal their 'errno' 
-- directly through the return value.
throwErrorIfNonZero_ :: String -> IO CInt -> IO ()
throwErrorIfNonZero_ loc act = do
    rc <- act
    if (rc == 0) 
     then return ()
     else ioError (errnoToIOError loc (Errno (fromIntegral rc)) Nothing Nothing)

-- Used when a function returns NULL to indicate either an error or
-- EOF, depending on whether the global errno is nonzero.
throwErrnoIfNullAndError :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNullAndError loc act = do
    rc <- act
    errno <- getErrno
    if rc == nullPtr && errno /= eOK
       then throwErrno loc
       else return rc
