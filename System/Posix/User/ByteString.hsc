{-# LANGUAGE Trustworthy, CApiFFI, PatternSynonyms, ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.User.ByteString
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

module System.Posix.User.ByteString (
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
    setEffectiveUserID,
    setEffectiveGroupID,
    setGroups

  ) where

#include "HsUnix.h"

import System.Posix.Types
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C ( CSize(..), CInt(..), CString, CLong(..), getErrno, throwErrno, eOK, throwErrnoIfMinus1_, throwErrnoIfNull, resetErrno, Errno(..), eRANGE, errnoToIOError )
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

import System.Posix.User.Common
#if defined(HAVE_GETPWENT) || defined(HAVE_GETGRENT)
#if defined(freebsd_HOST_OS)
import Control.Concurrent (runInBoundThread, rtsSupportsBoundThreads)
#endif
import Control.Concurrent.MVar ( MVar, newMVar, withMVar )
import Control.Exception
#endif
import Control.Monad
import System.IO.Error
import Data.ByteString ( ByteString, packCString, useAsCString )

#if !defined(HAVE_PWD_H)
import System.IO.Error ( ioeSetLocation )
import GHC.IO.Exception ( unsupportedOperation )
#endif


#if !defined(HAVE_PWD_H)

getRealUserID :: IO UserID
{-# WARNING getRealUserID
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_PWD_H@)" #-}
getRealUserID = ioError (ioeSetLocation unsupportedOperation "getRealUserID")

getRealGroupID :: IO GroupID
{-# WARNING getRealGroupID
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_PWD_H@)" #-}
getRealGroupID = ioError (ioeSetLocation unsupportedOperation "getRealGroupID")

getEffectiveUserID :: IO UserID
{-# WARNING getEffectiveUserID
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_PWD_H@)" #-}
getEffectiveUserID = ioError (ioeSetLocation unsupportedOperation "getEffectiveUserID")

getEffectiveGroupID :: IO GroupID
{-# WARNING getEffectiveGroupID
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_PWD_H@)" #-}
getEffectiveGroupID = ioError (ioeSetLocation unsupportedOperation "getEffectiveGroupID")

getGroups :: IO [GroupID]
{-# WARNING getGroups
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_PWD_H@)" #-}
getGroups = ioError (ioeSetLocation unsupportedOperation "getGroups")

setGroups :: [GroupID] -> IO ()
{-# WARNING setGroups
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_PWD_H@)" #-}
setGroups _ = ioError (ioeSetLocation unsupportedOperation "setGroups")

getLoginName :: IO ByteString
{-# WARNING getLoginName
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_PWD_H@)" #-}
getLoginName = ioError (ioeSetLocation unsupportedOperation "getLoginName")

setUserID :: UserID -> IO ()
{-# WARNING setUserID
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_PWD_H@)" #-}
setUserID _ = ioError (ioeSetLocation unsupportedOperation "setUserID")

setEffectiveUserID :: UserID -> IO ()
{-# WARNING setEffectiveUserID
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_PWD_H@)" #-}
setEffectiveUserID _ = ioError (ioeSetLocation unsupportedOperation "setEffectiveUserID")

setGroupID :: GroupID -> IO ()
{-# WARNING setGroupID
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_PWD_H@)" #-}
setGroupID _ = ioError (ioeSetLocation unsupportedOperation "setGroupID")

setEffectiveGroupID :: GroupID -> IO ()
{-# WARNING setEffectiveGroupID
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_PWD_H@)" #-}
setEffectiveGroupID _ = ioError (ioeSetLocation unsupportedOperation "setEffectiveGroupID")

getEffectiveUserName :: IO ByteString
{-# WARNING getEffectiveUserName
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_PWD_H@)" #-}
getEffectiveUserName = ioError (ioeSetLocation unsupportedOperation "getEffectiveUserName")

#else
-- -----------------------------------------------------------------------------
-- Thread safety of passwd/group database access APIs:
--
-- All supported unix platforms have @get(pw|gr)(nam|[ug]id)_r(3)@, which
-- store the result in a caller provided buffer, which solves the most
-- immediate thread-safety issues.
--
-- Things are more complicated for getpwent(3) and getgrent(3).
--
-- * On Linux systems, these read a global open file, opened via
--   setpwent(3) and closed via endpwent(3).  Only one thread at
--   a time can safely iterate through the file.
--
-- * On macOS (through Catalina 10.15), there is no getpwent_r(3) or
--   getgrent_r(3), so a lock is also required for safe buffer sharing.
--
-- * On FreeBSD, in the default configuration with passwd lookups configured
--   in nsswitch.conf to use "compat" rather than "files", the getpwnam_r(3)
--   and getpwuid_r(3) functions reset the iterator index used by getpwent(3).
--   A bug [report](https://bugs.freebsd.org/bugzilla/show_bug.cgi?id=252094)
--   has been filed to track this long-standing issue.  A similar issue affects
--   getgrent(3), this time regardless of the nsswitch.conf setting.  This too
--   should be fixed at some point in the future.  The state in question is
--   thread-specific, so both issues only affect overlapping use of the @*ent@
--   and @*(nam|[ug]id)_r(3)@ functions in the /same/ thread.
--
-- * Despite rather similar manpages for getpwent(3) and getpwnam(3), ... as
--   on FreeBSD, the above issue is not seen on NetBSD or macOS.
--
--   This is not an issue with 1-to-1 thread models, where the code executing
--   @get(pw|gr)ent@ has exclusive use of its thread, but it is a real issue
--   for Haskell with its many-to-1 green threads, because multiple `forkIO`
--   threads may take turns using the same underlying OS thread, breaking the
--   thread-safety of the @*_r@ functions, which mutate the file-offset of the
--   open file shared with any overlapping execution of @*ent(3)@ in the same
--   thread.
--
-- Consequently, correct portable support for @get(pw|gr)ent(3)@ is rather
-- non-trivial.  In the threaded runtime, we can run these functions in a
-- /bound thread/ (via 'forkOS'), thereby avoiding the FreeBSD issues.  We
-- still need a lock to serialise multiple threads calling these functions
-- on at least macOS for lack of @_r@ equivalents.  While on FreeBSD we could
-- use @getpwent_r(3)@ and @getgrent_r(3)@ in a bound thread without any
-- locks, implementing this special case is likely not worthwhile.
--
-- In the non-threaded runtime, `forkOS` is not available, and so on FreeBSD
-- systems we have to also lock the @*(nam|[ug]id)_r(3)@ functions to avoid
-- concurrent use with @*ent(3)@.
--
-- FWIW, the below Perl one-liners will quickly show whether interleaved calls
-- of getpwuid() or getgrgid() disturb iteration through all the entries. If
-- each line of output is distinct, there is likely no issue.  If the same
-- passwd or group entry repeats multiple times, the system is affected.
--
-- > for ($i=0;$i<3;++$i) {getpwuid(0); print join(":",getpwent()),"\n"}
-- > for ($i=0;$i<3;++$i) {getgrgid(0); print join(":",getgrent()),"\n"}
--
-- XXX: It has been suggested, not without some merit, that attempts to
-- enumerate /all/ users or /all/ groups are fundamentally flawed.  Modern
-- unix systems have a variety nsswitch backends, some of which instantiate
-- users on demand or may enumerate slowly or not at all.  We could shed a
-- lot of complexity by deprecating the "get all" functions and simply
-- always returning an empty list.
--

#if defined(HAVE_GETPWENT)
pwlock :: MVar ()
pwlock = unsafePerformIO $ newMVar ()
{-# NOINLINE pwlock #-}

lockpw :: LKUPTYPE -> IO a -> IO a
#if defined(freebsd_HOST_OS)
lockpw GETONE
    | rtsSupportsBoundThreads = id
    | otherwise = withMVar pwlock . const
lockpw GETALL
    | rtsSupportsBoundThreads = runInBoundThread . withMVar pwlock . const
    | otherwise = withMVar pwlock . const
#else
lockpw GETONE = id
lockpw GETALL = withMVar pwlock . const
#endif
#else
lockpw _ = id
#endif

#if defined(HAVE_GETGRENT)
grlock :: MVar ()
grlock = unsafePerformIO $ newMVar ()
{-# NOINLINE grlock #-}

lockgr :: LKUPTYPE -> IO a -> IO a
#if defined(freebsd_HOST_OS)
lockgr GETONE
    | rtsSupportsBoundThreads = id
    | otherwise = withMVar grlock . const
lockgr GETALL
    | rtsSupportsBoundThreads = runInBoundThread . withMVar grlock . const
    | otherwise = withMVar grlock . const
#else
lockgr GETONE = id
lockgr GETALL = withMVar grlock . const
#endif
#else
lockgr _ = id
#endif

-- -----------------------------------------------------------------------------
-- user environment

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


-- | @setGroups@ calls @setgroups@ to set the list of
--   supplementary @GroupID@s associated with the current process.
setGroups :: [GroupID] -> IO ()
setGroups groups = do
    withArrayLen groups $ \ ngroups arr ->
       throwErrnoIfMinus1_ "setGroups" (c_setgroups (fromIntegral ngroups) arr)

foreign import ccall unsafe "setgroups"
  c_setgroups :: CInt -> Ptr CGid -> IO CInt


-- | @getLoginName@ calls @getlogin@ to obtain the login name
--   associated with the current process.
getLoginName :: IO ByteString
getLoginName =  do
    -- ToDo: use getlogin_r
    str <- throwErrnoIfNull "getLoginName" c_getlogin
    packCString str

foreign import ccall unsafe "getlogin"
  c_getlogin :: IO CString

-- | @setUserID uid@ calls @setuid@ to set the real, effective, and
--   saved set-user-id associated with the current process to @uid@.
setUserID :: UserID -> IO ()
setUserID uid = throwErrnoIfMinus1_ "setUserID" (c_setuid uid)

foreign import ccall unsafe "setuid"
  c_setuid :: CUid -> IO CInt

-- | @setEffectiveUserID uid@ calls @seteuid@ to set the effective
--   user-id associated with the current process to @uid@. This
--   does not update the real user-id or set-user-id.
setEffectiveUserID :: UserID -> IO ()
setEffectiveUserID uid = throwErrnoIfMinus1_ "setEffectiveUserID" (c_seteuid uid)

foreign import ccall unsafe "seteuid"
  c_seteuid :: CUid -> IO CInt

-- | @setGroupID gid@ calls @setgid@ to set the real, effective, and
--   saved set-group-id associated with the current process to @gid@.
setGroupID :: GroupID -> IO ()
setGroupID gid = throwErrnoIfMinus1_ "setGroupID" (c_setgid gid)

foreign import ccall unsafe "setgid"
  c_setgid :: CGid -> IO CInt

-- | @setEffectiveGroupID uid@ calls @setegid@ to set the effective
--   group-id associated with the current process to @gid@. This
--   does not update the real group-id or set-group-id.
setEffectiveGroupID :: GroupID -> IO ()
setEffectiveGroupID gid =
  throwErrnoIfMinus1_ "setEffectiveGroupID" (c_setegid gid)


foreign import ccall unsafe "setegid"
  c_setegid :: CGid -> IO CInt

-- -----------------------------------------------------------------------------
-- User names

-- | @getEffectiveUserName@ gets the name
--   associated with the effective @UserID@ of the process.
getEffectiveUserName :: IO ByteString
getEffectiveUserName = do
    euid <- getEffectiveUserID
    pw <- getUserEntryForID euid
    return (userName pw)

#endif // HAVE_PWD_H

-- -----------------------------------------------------------------------------
-- The group database (grp.h)


#if !defined(HAVE_PWD_H)

getGroupEntryForID :: GroupID -> IO GroupEntry
{-# WARNING getGroupEntryForID
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_PWD_H@)" #-}
getGroupEntryForID _ = ioError (ioeSetLocation unsupportedOperation "getGroupEntryForID")

getGroupEntryForName :: ByteString -> IO GroupEntry
{-# WARNING getGroupEntryForName
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_PWD_H@)" #-}
getGroupEntryForName _ = ioError (ioeSetLocation unsupportedOperation "getGroupEntryForName")

getAllGroupEntries :: IO [GroupEntry]
{-# WARNING getAllGroupEntries
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_PWD_H@)" #-}
getAllGroupEntries = ioError (ioeSetLocation unsupportedOperation "getAllGroupEntries")

#else

-- | @getGroupEntryForID gid@ calls @getgrgid_r@ to obtain
--   the @GroupEntry@ information associated with @GroupID@
--   @gid@. This operation may fail with 'isDoesNotExistError'
--   if no such group exists.
getGroupEntryForID :: GroupID -> IO GroupEntry
#ifdef HAVE_GETGRGID_R
getGroupEntryForID gid = lockgr GETONE $
    allocaBytes (#const sizeof(struct group)) $ \pgr ->
        doubleAllocWhileERANGE "getGroupEntryForID" "group"
            grBufSize unpackGroupEntry $ c_getgrgid_r gid pgr

foreign import capi safe "HsUnix.h getgrgid_r"
  c_getgrgid_r :: CGid -> Ptr CGroup -> CString
                 -> CSize -> Ptr (Ptr CGroup) -> IO CInt
#else
{-# WARNING getGroupEntryForID "System.Posix.User.getGroupEntryForID: not supported" #-}
getGroupEntryForID = error "System.Posix.User.getGroupEntryForID: not supported"
#endif

-- | @getGroupEntryForName name@ calls @getgrnam_r@ to obtain
--   the @GroupEntry@ information associated with the group called
--   @name@. This operation may fail with 'isDoesNotExistError'
--   if no such group exists.
getGroupEntryForName :: ByteString -> IO GroupEntry
#ifdef HAVE_GETGRNAM_R
getGroupEntryForName name = lockgr GETONE $
    allocaBytes (#const sizeof(struct group)) $ \pgr ->
        useAsCString name $ \ pstr ->
            doubleAllocWhileERANGE "getGroupEntryForName" "group"
                grBufSize unpackGroupEntry $ c_getgrnam_r pstr pgr

foreign import capi safe "HsUnix.h getgrnam_r"
  c_getgrnam_r :: CString -> Ptr CGroup -> CString
                 -> CSize -> Ptr (Ptr CGroup) -> IO CInt
#else
{-# WARNING getGroupEntryForName "System.Posix.User.getGroupEntryForName: not supported" #-}
getGroupEntryForName = error "System.Posix.User.getGroupEntryForName: not supported"
#endif

-- | @getAllGroupEntries@ returns all group entries on the system by
--   repeatedly calling @getgrent@

--
-- getAllGroupEntries may fail with isDoesNotExistError on Linux due to
-- this bug in glibc:
--   http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=466647
--
getAllGroupEntries :: IO [GroupEntry]
#ifdef HAVE_GETGRENT
getAllGroupEntries = lockgr GETALL $ bracket_ c_setgrent c_endgrent $ worker []
  where
    worker accum = do
        resetErrno
        ppw <- throwErrnoIfNullAndError "getAllGroupEntries" $ c_getgrent
        if ppw == nullPtr
            then return (reverse accum)
            else do thisentry <- unpackGroupEntry ppw
                    worker (thisentry : accum)

foreign import ccall safe "getgrent" c_getgrent :: IO (Ptr CGroup)
foreign import ccall safe "setgrent" c_setgrent :: IO ()
foreign import ccall safe "endgrent" c_endgrent :: IO ()
#else
{-# WARNING getAllGroupEntries "System.Posix.User.getAllGroupEntries: not supported" #-}
getAllGroupEntries = error "System.Posix.User.getAllGroupEntries: not supported"
#endif

#if defined(HAVE_GETGRGID_R) || defined(HAVE_GETGRNAM_R)
grBufSize :: Int
#if defined(HAVE_SYSCONF) && defined(HAVE_SC_GETGR_R_SIZE_MAX)
grBufSize = sysconfWithDefault 1024 (#const _SC_GETGR_R_SIZE_MAX)
#else
grBufSize = 1024
#endif
#endif

#endif // HAVE_PWD_H

-- -----------------------------------------------------------------------------
-- The user database (pwd.h)


-- | @getUserEntryForID uid@ calls @getpwuid_r@ to obtain
--   the @UserEntry@ information associated with @UserID@
--   @uid@. This operation may fail with 'isDoesNotExistError'
--   if no such user exists.
getUserEntryForID :: UserID -> IO UserEntry
#ifdef HAVE_GETPWUID_R
getUserEntryForID uid = lockpw GETONE $
    allocaBytes (#const sizeof(struct passwd)) $ \ppw ->
        doubleAllocWhileERANGE "getUserEntryForID" "user"
            pwBufSize unpackUserEntry $ c_getpwuid_r uid ppw

foreign import capi safe "HsUnix.h getpwuid_r"
  c_getpwuid_r :: CUid -> Ptr CPasswd ->
                        CString -> CSize -> Ptr (Ptr CPasswd) -> IO CInt
#else
{-# WARNING getUserEntryForID "System.Posix.User.getUserEntryForID: not supported" #-}
getUserEntryForID = error "System.Posix.User.getUserEntryForID: not supported"
#endif

-- | @getUserEntryForName name@ calls @getpwnam_r@ to obtain
--   the @UserEntry@ information associated with the user login
--   @name@. This operation may fail with 'isDoesNotExistError'
--   if no such user exists.
getUserEntryForName :: ByteString -> IO UserEntry
#if HAVE_GETPWNAM_R
getUserEntryForName name = lockpw GETONE $
    allocaBytes (#const sizeof(struct passwd)) $ \ppw ->
        useAsCString name $ \ pstr ->
            doubleAllocWhileERANGE "getUserEntryForName" "user"
                pwBufSize unpackUserEntry $ c_getpwnam_r pstr ppw

foreign import capi safe "HsUnix.h getpwnam_r"
  c_getpwnam_r :: CString -> Ptr CPasswd
               -> CString -> CSize -> Ptr (Ptr CPasswd) -> IO CInt
#else
{-# WARNING getUserEntryForName "System.Posix.User.getUserEntryForName: not supported" #-}
getUserEntryForName = error "System.Posix.User.getUserEntryForName: not supported"
#endif

-- | @getAllUserEntries@ returns all user entries on the system by
--   repeatedly calling @getpwent@
getAllUserEntries :: IO [UserEntry]
#ifdef HAVE_GETPWENT
getAllUserEntries = lockpw GETALL $ bracket_ c_setpwent c_endpwent $ worker []
  where
    worker accum = do
        resetErrno
        ppw <- throwErrnoIfNullAndError "getAllUserEntries" $ c_getpwent
        if ppw == nullPtr
            then return (reverse accum)
            else do thisentry <- unpackUserEntry ppw
                    worker (thisentry : accum)

foreign import ccall safe "getpwent" c_getpwent :: IO (Ptr CPasswd)
foreign import ccall safe "setpwent" c_setpwent :: IO ()
foreign import ccall safe "endpwent" c_endpwent :: IO ()
#else
{-# WARNING getAllUserEntries "System.Posix.User.getAllUserEntries: not supported" #-}
getAllUserEntries = error "System.Posix.User.getAllUserEntries: not supported"
#endif

#if defined(HAVE_GETPWUID_R) || defined(HAVE_GETPWNAM_R)
pwBufSize :: Int
#if  defined(HAVE_SYSCONF) && defined(HAVE_SC_GETPW_R_SIZE_MAX)
pwBufSize = sysconfWithDefault 1024 (#const _SC_GETPW_R_SIZE_MAX)
#else
pwBufSize = 1024
#endif
#endif

#if defined(HAVE_SYSCONF) && defined(HAVE_PWD_H)
foreign import ccall unsafe "sysconf"
  c_sysconf :: CInt -> IO CLong

-- We need a default value since sysconf can fail and return -1
-- even when the parameter name is defined in unistd.h.
-- One example of this is _SC_GETPW_R_SIZE_MAX under
-- Mac OS X 10.4.9 on i386.
sysconfWithDefault :: Int -> CInt -> Int
sysconfWithDefault def sc =
    unsafePerformIO $ do v <- fmap fromIntegral $ c_sysconf sc
                         return $ if v == (-1) then def else v
#endif

#if defined(HAVE_PWD_H)

-- The following function is used by the getgr*_r, c_getpw*_r
-- families of functions. These functions return their result
-- in a struct that contains strings and they need a buffer
-- that they can use to store those strings. We have to be
-- careful to unpack the struct containing the result before
-- the buffer is deallocated.
doubleAllocWhileERANGE
  :: String
  -> String -- entry type: "user" or "group"
  -> Int
  -> (Ptr r -> IO a)
  -> (Ptr b -> CSize -> Ptr (Ptr r) -> IO CInt)
  -> IO a
doubleAllocWhileERANGE loc enttype initlen unpack action =
  alloca $ go initlen
 where
  go len res = do
    r <- allocaBytes len $ \buf -> do
           rc <- action buf (fromIntegral len) res
           if rc /= 0
             then return (Left rc)
             else do p <- peek res
                     when (p == nullPtr) $ notFoundErr
                     fmap Right (unpack p)
    case r of
      Right x -> return x
      Left rc | Errno rc == eRANGE ->
        -- ERANGE means this is not an error
        -- we just have to try again with a larger buffer
        go (2 * len) res
      Left rc ->
        ioError (errnoToIOError loc (Errno rc) Nothing Nothing)
  notFoundErr =
    ioError $ flip ioeSetErrorString ("no such " ++ enttype)
            $ mkIOError doesNotExistErrorType loc Nothing Nothing

-- Used when a function returns NULL to indicate either an error or
-- EOF, depending on whether the global errno is nonzero.
throwErrnoIfNullAndError :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNullAndError loc act = do
    rc <- act
    errno <- getErrno
    if rc == nullPtr && errno /= eOK
       then throwErrno loc
       else return rc

#endif // HAVE_PWD_H
