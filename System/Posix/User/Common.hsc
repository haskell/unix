{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.User.Common
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

module System.Posix.User.Common where

import Data.ByteString ( ByteString )
import System.Posix.Types

#include "HsUnix.h"


#if defined(HAVE_PWD_H)
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Data.ByteString ( packCString )

-- internal types
data {-# CTYPE "struct passwd" #-} CPasswd
data {-# CTYPE "struct group"  #-} CGroup

data LKUPTYPE = GETONE | GETALL

unpackGroupEntry :: Ptr CGroup -> IO GroupEntry
unpackGroupEntry ptr = do
   name    <- (#peek struct group, gr_name) ptr >>= packCString
   passwd  <- (#peek struct group, gr_passwd) ptr >>= packCString
   gid     <- (#peek struct group, gr_gid) ptr
   mem     <- (#peek struct group, gr_mem) ptr
   members <- peekArray0 nullPtr mem >>= mapM packCString
   return (GroupEntry name passwd gid members)

unpackUserEntry :: Ptr CPasswd -> IO UserEntry
unpackUserEntry ptr = do
   name   <- (#peek struct passwd, pw_name)   ptr >>= packCString
   passwd <- (#peek struct passwd, pw_passwd) ptr >>= packCString
   uid    <- (#peek struct passwd, pw_uid)    ptr
   gid    <- (#peek struct passwd, pw_gid)    ptr
#ifdef HAVE_NO_PASSWD_PW_GECOS
   gecos  <- return ""  -- pw_gecos does not exist on android
#else
   gecos  <- (#peek struct passwd, pw_gecos)  ptr >>= packCString
#endif
   dir    <- (#peek struct passwd, pw_dir)    ptr >>= packCString
   shell  <- (#peek struct passwd, pw_shell)  ptr >>= packCString
   return (UserEntry name passwd uid gid gecos dir shell)

#endif // HAVE_PWD_H

data UserEntry =
 UserEntry {
   userName      :: ByteString,     -- ^ Textual name of this user (pw_name)
   userPassword  :: ByteString,     -- ^ Password -- may be empty or fake if shadow is in use (pw_passwd)
   userID        :: UserID,         -- ^ Numeric ID for this user (pw_uid)
   userGroupID   :: GroupID,        -- ^ Primary group ID (pw_gid)
   userGecos     :: ByteString,     -- ^ Usually the real name for the user (pw_gecos)
   homeDirectory :: ByteString,     -- ^ Home directory (pw_dir)
   userShell     :: ByteString      -- ^ Default shell (pw_shell)
 } deriving (Show, Read, Eq)

data GroupEntry =
 GroupEntry {
  groupName     :: ByteString,   -- ^ The name of this group (gr_name)
  groupPassword :: ByteString,   -- ^ The password for this group (gr_passwd)
  groupID       :: GroupID,      -- ^ The unique numeric ID for this group (gr_gid)
  groupMembers  :: [ByteString]  -- ^ A list of zero or more usernames that are members (gr_mem)
 } deriving (Show, Read, Eq)
