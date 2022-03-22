-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.IO.PosixString
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
-- "System.IO".
--
-----------------------------------------------------------------------------

#include "HsUnix.h"

module System.Posix.IO.PosixString (
    -- * Input \/ Output

    -- ** Standard file descriptors
    stdInput, stdOutput, stdError,

    -- ** Opening and closing files
    OpenMode(..),
    OpenFileFlags(..), defaultFileFlags,
    openFd, openFdAt, createFile, createFileAt,
    closeFd,

    -- ** Reading\/writing data
    -- |Programmers using the 'fdRead' and 'fdWrite' API should be aware that
    -- EAGAIN exceptions may occur for non-blocking IO!

    fdRead, fdWrite,
    fdReadBuf, fdWriteBuf,

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

import System.Posix.Types
import System.Posix.IO.Common
import System.Posix.IO.ByteString ( fdRead, fdWrite )
import System.OsPath.Types

import System.Posix.PosixPath.FilePath



-- |Open and optionally create this file.  See 'System.Posix.Files'
-- for information on how to use the 'FileMode' type.
openFd :: PosixPath
       -> OpenMode
       -> OpenFileFlags
       -> IO Fd
openFd = openFdAt Nothing

-- | Open a file relative to an optional directory file descriptor.
--
-- Directory file descriptors can be used to avoid some race conditions when
-- navigating changing directory trees, or to retain access to a portion of the
-- directory tree that would otherwise become inaccessible after dropping
-- privileges.
openFdAt :: Maybe Fd -- ^ Optional directory file descriptor
         -> PosixPath -- ^ Pathname to open
         -> OpenMode -- ^ Read-only, read-write or write-only
         -> OpenFileFlags -- ^ Append, exclusive, truncate, etc.
         -> IO Fd
openFdAt fdMay name how flags =
   withFilePath name $ \str ->
     throwErrnoPathIfMinus1Retry "openFdAt" name $
       openat_ fdMay str how flags

-- |Create and open this file in WriteOnly mode.  A special case of
-- 'openFd'.  See 'System.Posix.Files' for information on how to use
-- the 'FileMode' type.
createFile :: PosixPath -> FileMode -> IO Fd
createFile = createFileAt Nothing

-- | Create and open a file for write-only, with default flags,
-- relative an optional directory file-descriptor.
--
-- Directory file descriptors can be used to avoid some race conditions when
-- navigating changing directory trees, or to retain access to a portion of the
-- directory tree that would otherwise become inaccessible after dropping
-- privileges.
createFileAt :: Maybe Fd -- ^ Optional directory file descriptor
             -> PosixPath -- ^ Pathname to create
             -> FileMode -- ^ File permission bits (before umask)
             -> IO Fd
createFileAt fdMay name mode
  = openFdAt fdMay name WriteOnly defaultFileFlags{ trunc=True, creat=(Just mode) }
