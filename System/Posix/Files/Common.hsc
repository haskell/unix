{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Files.Common
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- Functions defined by the POSIX standards for manipulating and querying the
-- file system. Names of underlying POSIX functions are indicated whenever
-- possible. A more complete documentation of the POSIX functions together
-- with a more detailed description of different error conditions are usually
-- available in the system's manual pages or from
-- <http://www.unix.org/version3/online.html> (free registration required).
--
-- When a function that calls an underlying POSIX function fails, the errno
-- code is converted to an 'IOError' using 'Foreign.C.Error.errnoToIOError'.
-- For a list of which errno codes may be generated, consult the POSIX
-- documentation for the underlying function.
--
-----------------------------------------------------------------------------

#include "HsUnix.h"

module System.Posix.Files.Common (
    -- * File modes
    -- FileMode exported by System.Posix.Types
    unionFileModes, intersectFileModes,
    nullFileMode,
    ownerReadMode, ownerWriteMode, ownerExecuteMode, ownerModes,
    groupReadMode, groupWriteMode, groupExecuteMode, groupModes,
    otherReadMode, otherWriteMode, otherExecuteMode, otherModes,
    setUserIDMode, setGroupIDMode,
    stdFileMode,   accessModes,
    fileTypeModes,
    blockSpecialMode, characterSpecialMode, namedPipeMode, regularFileMode,
    directoryMode, symbolicLinkMode, socketMode,

    -- ** Setting file modes
    setFdMode, setFileCreationMask,

    -- * File status
    FileStatus(..),
    -- ** Obtaining file status
    getFdStatus,
    -- ** Querying file status
    deviceID, fileID, fileMode, linkCount, fileOwner, fileGroup,
    specialDeviceID, fileSize, accessTime, modificationTime,
    statusChangeTime,
    isBlockDevice, isCharacterDevice, isNamedPipe, isRegularFile,
    isDirectory, isSymbolicLink, isSocket,

    -- * Setting file sizes
    setFdSize,

    -- * Changing file ownership
    setFdOwnerAndGroup,

    -- * Find system-specific limits for a file
    PathVar(..), getFdPathVar, pathVarConst,
  ) where

import System.Posix.Error
import System.Posix.Types
import System.IO.Unsafe
import Data.Bits
import System.Posix.Internals
import Foreign hiding (unsafePerformIO)
import Foreign.C

-- -----------------------------------------------------------------------------
-- POSIX file modes

-- The abstract type 'FileMode', constants and operators for
-- manipulating the file modes defined by POSIX.

-- | No permissions.
nullFileMode :: FileMode
nullFileMode = 0

-- | Owner has read permission.
ownerReadMode :: FileMode
ownerReadMode = (#const S_IRUSR)

-- | Owner has write permission.
ownerWriteMode :: FileMode
ownerWriteMode = (#const S_IWUSR)

-- | Owner has execute permission.
ownerExecuteMode :: FileMode
ownerExecuteMode = (#const S_IXUSR)

-- | Group has read permission.
groupReadMode :: FileMode
groupReadMode = (#const S_IRGRP)

-- | Group has write permission.
groupWriteMode :: FileMode
groupWriteMode = (#const S_IWGRP)

-- | Group has execute permission.
groupExecuteMode :: FileMode
groupExecuteMode = (#const S_IXGRP)

-- | Others have read permission.
otherReadMode :: FileMode
otherReadMode = (#const S_IROTH)

-- | Others have write permission.
otherWriteMode :: FileMode
otherWriteMode = (#const S_IWOTH)

-- | Others have execute permission.
otherExecuteMode :: FileMode
otherExecuteMode = (#const S_IXOTH)

-- | Set user ID on execution.
setUserIDMode :: FileMode
setUserIDMode = (#const S_ISUID)

-- | Set group ID on execution.
setGroupIDMode :: FileMode
setGroupIDMode = (#const S_ISGID)

-- | Owner, group and others have read and write permission.
stdFileMode :: FileMode
stdFileMode = ownerReadMode  .|. ownerWriteMode .|. 
	      groupReadMode  .|. groupWriteMode .|. 
	      otherReadMode  .|. otherWriteMode

-- | Owner has read, write and execute permission.
ownerModes :: FileMode
ownerModes = (#const S_IRWXU)

-- | Group has read, write and execute permission.
groupModes :: FileMode
groupModes = (#const S_IRWXG)

-- | Others have read, write and execute permission.
otherModes :: FileMode
otherModes = (#const S_IRWXO)

-- | Owner, group and others have read, write and execute permission.
accessModes :: FileMode
accessModes = ownerModes .|. groupModes .|. otherModes

-- | Combines the two file modes into one that contains modes that appear in
-- either.
unionFileModes :: FileMode -> FileMode -> FileMode
unionFileModes m1 m2 = m1 .|. m2

-- | Combines two file modes into one that only contains modes that appear in
-- both.
intersectFileModes :: FileMode -> FileMode -> FileMode
intersectFileModes m1 m2 = m1 .&. m2

fileTypeModes :: FileMode
fileTypeModes = (#const S_IFMT)

blockSpecialMode :: FileMode
blockSpecialMode = (#const S_IFBLK)

characterSpecialMode :: FileMode
characterSpecialMode = (#const S_IFCHR)

namedPipeMode :: FileMode
namedPipeMode = (#const S_IFIFO)

regularFileMode :: FileMode
regularFileMode = (#const S_IFREG)

directoryMode :: FileMode
directoryMode = (#const S_IFDIR)

symbolicLinkMode :: FileMode
symbolicLinkMode = (#const S_IFLNK)

socketMode :: FileMode
socketMode = (#const S_IFSOCK)

-- | @setFdMode fd mode@ acts like 'setFileMode' but uses a file descriptor
-- @fd@ instead of a 'FilePath'.
--
-- Note: calls @fchmod@.
setFdMode :: Fd -> FileMode -> IO ()
setFdMode (Fd fd) m =
  throwErrnoIfMinus1_ "setFdMode" (c_fchmod fd m)

foreign import ccall unsafe "fchmod" 
  c_fchmod :: CInt -> CMode -> IO CInt

-- | @setFileCreationMask mode@ sets the file mode creation mask to @mode@.
-- Modes set by this operation are subtracted from files and directories upon
-- creation. The previous file creation mask is returned.
--
-- Note: calls @umask@.
setFileCreationMask :: FileMode -> IO FileMode
setFileCreationMask mask = c_umask mask

-- -----------------------------------------------------------------------------
-- stat() support

-- | POSIX defines operations to get information, such as owner, permissions,
-- size and access times, about a file. This information is represented by the
-- 'FileStatus' type.
--
-- Note: see @chmod@.
newtype FileStatus = FileStatus (ForeignPtr CStat)

-- | ID of the device on which this file resides.
deviceID         :: FileStatus -> DeviceID
-- | inode number
fileID           :: FileStatus -> FileID
-- | File mode (such as permissions).
fileMode         :: FileStatus -> FileMode
-- | Number of hard links to this file.
linkCount        :: FileStatus -> LinkCount
-- | ID of owner.
fileOwner        :: FileStatus -> UserID
-- | ID of group.
fileGroup        :: FileStatus -> GroupID
-- | Describes the device that this file represents.
specialDeviceID  :: FileStatus -> DeviceID
-- | Size of the file in bytes. If this file is a symbolic link the size is
-- the length of the pathname it contains.
fileSize         :: FileStatus -> FileOffset
-- | Time of last access.
accessTime       :: FileStatus -> EpochTime
-- | Time of last modification.
modificationTime :: FileStatus -> EpochTime
-- | Time of last status change (i.e. owner, group, link count, mode, etc.).
statusChangeTime :: FileStatus -> EpochTime

deviceID (FileStatus stat) = 
  unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_dev)
fileID (FileStatus stat) = 
  unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_ino)
fileMode (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_mode)
linkCount (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_nlink)
fileOwner (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_uid)
fileGroup (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_gid)
specialDeviceID (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_rdev)
fileSize (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_size)
accessTime (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_atime)
modificationTime (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_mtime)
statusChangeTime (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_ctime)

-- | Checks if this file is a block device.
isBlockDevice     :: FileStatus -> Bool
-- | Checks if this file is a character device.
isCharacterDevice :: FileStatus -> Bool
-- | Checks if this file is a named pipe device.
isNamedPipe       :: FileStatus -> Bool
-- | Checks if this file is a regular file device.
isRegularFile     :: FileStatus -> Bool
-- | Checks if this file is a directory device.
isDirectory       :: FileStatus -> Bool
-- | Checks if this file is a symbolic link device.
isSymbolicLink    :: FileStatus -> Bool
-- | Checks if this file is a socket device.
isSocket          :: FileStatus -> Bool

isBlockDevice stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == blockSpecialMode
isCharacterDevice stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == characterSpecialMode
isNamedPipe stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == namedPipeMode
isRegularFile stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == regularFileMode
isDirectory stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == directoryMode
isSymbolicLink stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == symbolicLinkMode
isSocket stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == socketMode

-- | @getFdStatus fd@ acts as 'getFileStatus' but uses a file descriptor @fd@.
--
-- Note: calls @fstat@.
getFdStatus :: Fd -> IO FileStatus
getFdStatus (Fd fd) = do
  fp <- mallocForeignPtrBytes (#const sizeof(struct stat)) 
  withForeignPtr fp $ \p ->
    throwErrnoIfMinus1_ "getFdStatus" (c_fstat fd p)
  return (FileStatus fp)

-- -----------------------------------------------------------------------------
-- fchown()

-- | Acts as 'setOwnerAndGroup' but uses a file descriptor instead of a
-- 'FilePath'.
--
-- Note: calls @fchown@.
setFdOwnerAndGroup :: Fd -> UserID -> GroupID -> IO ()
setFdOwnerAndGroup (Fd fd) uid gid = 
  throwErrnoIfMinus1_ "setFdOwnerAndGroup" (c_fchown fd uid gid)

foreign import ccall unsafe "fchown"
  c_fchown :: CInt -> CUid -> CGid -> IO CInt

-- -----------------------------------------------------------------------------
-- ftruncate()

-- | Acts as 'setFileSize' but uses a file descriptor instead of a 'FilePath'.
--
-- Note: calls @ftruncate@.
setFdSize :: Fd -> FileOffset -> IO ()
setFdSize (Fd fd) off =
  throwErrnoIfMinus1_ "setFdSize" (c_ftruncate fd off)

-- -----------------------------------------------------------------------------
-- pathconf()/fpathconf() support

data PathVar
  = FileSizeBits		  {- _PC_FILESIZEBITS     -}
  | LinkLimit                     {- _PC_LINK_MAX         -}
  | InputLineLimit                {- _PC_MAX_CANON        -}
  | InputQueueLimit               {- _PC_MAX_INPUT        -}
  | FileNameLimit                 {- _PC_NAME_MAX         -}
  | PathNameLimit                 {- _PC_PATH_MAX         -}
  | PipeBufferLimit               {- _PC_PIPE_BUF         -}
				  -- These are described as optional in POSIX:
  				  {- _PC_ALLOC_SIZE_MIN     -}
  				  {- _PC_REC_INCR_XFER_SIZE -}
  				  {- _PC_REC_MAX_XFER_SIZE  -}
  				  {- _PC_REC_MIN_XFER_SIZE  -}
 				  {- _PC_REC_XFER_ALIGN     -}
  | SymbolicLinkLimit		  {- _PC_SYMLINK_MAX      -}
  | SetOwnerAndGroupIsRestricted  {- _PC_CHOWN_RESTRICTED -}
  | FileNamesAreNotTruncated      {- _PC_NO_TRUNC         -}
  | VDisableChar		  {- _PC_VDISABLE         -}
  | AsyncIOAvailable		  {- _PC_ASYNC_IO         -}
  | PrioIOAvailable		  {- _PC_PRIO_IO          -}
  | SyncIOAvailable		  {- _PC_SYNC_IO          -}

pathVarConst :: PathVar -> CInt
pathVarConst v = case v of
	LinkLimit     			-> (#const _PC_LINK_MAX)
	InputLineLimit			-> (#const _PC_MAX_CANON)
	InputQueueLimit			-> (#const _PC_MAX_INPUT)
	FileNameLimit			-> (#const _PC_NAME_MAX)
	PathNameLimit			-> (#const _PC_PATH_MAX)
	PipeBufferLimit			-> (#const _PC_PIPE_BUF)
	SetOwnerAndGroupIsRestricted	-> (#const _PC_CHOWN_RESTRICTED)
	FileNamesAreNotTruncated	-> (#const _PC_NO_TRUNC)
	VDisableChar			-> (#const _PC_VDISABLE)

#ifdef _PC_SYNC_IO
	SyncIOAvailable		-> (#const _PC_SYNC_IO)
#else
	SyncIOAvailable		-> error "_PC_SYNC_IO not available"
#endif

#ifdef _PC_ASYNC_IO
	AsyncIOAvailable	-> (#const _PC_ASYNC_IO)
#else
	AsyncIOAvailable	-> error "_PC_ASYNC_IO not available"
#endif

#ifdef _PC_PRIO_IO
	PrioIOAvailable		-> (#const _PC_PRIO_IO)
#else
	PrioIOAvailable		-> error "_PC_PRIO_IO not available"
#endif

#if _PC_FILESIZEBITS
	FileSizeBits		-> (#const _PC_FILESIZEBITS)
#else
	FileSizeBits		-> error "_PC_FILESIZEBITS not available"
#endif

#if _PC_SYMLINK_MAX
	SymbolicLinkLimit	-> (#const _PC_SYMLINK_MAX)
#else
	SymbolicLinkLimit	-> error "_PC_SYMLINK_MAX not available"
#endif

-- | @getFdPathVar var fd@ obtains the dynamic value of the requested
-- configurable file limit or option associated with the file or directory
-- attached to the open channel @fd@. For defined file limits, @getFdPathVar@
-- returns the associated value.  For defined file options, the result of
-- @getFdPathVar@ is undefined, but not failure.
--
-- Note: calls @fpathconf@.
getFdPathVar :: Fd -> PathVar -> IO Limit
getFdPathVar (Fd fd) v =
    throwErrnoIfMinus1 "getFdPathVar" $ 
      c_fpathconf fd (pathVarConst v)

foreign import ccall unsafe "fpathconf" 
  c_fpathconf :: CInt -> CInt -> IO CLong
