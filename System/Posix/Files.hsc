{-# OPTIONS -fffi #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Files
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX file support
--
-----------------------------------------------------------------------------

module System.Posix.Files (
    -- * File modes
    -- FileMode exported by System.Posix.Types
    unionFileModes, intersectFileModes,
    nullFileMode,
    ownerReadMode, ownerWriteMode, ownerExecuteMode, ownerModes,
    groupReadMode, groupWriteMode, groupExecuteMode, groupModes,
    otherReadMode, otherWriteMode, otherExecuteMode, otherModes,
    setUserIDMode, setGroupIDMode,
    stdFileMode,   accessModes,

    -- ** Setting file modes
    setFileMode, setFdMode, setFileCreationMask,

    -- ** Checking file existence and permissions
    fileAccess, fileExist,

    -- * File status
    FileStatus,
    -- ** Obtaining file status
    getFileStatus, getFdStatus, getSymbolicLinkStatus,
    -- ** Querying file status
    deviceID, fileID, fileMode, linkCount, fileOwner, fileGroup,
    specialDeviceID, fileSize, accessTime, modificationTime,
    statusChangeTime,
    isBlockDevice, isCharacterDevice, isNamedPipe, isRegularFile,
    isDirectory, isSymbolicLink, isSocket,

    -- * Creation
    createNamedPipe, 
    createDevice,

    -- * Hard links
    createLink, removeLink,

    -- * Symbolic links
    createSymbolicLink, readSymbolicLink,

    -- * Renaming files
    rename,

    -- * Changing file ownership
    setOwnerAndGroup,  setFdOwnerAndGroup,
#if HAVE_LCHOWN
    setSymbolicLinkOwnerAndGroup,
#endif

    -- * Changing file timestamps
    setFileTimes, touchFile,

    -- * Setting file sizes
    setFileSize, setFdSize,

    -- * Find system-specific limits for a file
    PathVar(..), getPathVar, getFdPathVar,
  ) where

#include "HsUnix.h"

import System.Posix.Types
import System.IO.Unsafe
import Data.Bits
import System.Posix.Internals
import Foreign
import Foreign.C

-- -----------------------------------------------------------------------------
-- POSIX file modes

-- The abstract type 'FileMode', constants and operators for
-- manipulating the file modes defined by POSIX.

nullFileMode :: FileMode
nullFileMode = 0

ownerReadMode :: FileMode
ownerReadMode = (#const S_IRUSR)

ownerWriteMode :: FileMode
ownerWriteMode = (#const S_IWUSR)

ownerExecuteMode :: FileMode
ownerExecuteMode = (#const S_IXUSR)

groupReadMode :: FileMode
groupReadMode = (#const S_IRGRP)

groupWriteMode :: FileMode
groupWriteMode = (#const S_IWGRP)

groupExecuteMode :: FileMode
groupExecuteMode = (#const S_IXGRP)

otherReadMode :: FileMode
otherReadMode = (#const S_IROTH)

otherWriteMode :: FileMode
otherWriteMode = (#const S_IWOTH)

otherExecuteMode :: FileMode
otherExecuteMode = (#const S_IXOTH)

setUserIDMode :: FileMode
setUserIDMode = (#const S_ISUID)

setGroupIDMode :: FileMode
setGroupIDMode = (#const S_ISGID)

stdFileMode :: FileMode
stdFileMode = ownerReadMode  .|. ownerWriteMode .|. 
	      groupReadMode  .|. groupWriteMode .|. 
	      otherReadMode  .|. otherWriteMode

ownerModes :: FileMode
ownerModes = (#const S_IRWXU)

groupModes :: FileMode
groupModes = (#const S_IRWXG)

otherModes :: FileMode
otherModes = (#const S_IRWXO)

accessModes :: FileMode
accessModes = ownerModes .|. groupModes .|. otherModes

unionFileModes :: FileMode -> FileMode -> FileMode
unionFileModes m1 m2 = m1 .|. m2

intersectFileModes :: FileMode -> FileMode -> FileMode
intersectFileModes m1 m2 = m1 .&. m2

-- Not exported:
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

setFileMode :: FilePath -> FileMode -> IO ()
setFileMode name m =
  withCString name $ \s -> do
    throwErrnoIfMinus1_ "setFileMode" (c_chmod s m)

setFdMode :: Fd -> FileMode -> IO ()
setFdMode fd m =
  throwErrnoIfMinus1_ "setFdMode" (c_fchmod fd m)

foreign import ccall unsafe "fchmod" 
  c_fchmod :: Fd -> CMode -> IO CInt

setFileCreationMask :: FileMode -> IO FileMode
setFileCreationMask mask = c_umask mask

-- -----------------------------------------------------------------------------
-- access()

fileAccess :: FilePath -> Bool -> Bool -> Bool -> IO Bool
fileAccess name read write exec = access name flags
  where
   flags   = read_f .|. write_f .|. exec_f
   read_f  = if read  then (#const R_OK) else 0
   write_f = if write then (#const W_OK) else 0
   exec_f  = if exec  then (#const X_OK) else 0

fileExist :: FilePath -> IO Bool
fileExist name = 
  withCString name $ \s -> do
    r <- c_access s (#const F_OK)
    if (r == 0)
	then return True
	else do err <- getErrno
	        if (err == eNOENT)
		   then return False
		   else throwErrno "fileExist"

access :: FilePath -> CMode -> IO Bool
access name flags = 
  withCString name $ \s -> do
    r <- c_access s flags
    if (r == 0)
	then return True
	else do err <- getErrno
	        if (err == eACCES)
		   then return False
		   else throwErrno "fileAccess"

-- -----------------------------------------------------------------------------
-- stat() support

newtype FileStatus = FileStatus (ForeignPtr CStat)

deviceID         :: FileStatus -> DeviceID
fileID           :: FileStatus -> FileID
fileMode         :: FileStatus -> FileMode
linkCount        :: FileStatus -> LinkCount
fileOwner        :: FileStatus -> UserID
fileGroup        :: FileStatus -> GroupID
specialDeviceID  :: FileStatus -> DeviceID
fileSize         :: FileStatus -> FileOffset
accessTime       :: FileStatus -> EpochTime
modificationTime :: FileStatus -> EpochTime
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

isBlockDevice     :: FileStatus -> Bool
isCharacterDevice :: FileStatus -> Bool
isNamedPipe       :: FileStatus -> Bool
isRegularFile     :: FileStatus -> Bool
isDirectory       :: FileStatus -> Bool
isSymbolicLink    :: FileStatus -> Bool
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

getFileStatus :: FilePath -> IO FileStatus
getFileStatus path = do
  fp <- mallocForeignPtrBytes (#const sizeof(struct stat)) 
  withForeignPtr fp $ \p ->
    withCString path $ \s -> 
      throwErrnoIfMinus1_ "getFileStatus" (c_stat s p)
  return (FileStatus fp)

getFdStatus :: Fd -> IO FileStatus
getFdStatus (Fd fd) = do
  fp <- mallocForeignPtrBytes (#const sizeof(struct stat)) 
  withForeignPtr fp $ \p ->
    throwErrnoIfMinus1_ "getFdStatus" (c_fstat fd p)
  return (FileStatus fp)

getSymbolicLinkStatus :: FilePath -> IO FileStatus
getSymbolicLinkStatus path = do
  fp <- mallocForeignPtrBytes (#const sizeof(struct stat)) 
  withForeignPtr fp $ \p ->
    withCString path $ \s -> 
      throwErrnoIfMinus1_ "getSymbolicLinkStatus" (c_lstat s p)
  return (FileStatus fp)

foreign import ccall unsafe "lstat" 
  c_lstat :: CString -> Ptr CStat -> IO CInt

createNamedPipe :: FilePath -> FileMode -> IO ()
createNamedPipe name mode = do
  withCString name $ \s -> 
    throwErrnoIfMinus1_ "createNamedPipe" (c_mkfifo s mode)

createDevice :: FilePath -> FileMode -> DeviceID -> IO ()
createDevice path mode dev =
  withCString path $ \s ->
    throwErrnoIfMinus1_ "createDevice" (c_mknod s mode dev)

foreign import ccall unsafe "mknod" 
  c_mknod :: CString -> CMode -> CDev -> IO CInt

-- -----------------------------------------------------------------------------
-- Hard links

createLink :: FilePath -> FilePath -> IO ()
createLink name1 name2 =
  withCString name1 $ \s1 ->
  withCString name2 $ \s2 ->
  throwErrnoIfMinus1_ "createLink" (c_link s1 s2)

removeLink :: FilePath -> IO ()
removeLink name =
  withCString name $ \s ->
  throwErrnoIfMinus1_ "removeLink" (c_unlink s)

-- -----------------------------------------------------------------------------
-- Symbolic Links

createSymbolicLink :: FilePath -> FilePath -> IO ()
createSymbolicLink file1 file2 =
  withCString file1 $ \s1 ->
  withCString file2 $ \s2 ->
  throwErrnoIfMinus1_ "createSymbolicLink" (c_symlink s1 s2)

foreign import ccall unsafe "symlink"
  c_symlink :: CString -> CString -> IO CInt

-- ToDo: should really use SYMLINK_MAX, but not everyone supports it yet,
-- and it seems that the intention is that SYMLINK_MAX is no larger than
-- PATH_MAX.
readSymbolicLink :: FilePath -> IO FilePath
readSymbolicLink file =
  allocaArray0 (#const PATH_MAX) $ \buf -> do
    withCString file $ \s -> do
      len <- throwErrnoIfMinus1 "readSymbolicLink" $ 
	c_readlink s buf (#const PATH_MAX)
      peekCStringLen (buf,fromIntegral len)

foreign import ccall unsafe "readlink"
  c_readlink :: CString -> CString -> CInt -> IO CInt

-- -----------------------------------------------------------------------------
-- Renaming files

rename :: FilePath -> FilePath -> IO ()
rename name1 name2 =
  withCString name1 $ \s1 ->
  withCString name2 $ \s2 ->
  throwErrnoIfMinus1_ "rename" (c_rename s1 s2)

-- -----------------------------------------------------------------------------
-- chmod()

setOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO ()
setOwnerAndGroup name uid gid = do
  withCString name $ \s ->
    throwErrnoIfMinus1_ "setOwnerAndGroup" (c_chown s uid gid)

foreign import ccall unsafe "chown"
  c_chown :: CString -> CUid -> CGid -> IO CInt

setFdOwnerAndGroup :: Fd -> UserID -> GroupID -> IO ()
setFdOwnerAndGroup (Fd fd) uid gid = 
  throwErrnoIfMinus1_ "setFdOwnerAndGroup" (c_fchown fd uid gid)

foreign import ccall unsafe "fchown"
  c_fchown :: CInt -> CUid -> CGid -> IO CInt

#if HAVE_LCHOWN
setSymbolicLinkOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO ()
setSymbolicLinkOwnerAndGroup name uid gid = do
  withCString name $ \s ->
    throwErrnoIfMinus1_ "setSymbolicLinkOwnerAndGroup" (c_lchown s uid gid)

foreign import ccall unsafe "lchown"
  c_lchown :: CString -> CUid -> CGid -> IO CInt
#endif

-- -----------------------------------------------------------------------------
-- utime()

setFileTimes :: FilePath -> EpochTime -> EpochTime -> IO ()
setFileTimes name atime mtime = do
  withCString name $ \s ->
   allocaBytes (#const sizeof(struct utimbuf)) $ \p -> do
     (#poke struct utimbuf, actime)  p atime
     (#poke struct utimbuf, modtime) p mtime
     throwErrnoIfMinus1_ "setFileTimes" (c_utime s p)

touchFile :: FilePath -> IO ()
touchFile name = do
  withCString name $ \s ->
   throwErrnoIfMinus1_ "touchFile" (c_utime s nullPtr)

-- -----------------------------------------------------------------------------
-- Setting file sizes

setFileSize :: FilePath -> FileOffset -> IO ()
setFileSize file off = 
  withCString file $ \s ->
    throwErrnoIfMinus1_ "setFileSize" (c_truncate s off)

foreign import ccall unsafe "truncate"
  c_truncate :: CString -> COff -> IO CInt

setFdSize :: Fd -> FileOffset -> IO ()
setFdSize fd off =
  throwErrnoIfMinus1_ "setFdSize" (c_ftruncate fd off)

foreign import ccall unsafe "ftruncate"
  c_ftruncate :: Fd -> COff -> IO CInt

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

getPathVar :: FilePath -> PathVar -> IO Limit
getPathVar name v = do
  withCString name $ \ nameP -> 
    throwErrnoIfMinus1 "getPathVar" $ 
      c_pathconf nameP (pathVarConst v)

foreign import ccall unsafe "pathconf" 
  c_pathconf :: CString -> CInt -> IO CLong

getFdPathVar :: Fd -> PathVar -> IO Limit
getFdPathVar fd v =
    throwErrnoIfMinus1 "getFdPathVar" $ 
      c_fpathconf fd (pathVarConst v)

foreign import ccall unsafe "fpathconf" 
  c_fpathconf :: Fd -> CInt -> IO CLong
