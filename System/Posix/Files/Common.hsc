{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumDecimals #-}

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
    accessTimeHiRes, modificationTimeHiRes, statusChangeTimeHiRes,
    setFdTimesHiRes, touchFd,
    isBlockDevice, isCharacterDevice, isNamedPipe, isRegularFile,
    isDirectory, isSymbolicLink, isSocket,

    fileBlockSize,
    fileBlocks,

    -- * Extended file status
    StatxFlags(..),
    pattern EmptyPath,
    pattern NoAutoMount,
    pattern SymlinkNoFollow,
    pattern SyncAsStat,
    pattern ForceSync,
    pattern DontSync,
    defaultStatxFlags,
    StatxMask(..),
    pattern StatxType,
    pattern StatxMode,
    pattern StatxNlink,
    pattern StatxUid,
    pattern StatxGid,
    pattern StatxAtime,
    pattern StatxMtime,
    pattern StatxCtime,
    pattern StatxBtime,
    pattern StatxIno,
    pattern StatxSize,
    pattern StatxBlocks,
    pattern StatxMntId,
    pattern StatxBasicStats,
    pattern StatxAll,
    defaultStatxMask,
    ExtendedFileStatus(..),
    CAttributes(..),
    getExtendedFileStatus_,
    fileBlockSizeX,
    linkCountX,
    fileOwnerX,
    fileGroupX,
    fileModeX,
    fileIDX,
    fileSizeX,
    fileBlocksX,
    accessTimeHiResX,
    creationTimeHiResX,
    statusChangeTimeHiResX,
    modificationTimeHiResX,
    deviceIDX,
    specialDeviceIDX,
    mountIDX,
    fileCompressedX,
    fileImmutableX,
    fileAppendX,
    fileNoDumpX,
    fileEncryptedX,
    fileVerityX,
    fileDaxX,
    isBlockDeviceX,
    isCharacterDeviceX,
    isNamedPipeX,
    isRegularFileX,
    isDirectoryX,
    isSymbolicLinkX,
    isSocketX,
    haveStatx,

    -- * Setting file sizes
    setFdSize,

    -- * Changing file ownership
    setFdOwnerAndGroup,

    -- * Find system-specific limits for a file
    PathVar(..), getFdPathVar, pathVarConst,

    -- * Low level types and functions
#ifdef HAVE_UTIMENSAT
    CTimeSpec(..),
    toCTimeSpec,
    c_utimensat,
#endif
#if defined(javascript_HOST_ARCH)
    js_futimes,
    js_utimes,
    js_lutimes,
#endif
    CTimeVal(..),
    toCTimeVal,
    c_utimes,
#ifdef HAVE_LUTIMES
    c_lutimes,
#endif
  ) where

import System.Posix.Types
import System.IO.Unsafe
import Data.Bits
import Data.Int
import Data.Ratio
import Data.Word
#ifdef HAVE_STATX
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Fixed (Fixed(..))
#endif
import Data.Time.Clock.POSIX (POSIXTime)
import System.Posix.Internals
import Foreign.C
import Foreign.ForeignPtr
#if defined(HAVE_FUTIMES) || defined(HAVE_FUTIMENS)
import Foreign.Marshal (withArray)
#endif
import Foreign.Ptr
import Foreign.Storable

#if !defined(HAVE_FCHMOD) || !defined(HAVE_CHOWN) || !defined(HAVE_STATX)
import System.IO.Error ( ioeSetLocation )
import GHC.IO.Exception ( unsupportedOperation )
#endif

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

#if defined(HAVE_FCHMOD)

-- | @setFdMode fd mode@ acts like 'setFileMode' but uses a file descriptor
-- @fd@ instead of a 'FilePath'.
--
-- Note: calls @fchmod@.
setFdMode :: Fd -> FileMode -> IO ()
setFdMode (Fd fd) m =
  throwErrnoIfMinus1_ "setFdMode" (c_fchmod fd m)

foreign import ccall unsafe "fchmod"
  c_fchmod :: CInt -> CMode -> IO CInt

#else

{-# WARNING setFdMode "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_FCHMOD@)" #-}
setFdMode :: Fd -> FileMode -> IO ()
setFdMode _ _ = ioError (ioeSetLocation unsupportedOperation "setFdMode")

#endif // HAVE_FCHMOD

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
--
-- Limitations: Support for high resolution timestamps is filesystem dependent:
--
-- - HFS+ volumes on OS X only support whole-second times.
--
newtype FileStatus = FileStatus (ForeignPtr CStat) -- ^ The constructor is considered internal and may change.

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
-- | Number of blocks allocated for this file, in units of
-- 512-bytes. Returns @Nothing@ if @st_blocks@ is not supported on this
-- platform.
fileBlocks       :: FileStatus -> Maybe CBlkCnt
-- | Gives the preferred block size for efficient filesystem I/O in
-- bytes. Returns @Nothing@ if @st_blocksize@ is not supported on this
-- platform.
fileBlockSize    :: FileStatus -> Maybe CBlkSize
-- | Time of last access.
accessTime       :: FileStatus -> EpochTime
-- | Time of last access in sub-second resolution. Depends on the timestamp resolution of the
-- underlying filesystem.
accessTimeHiRes  :: FileStatus -> POSIXTime
-- | Time of last modification.
modificationTime :: FileStatus -> EpochTime
-- | Time of last modification in sub-second resolution. Depends on the timestamp resolution of the
-- underlying filesystem.
modificationTimeHiRes :: FileStatus -> POSIXTime
-- | Time of last status change (i.e. owner, group, link count, mode, etc.).
statusChangeTime :: FileStatus -> EpochTime
-- | Time of last status change (i.e. owner, group, link count, mode, etc.) in sub-second resolution.
-- Depends on the timestamp resolution of the underlying filesystem.
statusChangeTimeHiRes :: FileStatus -> POSIXTime

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

#ifdef HAVE_STRUCT_STAT_ST_BLOCKS
fileBlocks (FileStatus stat) =
  Just $ unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_blocks)
#else
fileBlocks _ = Nothing
#endif
#ifdef HAVE_STRUCT_STAT_ST_BLKSIZE
fileBlockSize (FileStatus stat) =
  Just $ unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_blksize)
#else
fileBlockSize _ = Nothing
#endif

accessTimeHiRes (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ \stat_ptr -> do
    sec  <- (#peek struct stat, st_atime) stat_ptr :: IO EpochTime
#ifdef HAVE_STRUCT_STAT_ST_ATIM
    nsec <- (#peek struct stat, st_atim.tv_nsec) stat_ptr :: IO (#type long)
    let frac = toInteger nsec % 10^(9::Int)
#elif HAVE_STRUCT_STAT_ST_ATIMESPEC
    nsec <- (#peek struct stat, st_atimespec.tv_nsec) stat_ptr :: IO (#type long)
    let frac = toInteger nsec % 10^(9::Int)
#elif HAVE_STRUCT_STAT_ST_ATIMENSEC
    nsec <- (#peek struct stat, st_atimensec) stat_ptr :: IO (#type long)
    let frac = toInteger nsec % 10^(9::Int)
#elif HAVE_STRUCT_STAT_ST_ATIME_N
    nsec <- (#peek struct stat, st_atime_n) stat_ptr :: IO (#type int)
    let frac = toInteger nsec % 10^(9::Int)
#elif HAVE_STRUCT_STAT_ST_UATIME
    usec <- (#peek struct stat, st_uatime) stat_ptr :: IO (#type int)
    let frac = toInteger usec % 10^(6::Int)
#else
    let frac = 0
#endif
    return $ fromRational $ toRational sec + frac

modificationTimeHiRes (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ \stat_ptr -> do
    sec  <- (#peek struct stat, st_mtime) stat_ptr :: IO EpochTime
#ifdef HAVE_STRUCT_STAT_ST_MTIM
    nsec <- (#peek struct stat, st_mtim.tv_nsec) stat_ptr :: IO (#type long)
    let frac = toInteger nsec % 10^(9::Int)
#elif HAVE_STRUCT_STAT_ST_MTIMESPEC
    nsec <- (#peek struct stat, st_mtimespec.tv_nsec) stat_ptr :: IO (#type long)
    let frac = toInteger nsec % 10^(9::Int)
#elif HAVE_STRUCT_STAT_ST_MTIMENSEC
    nsec <- (#peek struct stat, st_mtimensec) stat_ptr :: IO (#type long)
    let frac = toInteger nsec % 10^(9::Int)
#elif HAVE_STRUCT_STAT_ST_MTIME_N
    nsec <- (#peek struct stat, st_mtime_n) stat_ptr :: IO (#type int)
    let frac = toInteger nsec % 10^(9::Int)
#elif HAVE_STRUCT_STAT_ST_UMTIME
    usec <- (#peek struct stat, st_umtime) stat_ptr :: IO (#type int)
    let frac = toInteger usec % 10^(6::Int)
#else
    let frac = 0
#endif
    return $ fromRational $ toRational sec + frac

statusChangeTimeHiRes (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ \stat_ptr -> do
    sec  <- (#peek struct stat, st_ctime) stat_ptr :: IO EpochTime
#ifdef HAVE_STRUCT_STAT_ST_CTIM
    nsec <- (#peek struct stat, st_ctim.tv_nsec) stat_ptr :: IO (#type long)
    let frac = toInteger nsec % 10^(9::Int)
#elif HAVE_STRUCT_STAT_ST_CTIMESPEC
    nsec <- (#peek struct stat, st_ctimespec.tv_nsec) stat_ptr :: IO (#type long)
    let frac = toInteger nsec % 10^(9::Int)
#elif HAVE_STRUCT_STAT_ST_CTIMENSEC
    nsec <- (#peek struct stat, st_ctimensec) stat_ptr :: IO (#type long)
    let frac = toInteger nsec % 10^(9::Int)
#elif HAVE_STRUCT_STAT_ST_CTIME_N
    nsec <- (#peek struct stat, st_ctime_n) stat_ptr :: IO (#type int)
    let frac = toInteger nsec % 10^(9::Int)
#elif HAVE_STRUCT_STAT_ST_UCTIME
    usec <- (#peek struct stat, st_uctime) stat_ptr :: IO (#type int)
    let frac = toInteger usec % 10^(6::Int)
#else
    let frac = 0
#endif
    return $ fromRational $ toRational sec + frac

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
-- Setting file times

#if HAVE_UTIMENSAT || HAVE_FUTIMENS
data CTimeSpec = CTimeSpec EpochTime CLong

instance Storable CTimeSpec where
    sizeOf    _ = #size struct timespec
    alignment _ = alignment (undefined :: CInt)
    poke p (CTimeSpec sec nsec) = do
        (#poke struct timespec, tv_sec ) p sec
        (#poke struct timespec, tv_nsec) p nsec
    peek p = do
        sec  <- #{peek struct timespec, tv_sec } p
        nsec <- #{peek struct timespec, tv_nsec} p
        return $ CTimeSpec sec nsec

toCTimeSpec :: POSIXTime -> CTimeSpec
toCTimeSpec t = CTimeSpec (CTime sec) (truncate $ 10^(9::Int) * frac)
  where
    (sec, frac) = if (frac' < 0) then (sec' - 1, frac' + 1) else (sec', frac')
    (sec', frac') = properFraction $ toRational t
#endif

#ifdef HAVE_UTIMENSAT
foreign import capi unsafe "sys/stat.h utimensat"
    c_utimensat :: CInt -> CString -> Ptr CTimeSpec -> CInt -> IO CInt
#endif

#if HAVE_FUTIMENS
foreign import capi unsafe "sys/stat.h futimens"
    c_futimens :: CInt -> Ptr CTimeSpec -> IO CInt
#endif

data CTimeVal = CTimeVal CLong CLong

instance Storable CTimeVal where
    sizeOf    _ = #size struct timeval
    alignment _ = alignment (undefined :: CInt)
    poke p (CTimeVal sec usec) = do
        (#poke struct timeval, tv_sec ) p sec
        (#poke struct timeval, tv_usec) p usec
    peek p = do
        sec  <- #{peek struct timeval, tv_sec } p
        usec <- #{peek struct timeval, tv_usec} p
        return $ CTimeVal sec usec

toCTimeVal :: POSIXTime -> CTimeVal
toCTimeVal t = CTimeVal sec (truncate $ 10^(6::Int) * frac)
  where
    (sec, frac) = if (frac' < 0) then (sec' - 1, frac' + 1) else (sec', frac')
    (sec', frac') = properFraction $ toRational t

foreign import capi unsafe "sys/time.h utimes"
    c_utimes :: CString -> Ptr CTimeVal -> IO CInt

#ifdef HAVE_LUTIMES
foreign import capi unsafe "sys/time.h lutimes"
    c_lutimes :: CString -> Ptr CTimeVal -> IO CInt
#endif

#if HAVE_FUTIMES
foreign import capi unsafe "sys/time.h futimes"
    c_futimes :: CInt -> Ptr CTimeVal -> IO CInt
#endif

#if defined(javascript_HOST_ARCH)
foreign import ccall unsafe "js_futimes"
    js_futimes :: CInt -> CDouble -> CDouble -> IO CInt
foreign import ccall unsafe "js_lutimes"
    js_lutimes :: CFilePath -> CDouble -> CDouble -> IO CInt
foreign import ccall unsafe "js_utimes"
    js_utimes :: CFilePath -> CDouble -> CDouble -> IO CInt
#endif

-- | Like 'setFileTimesHiRes' but uses a file descriptor instead of a path.
-- This operation is not supported on all platforms. On these platforms,
-- this function will raise an exception.
--
-- Note: calls @futimens@ or @futimes@. Support for high resolution timestamps
--   is filesystem dependent with the following limitations:
--
-- - HFS+ volumes on OS X truncate the sub-second part of the timestamp.
--
--
-- @since 2.7.0.0
setFdTimesHiRes :: Fd -> POSIXTime -> POSIXTime -> IO ()
#if defined(javascript_HOST_ARCH)
setFdTimesHiRes (Fd fd) atime mtime =
  throwErrnoIfMinus1_ "setFdTimesHiRes" (js_futimes fd (realToFrac atime) (realToFrac mtime))
#elif HAVE_FUTIMENS
setFdTimesHiRes (Fd fd) atime mtime =
  withArray [toCTimeSpec atime, toCTimeSpec mtime] $ \times ->
    throwErrnoIfMinus1_ "setFdTimesHiRes" (c_futimens fd times)
#elif HAVE_FUTIMES
setFdTimesHiRes (Fd fd) atime mtime =
  withArray [toCTimeVal atime, toCTimeVal mtime] $ \times ->
    throwErrnoIfMinus1_ "setFdTimesHiRes" (c_futimes fd times)
#else
{-# WARNING setFdTimesHiRes "setFdTimesHiRes: not available on this platform" #-}
setFdTimesHiRes =
  error "setSymbolicLinkTimesHiRes: not available on this platform"
#endif

-- | Like 'touchFile' but uses a file descriptor instead of a path.
-- This operation is not supported on all platforms. On these platforms,
-- this function will raise an exception.
--
-- Note: calls @futimes@.
--
-- @since 2.7.0.0
touchFd :: Fd -> IO ()
#if defined(javascript_HOST_ARCH)
touchFd (Fd fd) =
  -- (-1) indicates that current time must be used
  throwErrnoIfMinus1_ "touchFd" (js_futimes fd (-1) (-1))
#elif HAVE_FUTIMES
touchFd (Fd fd) =
  throwErrnoIfMinus1_ "touchFd" (c_futimes fd nullPtr)
#else
{-# WARNING touchFd "touchFd: not available on this platform" #-}
touchFd =
  error "touchFd: not available on this platform"
#endif

-- -----------------------------------------------------------------------------
-- fchown()

#if defined(HAVE_CHOWN)

-- | Acts as 'setOwnerAndGroup' but uses a file descriptor instead of a
-- 'FilePath'.
--
-- Note: calls @fchown@.
setFdOwnerAndGroup :: Fd -> UserID -> GroupID -> IO ()
setFdOwnerAndGroup (Fd fd) uid gid =
  throwErrnoIfMinus1_ "setFdOwnerAndGroup" (c_fchown fd uid gid)

foreign import ccall unsafe "fchown"
  c_fchown :: CInt -> CUid -> CGid -> IO CInt

#else

{-# WARNING setFdOwnerAndGroup "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_CHOWN@)" #-}
setFdOwnerAndGroup :: Fd -> UserID -> GroupID -> IO ()
setFdOwnerAndGroup _ _ _ = ioError (ioeSetLocation unsupportedOperation "setFdOwnerAndGroup")

#endif // HAVE_CHOWN

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
  = FileSizeBits                  {- _PC_FILESIZEBITS     -}
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
  | SymbolicLinkLimit             {- _PC_SYMLINK_MAX      -}
  | SetOwnerAndGroupIsRestricted  {- _PC_CHOWN_RESTRICTED -}
  | FileNamesAreNotTruncated      {- _PC_NO_TRUNC         -}
  | VDisableChar                  {- _PC_VDISABLE         -}
  | AsyncIOAvailable              {- _PC_ASYNC_IO         -}
  | PrioIOAvailable               {- _PC_PRIO_IO          -}
  | SyncIOAvailable               {- _PC_SYNC_IO          -}

pathVarConst :: PathVar -> CInt
pathVarConst v = case v of
        LinkLimit                       -> (#const _PC_LINK_MAX)
        InputLineLimit                  -> (#const _PC_MAX_CANON)
        InputQueueLimit                 -> (#const _PC_MAX_INPUT)
        FileNameLimit                   -> (#const _PC_NAME_MAX)
        PathNameLimit                   -> (#const _PC_PATH_MAX)
        PipeBufferLimit                 -> (#const _PC_PIPE_BUF)
        SetOwnerAndGroupIsRestricted    -> (#const _PC_CHOWN_RESTRICTED)
        FileNamesAreNotTruncated        -> (#const _PC_NO_TRUNC)
        VDisableChar                    -> (#const _PC_VDISABLE)

#ifdef _PC_SYNC_IO
        SyncIOAvailable         -> (#const _PC_SYNC_IO)
#else
        SyncIOAvailable         -> error "_PC_SYNC_IO not available"
#endif

#ifdef _PC_ASYNC_IO
        AsyncIOAvailable        -> (#const _PC_ASYNC_IO)
#else
        AsyncIOAvailable        -> error "_PC_ASYNC_IO not available"
#endif

#ifdef _PC_PRIO_IO
        PrioIOAvailable         -> (#const _PC_PRIO_IO)
#else
        PrioIOAvailable         -> error "_PC_PRIO_IO not available"
#endif

#if _PC_FILESIZEBITS
        FileSizeBits            -> (#const _PC_FILESIZEBITS)
#else
        FileSizeBits            -> error "_PC_FILESIZEBITS not available"
#endif

#if _PC_SYMLINK_MAX
        SymbolicLinkLimit       -> (#const _PC_SYMLINK_MAX)
#else
        SymbolicLinkLimit       -> error "_PC_SYMLINK_MAX not available"
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


-- -----------------------------------------------------------------------------
-- statx
--

newtype {-# CTYPE "__u64" #-} CAttributes = CAttributes Word64
 deriving (Read, Show, Eq, Ord, Storable, Num, Bits)

-- | Statx flags.
--
-- See the pattern synonyms for possible flags. These are combined via `(<>)`.
-- Flags can be tested via `(.&.)`.
--
-- The following flags influence pathname-based lookup:
--
-- - 'EmptyPath'
-- - 'NoAutoMount'
-- - 'SymlinkNoFollow'
--
-- The following flags can be used to control what sort of synchronization the kernel will do when querying a file on a remote filesystem:
--
-- - 'SyncAsStat'
-- - 'ForceSync'
-- - 'DontSync'
newtype StatxFlags = StatxFlags CInt deriving (Read, Show, Eq, Ord, Integral, Num, Enum, Bits, Real)

-- | ORs the flags.
instance Semigroup StatxFlags where
   a <> b = a .|. b

instance Monoid StatxFlags where
  mappend = (<>)
  mempty = 0

-- | If pathname to 'getExtendedFileStatus' is an empty string, operate on the file referred to by
-- the 'Maybe Fd' argument.
--
-- In this case, it can refer to any type of file, not just a directory.
pattern EmptyPath :: StatxFlags
#ifdef AT_EMPTY_PATH
pattern EmptyPath = StatxFlags (#const AT_EMPTY_PATH)
#else
pattern EmptyPath = StatxFlags 0
#endif

-- | Don't automount the terminal ("basename") component of pathname if it is a directory that is an automount point.
-- This allows the caller to gather attributes of an automount point (rather than the location it would mount).
-- This flag can be used in tools that scan directories to prevent mass-automounting of a directory of automount points.
-- This flag has no effect if the mount point has already been mounted over.
pattern NoAutoMount :: StatxFlags
#ifdef AT_NO_AUTOMOUNT
pattern NoAutoMount = StatxFlags (#const AT_NO_AUTOMOUNT)
#else
pattern NoAutoMount = StatxFlags 0
#endif

-- | If pathname is a symbolic link, do not dereference it: instead return information about the link itself, like @lstat(2)@.
pattern SymlinkNoFollow :: StatxFlags
#ifdef AT_SYMLINK_NOFOLLOW
pattern SymlinkNoFollow = StatxFlags (#const AT_SYMLINK_NOFOLLOW)
#else
pattern SymlinkNoFollow = StatxFlags 0
#endif

-- | Do whatever @stat(2)@ does. This is the default and is very much filesystem-specific.
pattern SyncAsStat :: StatxFlags
#ifdef AT_STATX_SYNC_AS_STAT
pattern SyncAsStat = StatxFlags (#const AT_STATX_SYNC_AS_STAT)
#else
pattern SyncAsStat = StatxFlags 0
#endif

-- | Force the attributes to be synchronized with the server.
-- This may require that a network filesystem perform a data writeback to get the timestamps correct.
pattern ForceSync :: StatxFlags
#ifdef AT_STATX_FORCE_SYNC
pattern ForceSync = StatxFlags (#const AT_STATX_FORCE_SYNC)
#else
pattern ForceSync = StatxFlags 0
#endif

-- | Don't synchronize anything, but rather just take whatever the system has cached if possible.
-- This may mean that the information returned is approximate, but, on a network filesystem,
-- it may not involve a round trip to the server - even if no lease is held.
pattern DontSync :: StatxFlags
#ifdef AT_STATX_DONT_SYNC
pattern DontSync = StatxFlags (#const AT_STATX_DONT_SYNC)
#else
pattern DontSync = StatxFlags 0
#endif

defaultStatxFlags :: StatxFlags
defaultStatxFlags = mempty

-- | Mask argument to 'statx'. It's used to tell the kernel which fields the caller is interested in.
--
-- See the pattern synonyms for possible masks. These are combined via @(<>)@.
-- Masks can be tested via `(.&.)`.
newtype StatxMask = StatxMask CInt deriving (Read, Show, Eq, Ord, Integral, Num, Enum, Bits, Real)

-- | ORs the masks.
instance Semigroup StatxMask where
   a <> b = a .|. b

instance Monoid StatxMask where
  mappend = (<>)
  mempty = 0

-- | Want @stx_mode & S_IFMT@.
pattern StatxType :: StatxMask
#ifdef STATX_TYPE
pattern StatxType = StatxMask (#const STATX_TYPE)
#else
pattern StatxType = StatxMask 0
#endif

-- | Want @stx_mode & ~S_IFMT@.
pattern StatxMode :: StatxMask
#ifdef STATX_MODE
pattern StatxMode = StatxMask (#const STATX_MODE)
#else
pattern StatxMode = StatxMask 0
#endif

-- | Want @stx_nlink@.
pattern StatxNlink :: StatxMask
#ifdef STATX_NLINK
pattern StatxNlink = StatxMask (#const STATX_NLINK)
#else
pattern StatxNlink = StatxMask 0
#endif

-- | Want @stx_uid@.
pattern StatxUid :: StatxMask
#ifdef STATX_UID
pattern StatxUid = StatxMask (#const STATX_UID)
#else
pattern StatxUid = StatxMask 0
#endif

-- | Want @stx_gid@.
pattern StatxGid :: StatxMask
#ifdef STATX_GID
pattern StatxGid = StatxMask (#const STATX_GID)
#else
pattern StatxGid = StatxMask 0
#endif

-- | Want @stx_atime@.
pattern StatxAtime :: StatxMask
#ifdef STATX_ATIME
pattern StatxAtime = StatxMask (#const STATX_ATIME)
#else
pattern StatxAtime = StatxMask 0
#endif

-- | Want @stx_mtime@.
pattern StatxMtime :: StatxMask
#ifdef STATX_MTIME
pattern StatxMtime = StatxMask (#const STATX_MTIME)
#else
pattern StatxMtime = StatxMask 0
#endif

-- | Want @stx_ctime@.
pattern StatxCtime :: StatxMask
#ifdef STATX_CTIME
pattern StatxCtime = StatxMask (#const STATX_CTIME)
#else
pattern StatxCtime = StatxMask 0
#endif

-- | Want @stx_btime@.
pattern StatxBtime :: StatxMask
#ifdef STATX_BTIME
pattern StatxBtime = StatxMask (#const STATX_BTIME)
#else
pattern StatxBtime = StatxMask 0
#endif

-- | Want @stx_mnt_id@.
pattern StatxMntId :: StatxMask
#ifdef HAVE_STATX_MNT_ID
pattern StatxMntId = StatxMask (#const STATX_MNT_ID)
#else
pattern StatxMntId = StatxMask 0
#endif

-- | Want @stx_ino@.
pattern StatxIno :: StatxMask
#ifdef STATX_INO
pattern StatxIno = StatxMask (#const STATX_INO)
#else
pattern StatxIno = StatxMask 0
#endif

-- | Want @stx_size@.
pattern StatxSize :: StatxMask
#ifdef STATX_SIZE
pattern StatxSize = StatxMask (#const STATX_SIZE)
#else
pattern StatxSize = StatxMask 0
#endif

-- | Want @stx_blocks@.
pattern StatxBlocks :: StatxMask
#ifdef STATX_BLOCKS
pattern StatxBlocks = StatxMask (#const STATX_BLOCKS)
#else
pattern StatxBlocks = StatxMask 0
#endif

-- | Want all of the above.
pattern StatxBasicStats :: StatxMask
#ifdef STATX_BASIC_STATS
pattern StatxBasicStats = StatxMask (#const STATX_BASIC_STATS)
#else
pattern StatxBasicStats = StatxMask 0
#endif

-- | Want all currently available fields.
pattern StatxAll :: StatxMask
#ifdef STATX_ALL
pattern StatxAll = StatxMask (#const STATX_ALL)
#else
pattern StatxAll = StatxMask 0
#endif


defaultStatxMask :: StatxMask
defaultStatxMask = mempty

newtype ExtendedFileStatus = ExtendedFileStatus (ForeignPtr CStatx) -- ^ The constructor is considered internal and may change.

-- | The "preferred" block size for efficient filesystem I/O.
-- (Writing to a file in smaller chunks may cause an inefficient read-mod‐ify-rewrite.)
fileBlockSizeX             :: ExtendedFileStatus -> CBlkSize
#if HAVE_STATX
-- | Further status information about the file.
fileAttributesX            :: ExtendedFileStatus -> CAttributes
#endif
-- | The number of hard links on a file.
linkCountX                 :: ExtendedFileStatus -> CNlink
-- | Te user ID of the owner of the file.
fileOwnerX                 :: ExtendedFileStatus -> UserID
-- | The ID of the group owner of the file.
fileGroupX                 :: ExtendedFileStatus -> GroupID
-- | The file type and mode.  See @inode(7)@ for details.
fileModeX                  :: ExtendedFileStatus -> FileMode
-- | The inode number of the file.
fileIDX                    :: ExtendedFileStatus -> FileID
-- | The size of the file (if it is a regular file or a symbolic link) in bytes.
-- The size of a symbolic link is the length of the pathname it contains,
-- without a terminating null byte.
fileSizeX                  :: ExtendedFileStatus -> Word64
-- | The  number of blocks allocated to the file on the medium, in 512-byte units.
-- (This may be smaller than stx_size/512 when the file has holes.)
fileBlocksX                :: ExtendedFileStatus -> Word64
#if HAVE_STATX
-- | A mask indicating which bits in 'fileAttributesX' are supported by the VFS and the filesystem.
fileAttributesMaskX        :: ExtendedFileStatus -> CAttributes
#endif
-- | The file's last access timestamp.
accessTimeHiResX           :: ExtendedFileStatus -> POSIXTime
-- | The file's creation timestamp.
creationTimeHiResX         :: ExtendedFileStatus -> POSIXTime
-- | The file's last status change timestamp.
statusChangeTimeHiResX     :: ExtendedFileStatus -> POSIXTime
-- | The file's last modification timestamp.
modificationTimeHiResX     :: ExtendedFileStatus -> POSIXTime
-- | ID of the device on which this file resides.
deviceIDX                  :: ExtendedFileStatus -> DeviceID
-- | Describes the device that this file represents.
specialDeviceIDX       :: ExtendedFileStatus -> DeviceID
-- | The mount ID of the mount containing the file. This is the same number
-- reported by name_to_handle_at(2) and corresponds to the number in the
-- first field in one of the records in /proc/self/mountinfo.
mountIDX               :: ExtendedFileStatus -> Word64
-- | The file is compressed by the filesystem and may take extra resources to access.
-- This is an extended attribute.
fileCompressedX            :: ExtendedFileStatus -> Bool
-- | The file cannot be modified: it cannot be deleted or renamed, no hard links can
-- be created to this file and no data can be written to it. See @chattr(1)@.
-- This is an extended attribute.
fileImmutableX             :: ExtendedFileStatus -> Bool
-- | The file can only be opened in append mode for writing. Random access writing is not permitted. See @chattr(1)@.
-- This is an extended attribute.
fileAppendX                :: ExtendedFileStatus -> Bool
-- | File is not a candidate for backup when a backup program such as @dump(8)@ is run. See @chattr(1)@.
-- This is an extended attribute.
fileNoDumpX                :: ExtendedFileStatus -> Bool
-- | A key is required for the file to be encrypted by the filesystem.
-- This is an extended attribute.
fileEncryptedX             :: ExtendedFileStatus -> Bool
-- | The file has fs-verity enabled.  It cannot be written to, and all reads from it
-- will be verified  against a cryptographic hash that covers the entire file (e.g., via a Merkle tree).
-- This is an extended attribute.
-- Since Linux 5.5.
fileVerityX                :: ExtendedFileStatus -> Bool
-- | The  file is in the DAX (cpu direct access) state.
-- This is an extended attribute.
-- Since Linux 5.8.
fileDaxX                   :: ExtendedFileStatus -> Bool

-- | Checks if this file is a block device.
isBlockDeviceX     :: ExtendedFileStatus -> Bool
-- | Checks if this file is a character device.
isCharacterDeviceX :: ExtendedFileStatus -> Bool
-- | Checks if this file is a named pipe device.
isNamedPipeX       :: ExtendedFileStatus -> Bool
-- | Checks if this file is a regular file device.
isRegularFileX     :: ExtendedFileStatus -> Bool
-- | Checks if this file is a directory device.
isDirectoryX       :: ExtendedFileStatus -> Bool
-- | Checks if this file is a symbolic link device.
isSymbolicLinkX    :: ExtendedFileStatus -> Bool
-- | Checks if this file is a socket device.
isSocketX          :: ExtendedFileStatus -> Bool

isBlockDeviceX statx =
  (fileModeX statx `intersectFileModes` fileTypeModes) == blockSpecialMode
isCharacterDeviceX statx =
  (fileModeX statx `intersectFileModes` fileTypeModes) == characterSpecialMode
isNamedPipeX statx =
  (fileModeX statx `intersectFileModes` fileTypeModes) == namedPipeMode
isRegularFileX statx =
  (fileModeX statx `intersectFileModes` fileTypeModes) == regularFileMode
isDirectoryX statx =
  (fileModeX statx `intersectFileModes` fileTypeModes) == directoryMode
isSymbolicLinkX statx =
  (fileModeX statx `intersectFileModes` fileTypeModes) == symbolicLinkMode
isSocketX statx =
  (fileModeX statx `intersectFileModes` fileTypeModes) == socketMode

#if HAVE_STATX
testFlag :: ExtendedFileStatus -> CAttributes -> Bool
testFlag ex flag =
  let attributes = fileAttributesX ex
      attributes_mask = fileAttributesMaskX ex
  in (attributes .&. attributes_mask .&. flag) /= 0

#ifdef STATX_ATTR_COMPRESSED
fileCompressedX ex = testFlag ex (#const STATX_ATTR_COMPRESSED)
#else
{-# WARNING fileCompressedX "fileCompressedX: not available on this platform, will default to 'False' (CPP guard: @#if STATX_ATTR_COMPRESSED@)" #-}
fileCompressedX _ = False
#endif
#ifdef STATX_ATTR_IMMUTABLE
fileImmutableX ex = testFlag ex (#const STATX_ATTR_IMMUTABLE)
#else
{-# WARNING fileImmutableX "fileImmutableX: not available on this platform, will default to 'False' (CPP guard: @#if STATX_ATTR_IMMUTABLE@)" #-}
fileImmutableX _ = False
#endif
#ifdef STATX_ATTR_APPEND
fileAppendX ex = testFlag ex (#const STATX_ATTR_APPEND)
#else
{-# WARNING fileAppendX "fileAppendX: not available on this platform, will default to 'False' (CPP guard: @#if STATX_ATTR_APPEND@)" #-}
fileAppendX _ = False
#endif
#ifdef STATX_ATTR_NODUMP
fileNoDumpX ex = testFlag ex (#const STATX_ATTR_NODUMP)
#else
{-# WARNING fileNoDumpX "fileNoDumpX: not available on this platform, will default to 'False' (CPP guard: @#if STATX_ATTR_NODUMP@)" #-}
fileNoDumpX _ = False
#endif
#ifdef STATX_ATTR_ENCRYPTED
fileEncryptedX ex = testFlag ex (#const STATX_ATTR_ENCRYPTED)
#else
{-# WARNING fileEncryptedX "fileEncryptedX: not available on this platform, will default to 'False' (CPP guard: @#if STATX_ATTR_ENCRYPTED@)" #-}
fileEncryptedX _ = False
#endif
#ifdef STATX_ATTR_VERITY
fileVerityX ex = testFlag ex (#const STATX_ATTR_VERITY)
#else
{-# WARNING fileVerityX "fileVerityX: not available on this platform, will default to 'False' (CPP guard: @#if STATX_ATTR_VERITY@)" #-}
fileVerityX _ = False
#endif
#ifdef STATX_ATTR_DAX
fileDaxX ex = testFlag ex (#const STATX_ATTR_DAX)
#else
{-# WARNING fileDaxX "fileDaxX: not available on this platform, will default to 'False' (CPP guard: @#if STATX_ATTR_DAX@)" #-}
fileDaxX _ = False
#endif

#ifdef HAVE_SYS_SYSMACROS_H
deviceIDX (ExtendedFileStatus statx) = unsafePerformIO $ do
  major <- withForeignPtr statx $ (#peek struct statx, stx_dev_major) :: IO CUInt
  minor <- withForeignPtr statx $ (#peek struct statx, stx_dev_minor) :: IO CUInt
  c_makedev major minor
#else
{-# WARNING deviceIDX "deviceIDX: not available on this platform, will throw error (CPP guard: @#if HAVE_SYS_SYSMACROS_H@)" #-}
deviceIDX _ = error "deviceIDX not available on this platform"
#endif
#ifdef HAVE_SYS_SYSMACROS_H
specialDeviceIDX (ExtendedFileStatus statx) = unsafePerformIO $ do
  major <- withForeignPtr statx $ (#peek struct statx, stx_rdev_major) :: IO CUInt
  minor <- withForeignPtr statx $ (#peek struct statx, stx_rdev_minor) :: IO CUInt
  c_makedev major minor
#else
{-# WARNING specialDeviceIDX "specialDeviceIDX: not available on this platform, will throw error (CPP guard: @#if HAVE_SYS_SYSMACROS_H@)" #-}
specialDeviceIDX _ = error "specialDeviceIDX not available on this platform"
#endif
#ifdef HAVE_STATX_MNT_ID
mountIDX (ExtendedFileStatus statx) =
  unsafePerformIO $ withForeignPtr statx $ (#peek struct statx, stx_mnt_id)
#else
{-# WARNING mountIDX "mountIDX: not available on this platform, will throw error (CPP guard: @#if HAVE_STATX_MNT_ID@)" #-}
mountIDX _ = error "mountIDX not available on this platform"
#endif
fileBlockSizeX (ExtendedFileStatus statx) = unsafePerformIO $ do
  r <- withForeignPtr statx $ (#peek struct statx, stx_blksize) :: IO Word32
  return $ CBlkSize (fromIntegral r)
fileAttributesX (ExtendedFileStatus statx) =
  unsafePerformIO $ withForeignPtr statx $ (#peek struct statx, stx_attributes)
#ifdef STATX_NLINK
linkCountX (ExtendedFileStatus statx) = unsafePerformIO $ do
  links <- withForeignPtr statx $ (#peek struct statx, stx_nlink) :: IO Word32
  return $ CNlink (fromIntegral links)
#else
{-# WARNING linkCountX "linkCountX: not available on this platform, will throw error (CPP guard: @#if STATX_NLINK@)" #-}
linkCountX _ = error "linkCountX not available on this platform"
#endif
#ifdef STATX_UID
fileOwnerX (ExtendedFileStatus statx) =
  unsafePerformIO $ withForeignPtr statx $ (#peek struct statx, stx_uid)
#else
{-# WARNING fileOwnerX "fileOwnerX: not available on this platform, will throw error (CPP guard: @#if STATX_UID@)" #-}
fileOwnerX _ = error "fileOwnerX not available on this platform"
#endif
#ifdef STATX_GID
fileGroupX (ExtendedFileStatus statx) =
  unsafePerformIO $ withForeignPtr statx $ (#peek struct statx, stx_gid)
#else
{-# WARNING fileGroupX "fileGroupX: not available on this platform, will throw error (CPP guard: @#if STATX_GID@)" #-}
fileGroupX _ = error "fileGroupX not available on this platform"
#endif
#ifdef STATX_MODE
fileModeX (ExtendedFileStatus statx) = unsafePerformIO $ do
  r <- withForeignPtr statx $ (#peek struct statx, stx_mode) :: IO Word16
  return $ CMode $ fromIntegral r
#else
{-# WARNING fileModeX "fileModeX: not available on this platform, will throw error (CPP guard: @#if STATX_MODE@)" #-}
fileModeX _ = error "fileModeX not available on this platform"
#endif
#ifdef STATX_INO
fileIDX (ExtendedFileStatus statx) =
  unsafePerformIO $ withForeignPtr statx $ (#peek struct statx, stx_ino)
#else
{-# WARNING fileIDX "fileIDX: not available on this platform, will throw error (CPP guard: @#if STATX_INO@)" #-}
fileIDX _ = error "fileIDX not available on this platform"
#endif
#ifdef STATX_SIZE
fileSizeX (ExtendedFileStatus statx) =
  unsafePerformIO $ withForeignPtr statx $ (#peek struct statx, stx_size) :: Word64
#else
{-# WARNING fileSizeX "fileSizeX: not available on this platform, will throw error (CPP guard: @#if STATX_SIZE@)" #-}
fileSizeX _ = error "fileSizeX not available on this platform"
#endif
#ifdef STATX_BLOCKS
fileBlocksX (ExtendedFileStatus statx) =
  unsafePerformIO $ withForeignPtr statx $ (#peek struct statx, stx_blocks) :: Word64
#else
{-# WARNING fileBlocksX "fileBlocksX: not available on this platform, will throw error (CPP guard: @#if STATX_BLOCKS@)" #-}
fileBlocksX _ = error "fileBlocksX not available on this platform"
#endif
fileAttributesMaskX (ExtendedFileStatus statx) =
  unsafePerformIO $ withForeignPtr statx $ (#peek struct statx, stx_attributes_mask)
#ifdef STATX_ATIME
accessTimeHiResX (ExtendedFileStatus statx) =
  unsafePerformIO $ withForeignPtr statx $ \statx_ptr -> do
    sec  <- (#peek struct statx, stx_atime.tv_sec) statx_ptr :: IO EpochTime
    nsec <- (#peek struct statx, stx_atime.tv_nsec) statx_ptr :: IO (#type int)
    return $ timeHiResToNominalDiffTime sec nsec
#else
{-# WARNING accessTimeHiResX "accessTimeHiResX: not available on this platform, will throw error (CPP guard: @#if STATX_ATIME@)" #-}
accessTimeHiResX _ = error "accessTimeHiResX not available on this platform"
#endif
#ifdef STATX_BTIME
creationTimeHiResX (ExtendedFileStatus statx) =
  unsafePerformIO $ withForeignPtr statx $ \statx_ptr -> do
    sec  <- (#peek struct statx, stx_btime.tv_sec) statx_ptr :: IO EpochTime
    nsec <- (#peek struct statx, stx_btime.tv_nsec) statx_ptr :: IO (#type int)
    return $ timeHiResToNominalDiffTime sec nsec
#else
{-# WARNING creationTimeHiResX "creationTimeHiResX: not available on this platform, will throw error (CPP guard: @#if STATX_BTIME@)" #-}
creationTimeHiResX _ = error "creationTimeHiResX not available on this platform"
#endif
#ifdef STATX_CTIME
statusChangeTimeHiResX (ExtendedFileStatus statx) =
  unsafePerformIO $ withForeignPtr statx $ \statx_ptr -> do
    sec  <- (#peek struct statx, stx_ctime.tv_sec) statx_ptr :: IO EpochTime
    nsec <- (#peek struct statx, stx_ctime.tv_nsec) statx_ptr :: IO (#type int)
    return $ timeHiResToNominalDiffTime sec nsec
#else
{-# WARNING statusChangeTimeHiResX "statusChangeTimeHiResX: not available on this platform, will throw error (CPP guard: @#if STATX_CTIME@)" #-}
statusChangeTimeHiResX _ = error "statusChangeTimeHiResX not available on this platform"
#endif
#ifdef STATX_MTIME
modificationTimeHiResX (ExtendedFileStatus statx) =
  unsafePerformIO $ withForeignPtr statx $ \statx_ptr -> do
    sec  <- (#peek struct statx, stx_mtime.tv_sec) statx_ptr :: IO EpochTime
    nsec <- (#peek struct statx, stx_mtime.tv_nsec) statx_ptr :: IO (#type int)
    return $ timeHiResToNominalDiffTime sec nsec
#else
{-# WARNING modificationTimeHiResX "modificationTimeHiResX: not available on this platform, will throw error (CPP guard: @#if STATX_MTIME@)" #-}
modificationTimeHiResX _ = error "modificationTimeHiResX not available on this platform"
#endif

timeHiResToNominalDiffTime :: EpochTime -> Int32 -> POSIXTime
timeHiResToNominalDiffTime (CTime sec) nsec = secondsToNominalDiffTime $ MkFixed $ toInteger sec * 1e12 + toInteger nsec * 1e3

#else
{-# WARNING linkCountX "linkCountX: not available on this platform, will throw error (CPP guard: @#if HAVE_STATX@)" #-}
linkCountX _ = error "linkCountX not available on this platform"
{-# WARNING fileBlockSizeX "fileBlockSizeX: not available on this platform, will throw error (CPP guard: @#if HAVE_STATX@)" #-}
fileBlockSizeX _ = error "fileBlockSizeX not available on this platform"
{-# WARNING deviceIDX "deviceIDX: not available on this platform, will throw error (CPP guard: @#if HAVE_STATX@)" #-}
deviceIDX _ = error "deviceIDX not available on this platform"
{-# WARNING specialDeviceIDX "specialDeviceIDX: not available on this platform, will throw error (CPP guard: @#if HAVE_STATX@)" #-}
specialDeviceIDX _ = error "specialDeviceIDX not available on this platform"
{-# WARNING mountIDX "mountIDX: not available on this platform, will throw error (CPP guard: @#if HAVE_STATX@)" #-}
mountIDX _ = error "mountIDX not available on this platform"
{-# WARNING fileOwnerX "fileOwnerX: not available on this platform, will throw error (CPP guard: @#if HAVE_STATX@)" #-}
fileOwnerX _ = error "fileOwnerX not available on this platform"
{-# WARNING fileGroupX "fileGroupX: not available on this platform, will throw error (CPP guard: @#if HAVE_STATX@)" #-}
fileGroupX _ = error "fileGroupX not available on this platform"
{-# WARNING fileModeX "fileModeX: not available on this platform, will throw error (CPP guard: @#if HAVE_STATX@)" #-}
fileModeX _ = error "fileModeX not available on this platform"
{-# WARNING fileIDX "fileIDX: not available on this platform, will throw error (CPP guard: @#if HAVE_STATX@)" #-}
fileIDX _ = error "fileIDX not available on this platform"
{-# WARNING fileSizeX "fileSizeX: not available on this platform, will throw error (CPP guard: @#if HAVE_STATX@)" #-}
fileSizeX _ = error "fileSizeX not available on this platform"
{-# WARNING fileBlocksX "fileBlocksX: not available on this platform, will throw error (CPP guard: @#if HAVE_STATX@)" #-}
fileBlocksX _ = error "fileBlocksX not available on this platform"
{-# WARNING accessTimeHiResX "accessTimeHiResX: not available on this platform, will throw error (CPP guard: @#if HAVE_STATX@)" #-}
accessTimeHiResX _ = error "accessTimeHiResX not available on this platform"
{-# WARNING creationTimeHiResX "creationTimeHiResX: not available on this platform, will throw error (CPP guard: @#if HAVE_STATX@)" #-}
creationTimeHiResX _ = error "creationTimeHiResX not available on this platform"
{-# WARNING statusChangeTimeHiResX "statusChangeTimeHiResX: not available on this platform, will throw error (CPP guard: @#if HAVE_STATX@)" #-}
statusChangeTimeHiResX _ = error "statusChangeTimeHiResX not available on this platform"
{-# WARNING modificationTimeHiResX "modificationTimeHiResX: not available on this platform, will throw error (CPP guard: @#if HAVE_STATX@)" #-}
modificationTimeHiResX _ = error "modificationTimeHiResX not available on this platform"
{-# WARNING fileCompressedX "fileCompressedX: not available on this platform, will default to 'False' (CPP guard: @#if HAVE_STATX@)" #-}
fileCompressedX _ = False
{-# WARNING fileImmutableX "fileImmutableX: not available on this platform, will default to 'False' (CPP guard: @#if HAVE_STATX@)" #-}
fileImmutableX _ = False
{-# WARNING fileAppendX "fileAppendX: not available on this platform, will default to 'False' (CPP guard: @#if HAVE_STATX@)" #-}
fileAppendX _ = False
{-# WARNING fileNoDumpX "fileNoDumpX: not available on this platform, will default to 'False' (CPP guard: @#if HAVE_STATX@)" #-}
fileNoDumpX _ = False
{-# WARNING fileEncryptedX "fileEncryptedX: not available on this platform, will default to 'False' (CPP guard: @#if HAVE_STATX@)" #-}
fileEncryptedX _ = False
{-# WARNING fileVerityX "fileVerityX: not available on this platform, will default to 'False' (CPP guard: @#if HAVE_STATX@)" #-}
fileVerityX _ = False
{-# WARNING fileDaxX "fileDaxX: not available on this platform, will default to 'False' (CPP guard: @#if HAVE_STATX@)" #-}
fileDaxX _ = False
#endif

data {-# CTYPE "struct statx" #-} CStatx

#ifdef HAVE_STATX
foreign import capi unsafe "sys/stat.h statx"
   c_statx :: CInt -> CFilePath -> CInt -> CInt -> Ptr CStatx -> IO CInt

#ifdef HAVE_SYS_SYSMACROS_H
foreign import capi unsafe "sys/sysmacros.h makedev"
  c_makedev :: CUInt -> CUInt -> IO CDev
#endif
#endif


getExtendedFileStatus_ :: Maybe Fd  -- ^ Optional directory file descriptor
                       -> CString   -- ^ Pathname to open
                       -> StatxFlags
                       -> StatxMask
                       -> IO ExtendedFileStatus
#ifdef HAVE_STATX
getExtendedFileStatus_ fdMay str (StatxFlags flags) (StatxMask masks) = do
  fp <- mallocForeignPtrBytes (#const sizeof(struct statx))
  withForeignPtr fp $ \p ->
      throwErrnoIfMinus1_ "getExtendedFileStatus_" (c_statx c_fd str flags masks p)
  return (ExtendedFileStatus fp)
 where
  c_fd = maybe (#const AT_FDCWD) (\ (Fd fd) -> fd) fdMay
#else
{-# WARNING getExtendedFileStatus_ "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_STATX@)" #-}
getExtendedFileStatus_ _ _ _ _ = ioError (ioeSetLocation unsupportedOperation "getExtendedFileStatus")
#endif

-- | Whether 'statx' is available on this platform and 'getExtendedFileStatus' and
-- related functions will work.
haveStatx :: Bool
#ifdef HAVE_STATX
haveStatx = True
#else
haveStatx = False
#endif
