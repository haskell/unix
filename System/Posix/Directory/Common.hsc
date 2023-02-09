{-# LANGUAGE CPP, Safe, CApiFFI, PatternSynonyms #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Directory.Common
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX directory support
--
-----------------------------------------------------------------------------

#include "HsUnix.h"
#include "HsUnixConfig.h"
##include "HsUnixConfig.h"

module System.Posix.Directory.Common (
       DirStream(..), DirEnt(..), CDir, CDirent, DirStreamOffset(..),
       DirType( DirType
              , UnknownType
              , NamedPipeType
              , CharacterDeviceType
              , DirectoryType
              , BlockDeviceType
              , RegularFileType
              , SymbolicLinkType
              , SocketType
              , WhiteoutType
              ),
       isUnknownType, isBlockDeviceType, isCharacterDeviceType, isNamedPipeType,
       isRegularFileType, isDirectoryType, isSymbolicLinkType, isSocketType,
       isWhiteoutType,
       unsafeOpenDirStreamFd,
       readDirStreamWith,
       readDirStreamWithPtr,
       rewindDirStream,
       closeDirStream,
#ifdef HAVE_SEEKDIR
       seekDirStream,
#endif
#ifdef HAVE_TELLDIR
       tellDirStream,
#endif
       changeWorkingDirectoryFd,
  ) where

import Control.Exception (mask_)
import Control.Monad (void, when)
import System.Posix.Types
import Foreign hiding (void)
import Foreign.C

#if !defined(HAVE_FCHDIR)
import System.IO.Error ( ioeSetLocation )
import GHC.IO.Exception ( unsupportedOperation )
#endif

newtype DirStream = DirStream (Ptr CDir)

newtype DirEnt = DirEnt (Ptr CDirent)

-- We provide a hand-written instance here since GeneralizedNewtypeDeriving and
-- DerivingVia are not allowed in Safe Haskell.
instance Storable DirEnt where
  sizeOf _ = sizeOf (undefined :: Ptr CDirent)
  {-# INLINE sizeOf #-}

  alignment _ = alignment (undefined :: Ptr CDirent)
  {-# INLINE alignment #-}

  peek ptr = DirEnt <$> peek (castPtr ptr)
  {-# INLINE peek #-}

  poke ptr (DirEnt dEnt) = poke (castPtr ptr) dEnt
  {-# INLINE poke#-}

data {-# CTYPE "DIR" #-} CDir
data {-# CTYPE "struct dirent" #-} CDirent

-- | The value of the @d_type@ field of a @dirent@ struct.
-- Note that the possible values of that type depend on the filesystem that is
-- queried. From @readdir(3)@:
--
-- > Currently, only some filesystems (among them: Btrfs, ext2, ext3, and ext4)
-- > have full support for returning the file type in d_type. All applications
-- > must properly handle a return of DT_UNKNOWN.
--
-- For example, JFS is a filesystem that does not support @d_type@;
-- See https://github.com/haskell/ghcup-hs/issues/766
--
-- Furthermore, @dirent@ or the constants represented by the associated pattern
-- synonyms of this type may not be provided by the underlying platform. In that
-- case none of those patterns will match and the application must handle that
-- case accordingly.
newtype DirType = DirType CChar
    deriving Eq

-- | The 'DirType' refers to an entry of unknown type.
pattern UnknownType :: DirType
pattern UnknownType = DirType CONST_DT_UNKNOWN

-- | The 'DirType' refers to an entry that is a named pipe.
pattern NamedPipeType :: DirType
pattern NamedPipeType = DirType CONST_DT_FIFO

-- | The 'DirType' refers to an entry that is a character device.
pattern CharacterDeviceType :: DirType
pattern CharacterDeviceType = DirType CONST_DT_CHR

-- | The 'DirType' refers to an entry that is a directory.
pattern DirectoryType :: DirType
pattern DirectoryType = DirType CONST_DT_DIR

-- | The 'DirType' refers to an entry that is a block device.
pattern BlockDeviceType :: DirType
pattern BlockDeviceType = DirType CONST_DT_BLK

-- | The 'DirType' refers to an entry that is a regular file.
pattern RegularFileType :: DirType
pattern RegularFileType = DirType CONST_DT_REG

-- | The 'DirType' refers to an entry that is a symbolic link.
pattern SymbolicLinkType :: DirType
pattern SymbolicLinkType = DirType CONST_DT_LNK

-- | The 'DirType' refers to an entry that is a socket.
pattern SocketType :: DirType
pattern SocketType = DirType CONST_DT_SOCK

-- | The 'DirType' refers to an entry that is a whiteout.
pattern WhiteoutType :: DirType
pattern WhiteoutType = DirType CONST_DT_WHT

-- | Checks if this 'DirType' refers to an entry of unknown type.
isUnknownType         :: DirType -> Bool
-- | Checks if this 'DirType' refers to a block device entry.
isBlockDeviceType     :: DirType -> Bool
-- | Checks if this 'DirType' refers to a character device entry.
isCharacterDeviceType :: DirType -> Bool
-- | Checks if this 'DirType' refers to a named pipe entry.
isNamedPipeType       :: DirType -> Bool
-- | Checks if this 'DirType' refers to a regular file entry.
isRegularFileType     :: DirType -> Bool
-- | Checks if this 'DirType' refers to a directory entry.
isDirectoryType       :: DirType -> Bool
-- | Checks if this 'DirType' refers to a symbolic link entry.
isSymbolicLinkType    :: DirType -> Bool
-- | Checks if this 'DirType' refers to a socket entry.
isSocketType          :: DirType -> Bool
-- | Checks if this 'DirType' refers to a whiteout entry.
isWhiteoutType        :: DirType -> Bool

isUnknownType dtype = dtype == UnknownType
isBlockDeviceType dtype = dtype == BlockDeviceType
isCharacterDeviceType dtype = dtype == CharacterDeviceType
isNamedPipeType dtype = dtype == NamedPipeType
isRegularFileType dtype = dtype == RegularFileType
isDirectoryType dtype = dtype == DirectoryType
isSymbolicLinkType dtype = dtype == SymbolicLinkType
isSocketType dtype = dtype == SocketType
isWhiteoutType dtype = dtype == WhiteoutType

-- | Call @fdopendir@ to obtain a directory stream for @fd@. @fd@ must not be
-- otherwise used after this.
--
-- On success, it is owned by the returned 'DirStream', which should be closed
-- via 'closeDirStream' when no longer needed.  On error, the file descriptor
-- is automatically closed and then an exception is thrown.  There is no code
-- path in which the file descriptor remains open and yet not owned by a
-- returned 'DirStream'.
--
-- The input file descriptor must not have been used with @threadWaitRead@ or
-- @threadWaitWrite@.
unsafeOpenDirStreamFd :: Fd -> IO DirStream
unsafeOpenDirStreamFd (Fd fd) = mask_ $ do
    ptr <- c_fdopendir fd
    when (ptr == nullPtr) $ do
        errno <- getErrno
        void $ c_close fd
        ioError (errnoToIOError "openDirStreamFd" errno Nothing Nothing)
    return $ DirStream ptr

-- We need c_close here, because 'closeFd' throws exceptions on error,
-- but we want to silently close the (presumably directory) descriptor.
foreign import ccall unsafe "HsUnix.h close"
   c_close :: CInt -> IO CInt

-- NOTE: It is /critical/ to use "capi" and "dirent.h" here, because system
-- headers on e.g. macOS alias this function, and linking directly to the
-- "fdopendir" symbol in libc leads to a crash!
--
foreign import capi unsafe "dirent.h fdopendir"
    c_fdopendir :: CInt -> IO (Ptr CDir)


-- | @readDirStreamWith f dp@ calls @readdir@ to obtain the next directory entry
--   (@struct dirent@) for the open directory stream @dp@. If an entry is read,
--   it passes the pointer to that structure to the provided function @f@ for
--   processing. It returns the result of that function call wrapped in a @Just@
--   if an entry was read and @Nothing@ if the end of the directory stream was
--   reached.
--
--   __NOTE:__ The lifetime of the pointer wrapped in the `DirEnt` is limited to
--   invocation of the callback and it will be freed automatically after. Do not
--   pass it to the outside world!
readDirStreamWith :: (DirEnt -> IO a) -> DirStream -> IO (Maybe a)
readDirStreamWith f dstream = alloca
  (\ptr_dEnt  -> readDirStreamWithPtr ptr_dEnt f dstream)

-- | A version of 'readDirStreamWith' that takes a pre-allocated pointer in
--   addition to the other arguments. This pointer is used to store the pointer
--   to the next directory entry, if there is any. This function is intended for
--   usecases where you need to read a lot of directory entries and want to
--   reuse the pointer for each of them. Using for example 'readDirStream' or
--   'readDirStreamWith' in this scenario would allocate a new pointer for each
--   call of these functions.
--
--   __NOTE__: You are responsible for releasing the pointer after you are done.
readDirStreamWithPtr :: Ptr DirEnt -> (DirEnt -> IO a) -> DirStream -> IO (Maybe a)
readDirStreamWithPtr ptr_dEnt f dstream@(DirStream dirp) = do
  resetErrno
  r <- c_readdir dirp (castPtr ptr_dEnt)
  if (r == 0)
       then do dEnt@(DirEnt dEntPtr) <- peek ptr_dEnt
               if (dEntPtr == nullPtr)
                  then return Nothing
                  else do
                   res <- f dEnt
                   c_freeDirEnt dEntPtr
                   return (Just res)
       else do errno <- getErrno
               if (errno == eINTR)
                  then readDirStreamWithPtr ptr_dEnt f dstream
                  else do
                   let (Errno eo) = errno
                   if (eo == 0)
                      then return Nothing
                      else throwErrno "readDirStream"

-- traversing directories
foreign import ccall unsafe "__hscore_readdir"
  c_readdir  :: Ptr CDir -> Ptr (Ptr CDirent) -> IO CInt

foreign import ccall unsafe "__hscore_free_dirent"
  c_freeDirEnt  :: Ptr CDirent -> IO ()


-- | @rewindDirStream dp@ calls @rewinddir@ to reposition
--   the directory stream @dp@ at the beginning of the directory.
rewindDirStream :: DirStream -> IO ()
rewindDirStream (DirStream dirp) = c_rewinddir dirp

foreign import ccall unsafe "rewinddir"
   c_rewinddir :: Ptr CDir -> IO ()

-- | @closeDirStream dp@ calls @closedir@ to close
--   the directory stream @dp@.
closeDirStream :: DirStream -> IO ()
closeDirStream (DirStream dirp) = do
  throwErrnoIfMinus1Retry_ "closeDirStream" (c_closedir dirp)

foreign import ccall unsafe "closedir"
   c_closedir :: Ptr CDir -> IO CInt

newtype DirStreamOffset = DirStreamOffset COff

#ifdef HAVE_SEEKDIR
seekDirStream :: DirStream -> DirStreamOffset -> IO ()
seekDirStream (DirStream dirp) (DirStreamOffset off) =
  c_seekdir dirp (fromIntegral off) -- TODO: check for CLong/COff overflow

foreign import ccall unsafe "seekdir"
  c_seekdir :: Ptr CDir -> CLong -> IO ()
#endif

#ifdef HAVE_TELLDIR
tellDirStream :: DirStream -> IO DirStreamOffset
tellDirStream (DirStream dirp) = do
  off <- c_telldir dirp
  return (DirStreamOffset (fromIntegral off)) -- TODO: check for overflow

foreign import ccall unsafe "telldir"
  c_telldir :: Ptr CDir -> IO CLong
#endif

#if defined(HAVE_FCHDIR)

changeWorkingDirectoryFd :: Fd -> IO ()
changeWorkingDirectoryFd (Fd fd) =
  throwErrnoIfMinus1Retry_ "changeWorkingDirectoryFd" (c_fchdir fd)

foreign import ccall unsafe "fchdir"
  c_fchdir :: CInt -> IO CInt

#else

{-# WARNING changeWorkingDirectoryFd "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_FCHDIR@)" #-}
changeWorkingDirectoryFd :: Fd -> IO ()
changeWorkingDirectoryFd _ = ioError (ioeSetLocation unsupportedOperation "changeWorkingDirectoryFd")

#endif // HAVE_FCHDIR
