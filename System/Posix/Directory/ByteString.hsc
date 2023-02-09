{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Directory.ByteString
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- String-based POSIX directory support
--
-----------------------------------------------------------------------------

#include "HsUnix.h"

-- hack copied from System.Posix.Files
#if !defined(PATH_MAX)
# define PATH_MAX 4096
#endif

module System.Posix.Directory.ByteString (
   -- * Creating and removing directories
   createDirectory, removeDirectory,

   -- * Reading directories
   DirStream, DirStreamWithPath,
   fromDirStreamWithPath,
   DirType( UnknownType
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
   openDirStream,
   openDirStreamWithPath,
   readDirStream,
   readDirStreamMaybe,
   readDirStreamWithType,
   rewindDirStream,
   closeDirStream,
   DirStreamOffset,
#ifdef HAVE_TELLDIR
   tellDirStream,
#endif
#ifdef HAVE_SEEKDIR
   seekDirStream,
#endif

   -- * The working directory
   getWorkingDirectory,
   changeWorkingDirectory,
   changeWorkingDirectoryFd,
  ) where

import Data.Maybe
import System.Posix.Types
import Foreign
import Foreign.C

import Data.ByteString.Char8 as BC

import System.Posix.Directory.Common
import System.Posix.Files.ByteString
import System.Posix.ByteString.FilePath

-- | @createDirectory dir mode@ calls @mkdir@ to
--   create a new directory, @dir@, with permissions based on
--   @mode@.
createDirectory :: RawFilePath -> FileMode -> IO ()
createDirectory name mode =
  withFilePath name $ \s ->
    throwErrnoPathIfMinus1Retry_ "createDirectory" name (c_mkdir s mode)
    -- POSIX doesn't allow mkdir() to return EINTR, but it does on
    -- OS X (#5184), so we need the Retry variant here.

foreign import ccall unsafe "mkdir"
  c_mkdir :: CString -> CMode -> IO CInt

-- | @openDirStream dir@ calls @opendir@ to obtain a
--   directory stream for @dir@.
openDirStream :: RawFilePath -> IO DirStream
openDirStream name =
  withFilePath name $ \s -> do
    dirp <- throwErrnoPathIfNullRetry "openDirStream" name $ c_opendir s
    return (DirStream dirp)

-- | A version of 'openDirStream' where the path of the directory is stored in
-- the returned 'DirStreamWithPath'.
openDirStreamWithPath :: RawFilePath -> IO (DirStreamWithPath RawFilePath)
openDirStreamWithPath name = toDirStreamWithPath name <$> openDirStream name

foreign import capi unsafe "HsUnix.h opendir"
   c_opendir :: CString  -> IO (Ptr CDir)

-- | @readDirStream dp@ calls @readdir@ to obtain the
--   next directory entry (@struct dirent@) for the open directory
--   stream @dp@, and returns the @d_name@ member of that
--   structure.
--
--   Note that this function returns an empty filepath if the end of the
--   directory stream is reached. For a safer alternative use
--   'readDirStreamMaybe'.
readDirStream :: DirStream -> IO RawFilePath
readDirStream = fmap (fromMaybe BC.empty) . readDirStreamMaybe

-- | @readDirStreamMaybe dp@ calls @readdir@ to obtain the
--   next directory entry (@struct dirent@) for the open directory
--   stream @dp@. It returns the @d_name@ member of that
--   structure wrapped in a @Just d_name@ if an entry was read and @Nothing@ if
--   the end of the directory stream was reached.
readDirStreamMaybe :: DirStream -> IO (Maybe RawFilePath)
readDirStreamMaybe = readDirStreamWith
  (\(DirEnt dEnt) -> d_name dEnt >>= peekFilePath)

-- | @readDirStreamWithType dp@ calls @readdir@ to obtain the
--   next directory entry (@struct dirent@) for the open directory
--   stream @dp@. It returns the @d_name@ member of that
--   structure together with the entry's type (@d_type@) wrapped in a
--   @Just (d_name, d_type)@ if an entry was read and @Nothing@ if
--   the end of the directory stream was reached.
--
--   __Note__: The returned 'DirType' has some limitations; Please see its
--   documentation.
readDirStreamWithType :: DirStreamWithPath RawFilePath -> IO (Maybe (RawFilePath, DirType))
readDirStreamWithType (DirStreamWithPath (base, ptr)) = readDirStreamWith
  (\(DirEnt dEnt) -> do
    name <- d_name dEnt >>= peekFilePath
    let getStat = getFileStatus (base <> "/" <> name)
    dtype <- d_type dEnt >>= getRealDirType getStat . DirType
    return (name, dtype)
  )
  (DirStream ptr)

foreign import ccall unsafe "__hscore_d_name"
  d_name :: Ptr CDirent -> IO CString

foreign import ccall unsafe "__hscore_d_type"
  d_type :: Ptr CDirent -> IO CChar


-- | @getWorkingDirectory@ calls @getcwd@ to obtain the name
--   of the current working directory.
getWorkingDirectory :: IO RawFilePath
getWorkingDirectory = go (#const PATH_MAX)
  where
    go bytes = do
        r <- allocaBytes bytes $ \buf -> do
            buf' <- c_getcwd buf (fromIntegral bytes)
            if buf' /= nullPtr
                then do s <- peekFilePath buf
                        return (Just s)
                else do errno <- getErrno
                        if errno == eRANGE
                            -- we use Nothing to indicate that we should
                            -- try again with a bigger buffer
                            then return Nothing
                            else throwErrno "getWorkingDirectory"
        maybe (go (2 * bytes)) return r

foreign import ccall unsafe "getcwd"
   c_getcwd   :: Ptr CChar -> CSize -> IO (Ptr CChar)

-- | @changeWorkingDirectory dir@ calls @chdir@ to change
--   the current working directory to @dir@.
changeWorkingDirectory :: RawFilePath -> IO ()
changeWorkingDirectory path =
  withFilePath path $ \s ->
     throwErrnoPathIfMinus1Retry_ "changeWorkingDirectory" path (c_chdir s)

foreign import ccall unsafe "chdir"
   c_chdir :: CString -> IO CInt

removeDirectory :: RawFilePath -> IO ()
removeDirectory path =
  withFilePath path $ \s ->
     throwErrnoPathIfMinus1Retry_ "removeDirectory" path (c_rmdir s)

foreign import ccall unsafe "rmdir"
   c_rmdir :: CString -> IO CInt
