{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NondecreasingIndentation #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Directory.PosixPath
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- PosixPath based POSIX directory support
--
-----------------------------------------------------------------------------

#include "HsUnix.h"

-- hack copied from System.Posix.Files
#if !defined(PATH_MAX)
# define PATH_MAX 4096
#endif

module System.Posix.Directory.PosixPath (
   -- * Creating and removing directories
   createDirectory, removeDirectory,

   -- * Reading directories
   Common.DirStream,
   Common.DirType( DtUnknown
                 , DtFifo
                 , DtChr
                 , DtDir
                 , DtBlk
                 , DtReg
                 , DtLnk
                 , DtSock
                 , DtWht
                 ),
   openDirStream,
   readDirStream,
   readDirStreamMaybe,
   readDirStreamWithType,
   Common.rewindDirStream,
   Common.closeDirStream,
   Common.DirStreamOffset,
#ifdef HAVE_TELLDIR
   Common.tellDirStream,
#endif
#ifdef HAVE_SEEKDIR
   Common.seekDirStream,
#endif

   -- * The working directory
   getWorkingDirectory,
   changeWorkingDirectory,
   Common.changeWorkingDirectoryFd,
  ) where

import Data.Maybe
import System.Posix.Types
import Foreign
import Foreign.C

import System.OsPath.Types
import qualified System.Posix.Directory.Common as Common
import System.Posix.PosixPath.FilePath

-- | @createDirectory dir mode@ calls @mkdir@ to
--   create a new directory, @dir@, with permissions based on
--   @mode@.
createDirectory :: PosixPath -> FileMode -> IO ()
createDirectory name mode =
  withFilePath name $ \s ->
    throwErrnoPathIfMinus1Retry_ "createDirectory" name (c_mkdir s mode)
    -- POSIX doesn't allow mkdir() to return EINTR, but it does on
    -- OS X (#5184), so we need the Retry variant here.

foreign import ccall unsafe "mkdir"
  c_mkdir :: CString -> CMode -> IO CInt

-- | @openDirStream dir@ calls @opendir@ to obtain a
--   directory stream for @dir@.
openDirStream :: PosixPath -> IO Common.DirStream
openDirStream name =
  withFilePath name $ \s -> do
    dirp <- throwErrnoPathIfNullRetry "openDirStream" name $ c_opendir s
    return (Common.DirStream dirp)

foreign import capi unsafe "HsUnix.h opendir"
   c_opendir :: CString  -> IO (Ptr Common.CDir)

-- | @readDirStream dp@ calls @readdir@ to obtain the
--   next directory entry (@struct dirent@) for the open directory
--   stream @dp@, and returns the @d_name@ member of that
--   structure.
--
--   Note that this function returns an empty filepath if the end of the
--   directory stream is reached. For a safer alternative use
--   'readDirStreamMaybe'.
readDirStream :: Common.DirStream -> IO PosixPath
readDirStream = fmap (fromMaybe mempty) . readDirStreamMaybe

-- | @readDirStreamMaybe dp@ calls @readdir@ to obtain the
--   next directory entry (@struct dirent@) for the open directory
--   stream @dp@. It returns the @d_name@ member of that
--   structure wrapped in a @Just d_name@ if an entry was read and @Nothing@ if
--   the end of the directory stream was reached.
readDirStreamMaybe :: Common.DirStream -> IO (Maybe PosixPath)
readDirStreamMaybe = Common.readDirStreamWith
  (\(Common.DirEnt dEnt) -> d_name dEnt >>= peekFilePath)

-- | @readDirStreamWithType dp@ calls @readdir@ to obtain the
--   next directory entry (@struct dirent@) for the open directory
--   stream @dp@. It returns the @d_name@ member of that
--   structure together with the entry's type (@d_type@) wrapped in a
--   @Just (d_name, d_type)@ if an entry was read and @Nothing@ if
--   the end of the directory stream was reached.
readDirStreamWithType :: Common.DirStream -> IO (Maybe (PosixPath, Common.DirType))
readDirStreamWithType = Common.readDirStreamWith
  (\(Common.DirEnt dEnt) -> (,)
    <$> (d_name dEnt >>= peekFilePath)
    <*> (Common.DirType <$> d_type dEnt)
  )

foreign import ccall unsafe "__hscore_d_name"
  d_name :: Ptr Common.CDirent -> IO CString

foreign import ccall unsafe "__hscore_d_type"
  d_type :: Ptr Common.CDirent -> IO CChar


-- | @getWorkingDirectory@ calls @getcwd@ to obtain the name
--   of the current working directory.
getWorkingDirectory :: IO PosixPath
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
changeWorkingDirectory :: PosixPath -> IO ()
changeWorkingDirectory path =
  withFilePath path $ \s ->
     throwErrnoPathIfMinus1Retry_ "changeWorkingDirectory" path (c_chdir s)

foreign import ccall unsafe "chdir"
   c_chdir :: CString -> IO CInt

removeDirectory :: PosixPath -> IO ()
removeDirectory path =
  withFilePath path $ \s ->
     throwErrnoPathIfMinus1Retry_ "removeDirectory" path (c_rmdir s)

foreign import ccall unsafe "rmdir"
   c_rmdir :: CString -> IO CInt
