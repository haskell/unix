{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NondecreasingIndentation #-}
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
   DirStream,
   openDirStream,
   readDirStream,
   readDirStreamMaybe,
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
import System.Posix.ByteString.FilePath

-- | @createDirectory dir mode@ calls @mkdir@ to
--   create a new directory, @dir@, with permissions based on
--  @mode@.
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

foreign import capi unsafe "HsUnix.h opendir"
   c_opendir :: CString  -> IO (Ptr CDir)

-- | @readDirStream dp@ calls @readdir@ to obtain the
--   next directory entry (@struct dirent@) for the open directory
--   stream @dp@, and returns the @d_name@ member of that
--  structure.
--
--  Note that this function returns an empty filepath if the end of the
--  directory stream is reached. For a safer alternative use
--  'readDirStreamMaybe'.
readDirStream :: DirStream -> IO RawFilePath
readDirStream = fmap (fromMaybe BC.empty) . readDirStreamMaybe

-- | @readDirStreamMaybe dp@ calls @readdir@ to obtain the
--   next directory entry (@struct dirent@) for the open directory
--   stream @dp@. It returns the @d_name@ member of that
--  structure wrapped in a @Just d_name@ if an entry was read and @Nothing@ if
--  the end of the directory stream was reached.
readDirStreamMaybe :: DirStream -> IO (Maybe RawFilePath)
readDirStreamMaybe (DirStream dirp) =
  alloca $ \ptr_dEnt  -> loop ptr_dEnt
 where
  loop ptr_dEnt = do
    resetErrno
    r <- c_readdir dirp ptr_dEnt
    if (r == 0)
         then do dEnt <- peek ptr_dEnt
                 if (dEnt == nullPtr)
                    then return Nothing
                    else do
                     entry <- (d_name dEnt >>= peekFilePath)
                     c_freeDirEnt dEnt
                     return $ Just entry
         else do errno <- getErrno
                 if (errno == eINTR) then loop ptr_dEnt else do
                 let (Errno eo) = errno
                 if (eo == 0)
                    then return Nothing
                    else throwErrno "readDirStream"

-- traversing directories
foreign import ccall unsafe "__hscore_readdir"
  c_readdir  :: Ptr CDir -> Ptr (Ptr CDirent) -> IO CInt

foreign import ccall unsafe "__hscore_free_dirent"
  c_freeDirEnt  :: Ptr CDirent -> IO ()

foreign import ccall unsafe "__hscore_d_name"
  d_name :: Ptr CDirent -> IO CString


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
