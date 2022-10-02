{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE Safe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Directory
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

module System.Posix.Directory (
   -- * Creating and removing directories
   createDirectory, removeDirectory,

   -- * Reading directories
   DirStream,
   DirEnt(..),
   openDirStream,
   readDirStream,
   readDirStreamMaybe,
   readDirStreamWith,
   readDirStreamWithPtr,
   rewindDirStream,
   closeDirStream,
   DirStreamOffset,
#ifdef HAVE_TELLDIR
   tellDirStream,
#endif
#ifdef HAVE_SEEKDIR
   seekDirStream,
#endif

   -- * The working dirctory
   getWorkingDirectory,
   changeWorkingDirectory,
   changeWorkingDirectoryFd,
  ) where

import Data.Maybe
import System.Posix.Error
import System.Posix.Types
import Foreign
import Foreign.C

import System.Posix.Directory.Common
import System.Posix.Internals (withFilePath, peekFilePath)

-- | @createDirectory dir mode@ calls @mkdir@ to
--   create a new directory, @dir@, with permissions based on
--  @mode@.
createDirectory :: FilePath -> FileMode -> IO ()
createDirectory name mode =
  withFilePath name $ \s ->
    throwErrnoPathIfMinus1Retry_ "createDirectory" name (c_mkdir s mode)
    -- POSIX doesn't allow mkdir() to return EINTR, but it does on
    -- OS X (#5184), so we need the Retry variant here.

foreign import ccall unsafe "mkdir"
  c_mkdir :: CString -> CMode -> IO CInt

-- | @openDirStream dir@ calls @opendir@ to obtain a
--   directory stream for @dir@.
openDirStream :: FilePath -> IO DirStream
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
readDirStream :: DirStream -> IO FilePath
readDirStream = fmap (fromMaybe "") . readDirStreamMaybe

-- | @readDirStreamMaybe dp@ calls @readdir@ to obtain the
--   next directory entry (@struct dirent@) for the open directory
--   stream @dp@. It returns the @d_name@ member of that
--   structure wrapped in a @Just d_name@ if an entry was read and @Nothing@ if
--   the end of the directory stream was reached.
readDirStreamMaybe :: DirStream -> IO (Maybe FilePath)
readDirStreamMaybe = readDirStreamWith
  (\(DirEnt dEnt) -> d_name dEnt >>= peekFilePath)

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

foreign import ccall unsafe "__hscore_d_name"
  d_name :: Ptr CDirent -> IO CString


-- | @getWorkingDirectory@ calls @getcwd@ to obtain the name
--   of the current working directory.
getWorkingDirectory :: IO FilePath
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
changeWorkingDirectory :: FilePath -> IO ()
changeWorkingDirectory path =
  withFilePath path $ \s ->
     throwErrnoPathIfMinus1Retry_ "changeWorkingDirectory" path (c_chdir s)

foreign import ccall unsafe "chdir"
   c_chdir :: CString -> IO CInt

removeDirectory :: FilePath -> IO ()
removeDirectory path =
  withFilePath path $ \s ->
     throwErrnoPathIfMinus1Retry_ "removeDirectory" path (c_rmdir s)

foreign import ccall unsafe "rmdir"
   c_rmdir :: CString -> IO CInt
