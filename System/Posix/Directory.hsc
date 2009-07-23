{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
-- POSIX directory support
--
-----------------------------------------------------------------------------

module System.Posix.Directory (
   -- * Creating and removing directories
   createDirectory, removeDirectory,

   -- * Reading directories
   DirStream,
   openDirStream,
   readDirStream,
   rewindDirStream,   
   closeDirStream,
   DirStreamOffset,
   tellDirStream,
   seekDirStream,

   -- * The working dirctory
   getWorkingDirectory,
   changeWorkingDirectory,
   changeWorkingDirectoryFd,
  ) where

import System.IO.Error
import System.Posix.Error
import System.Posix.Types
import Foreign
import Foreign.C

-- | @createDirectory dir mode@ calls @mkdir@ to 
--   create a new directory, @dir@, with permissions based on
--  @mode@.
createDirectory :: FilePath -> FileMode -> IO ()
createDirectory name mode =
  withCString name $ \s -> 
    throwErrnoPathIfMinus1_ "createDirectory" name (c_mkdir s mode)  

foreign import ccall unsafe "mkdir"
  c_mkdir :: CString -> CMode -> IO CInt

newtype DirStream = DirStream (Ptr CDir)

-- | @openDirStream dir@ calls @opendir@ to obtain a
--   directory stream for @dir@.
openDirStream :: FilePath -> IO DirStream
openDirStream name =
  withCString name $ \s -> do
    dirp <- throwErrnoPathIfNull "openDirStream" name $ c_opendir s
    return (DirStream dirp)

foreign import ccall unsafe "__hsunix_opendir"
   c_opendir :: CString  -> IO (Ptr CDir)

-- | @readDirStream dp@ calls @readdir@ to obtain the
--   next directory entry (@struct dirent@) for the open directory
--   stream @dp@, and returns the @d_name@ member of that
--  structure.
readDirStream :: DirStream -> IO FilePath
readDirStream (DirStream dirp) =
  alloca $ \ptr_dEnt  -> loop ptr_dEnt
 where
  loop ptr_dEnt = do
    resetErrno
    r <- c_readdir dirp ptr_dEnt
    if (r == 0)
	 then do dEnt <- peek ptr_dEnt
		 if (dEnt == nullPtr)
		    then return []
		    else do
	 	     entry <- (d_name dEnt >>= peekCString)
		     c_freeDirEnt dEnt
		     return entry
	 else do errno <- getErrno
		 if (errno == eINTR) then loop ptr_dEnt else do
		 let (Errno eo) = errno
		 if (eo == 0)
		    then return []
		    else throwErrno "readDirStream"

type CDir       = ()
type CDirent    = ()

-- traversing directories
foreign import ccall unsafe "__hscore_readdir"
  c_readdir  :: Ptr CDir -> Ptr (Ptr CDirent) -> IO CInt

foreign import ccall unsafe "__hscore_free_dirent"
  c_freeDirEnt  :: Ptr CDirent -> IO ()

foreign import ccall unsafe "__hscore_d_name"
  d_name :: Ptr CDirent -> IO CString

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
  throwErrnoIfMinus1_ "closeDirStream" (c_closedir dirp)

foreign import ccall unsafe "closedir"
   c_closedir :: Ptr CDir -> IO CInt

newtype DirStreamOffset = DirStreamOffset COff

seekDirStream :: DirStream -> DirStreamOffset -> IO ()
seekDirStream (DirStream dirp) (DirStreamOffset off) =
  c_seekdir dirp off

foreign import ccall unsafe "seekdir"
  c_seekdir :: Ptr CDir -> COff -> IO ()

tellDirStream :: DirStream -> IO DirStreamOffset
tellDirStream (DirStream dirp) = do
  off <- c_telldir dirp
  return (DirStreamOffset off)

foreign import ccall unsafe "telldir"
  c_telldir :: Ptr CDir -> IO COff

{-
 Renamings of functionality provided via Directory interface,
 kept around for b.wards compatibility and for having more POSIXy
 names
-}

-- | @getWorkingDirectory@ calls @getcwd@ to obtain the name
--   of the current working directory.
getWorkingDirectory :: IO FilePath
getWorkingDirectory = do
  p <- mallocBytes long_path_size
  go p long_path_size
  where go p bytes = do
    	  p' <- c_getcwd p (fromIntegral bytes)
	  if p' /= nullPtr 
	     then do s <- peekCString p'
		     free p'
		     return s
	     else do errno <- getErrno
		     if errno == eRANGE
		        then do let bytes' = bytes * 2
			        p'' <- reallocBytes p bytes'
			        go p'' bytes'
		        else throwErrno "getCurrentDirectory"

foreign import ccall unsafe "getcwd"
   c_getcwd   :: Ptr CChar -> CSize -> IO (Ptr CChar)

foreign import ccall unsafe "__hsunix_long_path_size"
  long_path_size :: Int

-- | @changeWorkingDirectory dir@ calls @chdir@ to change
--   the current working directory to @dir@.
changeWorkingDirectory :: FilePath -> IO ()
changeWorkingDirectory path =
  modifyIOError (`ioeSetFileName` path) $
    withCString path $ \s -> 
       throwErrnoIfMinus1Retry_ "changeWorkingDirectory" (c_chdir s)

foreign import ccall unsafe "chdir"
   c_chdir :: CString -> IO CInt

removeDirectory :: FilePath -> IO ()
removeDirectory path =
  modifyIOError (`ioeSetFileName` path) $
    withCString path $ \s ->
       throwErrnoIfMinus1Retry_ "removeDirectory" (c_rmdir s)

foreign import ccall unsafe "rmdir"
   c_rmdir :: CString -> IO CInt

changeWorkingDirectoryFd :: Fd -> IO ()
changeWorkingDirectoryFd (Fd fd) = 
  throwErrnoIfMinus1_ "changeWorkingDirectoryFd" (c_fchdir fd)

foreign import ccall unsafe "fchdir"
  c_fchdir :: CInt -> IO CInt
