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

import System.Posix.Types
import System.Posix.Internals
import System.Directory hiding (createDirectory)
import Foreign
import Foreign.C

createDirectory :: FilePath -> FileMode -> IO ()
createDirectory name mode =
  withCString name $ \s -> 
    throwErrnoIfMinus1_ "createDirectory" (c_mkdir s mode)  

foreign import ccall unsafe "mkdir"
  c_mkdir :: CString -> CMode -> IO CInt

newtype DirStream = DirStream (Ptr CDir)

openDirStream :: FilePath -> IO DirStream
openDirStream name =
  withCString name $ \s -> do
    dirp <- throwErrnoIfNull "openDirStream" $ c_opendir s
    return (DirStream dirp)

readDirStream :: DirStream -> IO FilePath
readDirStream (DirStream dirp) =
  alloca $ \ptr_dEnt  -> loop ptr_dEnt
 where
  loop ptr_dEnt = do
    resetErrno
    r <- readdir dirp ptr_dEnt
    if (r == 0)
	 then do dEnt <- peek ptr_dEnt
		 if (dEnt == nullPtr)
		    then return []
		    else do
	 	     entry <- (d_name dEnt >>= peekCString)
		     freeDirEnt dEnt
		     return entry
	 else do errno <- getErrno
		 if (errno == eINTR) then loop ptr_dEnt else do
		 let (Errno eo) = errno
		 if (eo == end_of_dir)
		    then return []
		    else throwErrno "readDirStream"

rewindDirStream :: DirStream -> IO ()
rewindDirStream (DirStream dirp) = c_rewinddir dirp

closeDirStream :: DirStream -> IO ()
closeDirStream (DirStream dirp) = do
  throwErrnoIfMinus1_ "closeDirStream" (c_closedir dirp)

newtype DirStreamOffset = DirStreamOffset CLong

seekDirStream :: DirStream -> DirStreamOffset -> IO ()
seekDirStream (DirStream dirp) (DirStreamOffset off) =
  c_seekdir dirp off

foreign import ccall unsafe "seekdir"
  c_seekdir :: Ptr CDir -> CLong -> IO ()

tellDirStream :: DirStream -> IO DirStreamOffset
tellDirStream (DirStream dirp) = do
  off <- c_telldir dirp
  return (DirStreamOffset off)

foreign import ccall unsafe "telldir"
  c_telldir :: Ptr CDir -> IO CLong

{-
 Renamings of functionality provided via Directory interface,
 kept around for b.wards compatibility and for having more POSIXy
 names
-}
getWorkingDirectory :: IO FilePath
getWorkingDirectory = getCurrentDirectory

changeWorkingDirectory :: FilePath -> IO ()
changeWorkingDirectory name = setCurrentDirectory name

changeWorkingDirectoryFd :: Fd -> IO ()
changeWorkingDirectoryFd (Fd fd) = 
  throwErrnoIfMinus1_ "changeWorkingDirectoryFd" (c_fchdir fd)

foreign import ccall unsafe "fchdir"
  c_fchdir :: CInt -> IO CInt
