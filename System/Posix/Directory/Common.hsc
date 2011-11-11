{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

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

module System.Posix.Directory.Common (
       DirStream(..), CDir, CDirent, DirStreamOffset(..),
       rewindDirStream,
       closeDirStream,
       seekDirStream,
       tellDirStream,
       changeWorkingDirectoryFd,
  ) where

import System.IO.Error
import System.Posix.Error
import System.Posix.Types
import Foreign
import Foreign.C

newtype DirStream = DirStream (Ptr CDir)

type CDir       = ()
type CDirent    = ()

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

changeWorkingDirectoryFd :: Fd -> IO ()
changeWorkingDirectoryFd (Fd fd) = 
  throwErrnoIfMinus1Retry_ "changeWorkingDirectoryFd" (c_fchdir fd)

foreign import ccall unsafe "fchdir"
  c_fchdir :: CInt -> IO CInt
