{-# LANGUAGE Safe, CApiFFI #-}

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

module System.Posix.Directory.Common (
       DirStream(..), CDir, CDirent, DirStreamOffset(..),
       unsafeOpenDirStreamFd,
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

import System.Posix.Types
import Foreign
import Foreign.C

newtype DirStream = DirStream (Ptr CDir)

data {-# CTYPE "DIR" #-} CDir
data {-# CTYPE "struct dirent" #-} CDirent

-- | @unsafeOpenDirStreamFd fd@ calls @fdopendir@ to obtain a directory stream
--   for @fd@. @fd@ must not be otherwise used after this; see
--   <https://pubs.opengroup.org/onlinepubs/9699919799/functions/fdopendir.html POSIX>.
unsafeOpenDirStreamFd :: Fd -> IO DirStream
unsafeOpenDirStreamFd (Fd fd) = DirStream <$> throwErrnoIfNull "openDirStreamFd" (c_fdopendir fd)

-- NOTE: It is /critical/ to use "capi" and "dirent.h" here, because system
-- headers on e.g. MacOS alias this function, and linking directly to the
-- "fdopendir" symbol in libc leads to a crash!
--
foreign import capi unsafe "dirent.h fdopendir"
    c_fdopendir :: CInt -> IO (Ptr CDir)

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

changeWorkingDirectoryFd :: Fd -> IO ()
changeWorkingDirectoryFd (Fd fd) =
  throwErrnoIfMinus1Retry_ "changeWorkingDirectoryFd" (c_fchdir fd)

foreign import ccall unsafe "fchdir"
  c_fchdir :: CInt -> IO CInt
