{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE PackageImports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Temp.PosixString
-- Copyright   :  (c) Volker Stolz <vs@foldr.org>
--                    Deian Stefan <deian@cs.stanford.edu>
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org, vs@foldr.org, deian@cs.stanford.edu
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX temporary file and directory creation functions.
--
-----------------------------------------------------------------------------

module System.Posix.Temp.PosixString (
        mkstemp, mkstemps, mkdtemp
    ) where

#include "HsUnix.h"

#if MIN_VERSION_filepath(1, 5, 0)
import qualified "os-string" System.OsString.Data.ByteString.Short as BC
#else
import qualified "filepath" System.OsPath.Data.ByteString.Short as BC
#endif
import Data.Word

import Foreign.C

import System.OsPath.Types
import System.IO
import System.Posix.PosixPath.FilePath
import System.OsString.Internal.Types (PosixString(..))
#if !HAVE_MKDTEMP
import System.Posix.Directory.PosixPath (createDirectory)
#endif
import System.Posix.IO.PosixString
import System.Posix.Types

#if !defined(HAVE_MKSTEMP)
import System.IO.Error ( ioeSetLocation )
import GHC.IO.Exception ( unsupportedOperation )
#endif

#if !defined(HAVE_MKSTEMP)

{-# WARNING mkstemp "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_MKSTEMP@)" #-}
mkstemp :: PosixString -> IO (PosixPath, Handle)
mkstemp _ = ioError (ioeSetLocation unsupportedOperation "mkstemp")

#else

foreign import capi unsafe "HsUnix.h mkstemp"
  c_mkstemp :: CString -> IO CInt

-- | Make a unique filename and open it for reading\/writing. The returned
-- 'PosixPath' is the (possibly relative) path of the created file, which is
-- padded with 6 random characters. The argument is the desired prefix of the
-- filepath of the temporary file to be created.
--
-- If you aren't using GHC or Hugs then this function simply wraps mktemp and
-- so shouldn't be considered safe.
mkstemp :: PosixString -> IO (PosixPath, Handle)
mkstemp (PosixString template') = do
  let template = PosixString $ template' `BC.append` (BC.pack [_X,_X,_X,_X,_X,_X])
  withFilePath template $ \ ptr -> do
    fd <- throwErrnoIfMinus1 "mkstemp" (c_mkstemp ptr)
    name <- peekFilePath ptr
    h <- fdToHandle (Fd fd)
    return (name, h)

#endif // HAVE_MKSTEMP

#if HAVE_MKSTEMPS
foreign import capi unsafe "HsUnix.h mkstemps"
  c_mkstemps :: CString -> CInt -> IO CInt
#endif

-- |'mkstemps' - make a unique filename with a given prefix and suffix
-- and open it for reading\/writing (only safe on GHC & Hugs).
-- The returned 'PosixPath' is the (possibly relative) path of
-- the created file, which contains  6 random characters in between
-- the prefix and suffix.
mkstemps :: PosixString -> PosixString -> IO (PosixPath, Handle)
#if HAVE_MKSTEMPS
mkstemps (PosixString prefix) (PosixString suffix) = do
  let template = PosixString $ prefix `BC.append` (BC.pack [_X,_X,_X,_X,_X,_X]) `BC.append` suffix
      lenOfsuf = (fromIntegral $ BC.length suffix) :: CInt
  withFilePath template $ \ ptr -> do
    fd <- throwErrnoIfMinus1 "mkstemps" (c_mkstemps ptr lenOfsuf)
    name <- peekFilePath ptr
    h <- fdToHandle (Fd fd)
    return (name, h)
#else
mkstemps = error "System.Posix.Temp.mkstemps: not available on this platform"
#endif

#if HAVE_MKDTEMP
foreign import capi unsafe "HsUnix.h mkdtemp"
  c_mkdtemp :: CString -> IO CString
#endif

-- | Make a unique directory. The returned 'PosixPath' is the path of the
-- created directory, which is padded with 6 random characters. The argument is
-- the desired prefix of the filepath of the temporary directory to be created.
--
-- If you aren't using GHC or Hugs then this function simply wraps mktemp and
-- so shouldn't be considered safe.
mkdtemp :: PosixString -> IO PosixPath
mkdtemp (PosixString template') = do
  let template = PosixString $ template' `BC.append` (BC.pack [_X,_X,_X,_X,_X,_X])
#if HAVE_MKDTEMP
  withFilePath template $ \ ptr -> do
    _ <- throwErrnoIfNull "mkdtemp" (c_mkdtemp ptr)
    name <- peekFilePath ptr
    return name
#else
  name <- mktemp template
  h <- createDirectory name (toEnum 0o700)
  return name
#endif

#if !HAVE_MKDTEMP

foreign import ccall unsafe "mktemp"
  c_mktemp :: CString -> IO CString

-- | Make a unique file name It is required that the template have six trailing
-- \'X\'s. This function should be considered deprecated.
{-# WARNING mktemp "This function is unsafe; use mkstemp instead" #-}
mktemp :: PosixString -> IO PosixPath
mktemp template = do
  withFilePath template $ \ ptr -> do
    ptr <- throwErrnoIfNull "mktemp" (c_mktemp ptr)
    peekFilePath ptr
#endif

_X :: Word8
_X = 0x58

