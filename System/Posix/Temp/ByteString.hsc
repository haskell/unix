{-# LANGUAGE ForeignFunctionInterface #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Temp.ByteString
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

module System.Posix.Temp.ByteString (
        mkstemp, mkstemps, mkdtemp
    ) where

#include "HsUnix.h"

import Control.Exception (throwIO)

import System.IO
import System.Posix.IO
import System.Posix.Types
#if !defined(__GLASGOW_HASKELL__) && !defined(__HUGS__)
import System.Posix.Directory (createDirectory)
#endif

import Foreign.C

import System.Posix.ByteString.FilePath

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC


-- | Make a unique filename and open it for reading\/writing. The returned
-- 'RawFilePath' is the (possibly relative) path of the created file, which is
-- padded with 6 random characters. The argument is the desired prefix of the
-- filepath of the temporary file to be created.
--
-- If you aren't using GHC or Hugs then this function simply wraps mktemp and
-- so shouldn't be considered safe.
mkstemp :: ByteString -> IO (RawFilePath, Handle)
mkstemp template' = do
  let template = template' `B.append` (BC.pack "XXXXXX")
#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
  withFilePath template $ \ ptr -> do
    fd <- throwErrnoIfMinus1 "mkstemp" (c_mkstemp ptr)
    name <- peekFilePath ptr
    h <- fdToHandle (Fd fd)
    return (name, h)
#else
  name <- mktemp template
  h <- openFile (BC.unpack name) ReadWriteMode
  return (name, h)
#endif

#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
foreign import ccall unsafe "HsUnix.h __hscore_mkstemp"
  c_mkstemp :: CString -> IO CInt
#endif

-- |'mkstemps' - make a unique filename with a given prefix and suffix 
-- and open it for reading\/writing (only safe on GHC & Hugs).
-- The returned 'RawFilePath' is the (possibly relative) path of
-- the created file, which contains  6 random characters in between
-- the prefix and suffix.
mkstemps :: ByteString -> ByteString -> IO (RawFilePath, Handle)
mkstemps prefix suffix = do
#if HAVE_MKSTEMPS
  let template = prefix `B.append` (BC.pack "XXXXXX") `B.append` suffix
      lenOfsuf :: CInt
      lenOfsuf = fromIntegral $ B.length suffix
  withFilePath template $ \ ptr -> do
    fd <- throwErrnoIfMinus1 "mkstemps" (c_mkstemps ptr lenOfsuf)
    name <- peekFilePath ptr
    h <- fdToHandle (Fd fd)
    return (name, h)
#else
  throwIO . userError $ "mkstemps: System does not have a mkstemp C function." 
#endif

#if HAVE_MKSTEMPS
foreign import ccall unsafe "HsUnix.h __hscore_mkstemps"
  c_mkstemps :: CString -> CInt -> IO CInt
#endif

-- | Make a unique directory. The returned 'RawFilePath' is the path of the
-- created directory, which is padded with 6 random characters. The argument is
-- the desired prefix of the filepath of the temporary directory to be created.
--
-- If you aren't using GHC or Hugs then this function simply wraps mktemp and
-- so shouldn't be considered safe.
mkdtemp :: ByteString -> IO RawFilePath
mkdtemp template' = do
  let template = template' `B.append` (BC.pack "XXXXXX")
#if HAVE_MKDTEMP
  withFilePath template $ \ ptr -> do
    _ <- throwErrnoIfNull "mkdtemp" (c_mkdtemp ptr)
    name <- peekFilePath ptr
    return name
#else
  name <- mktemp template
  h <- createDirectory (BC.unpack name) (toEnum 0o700)
  return name
#endif

#if HAVE_MKDTEMP
foreign import ccall unsafe "HsUnix.h __hscore_mkdtemp"
  c_mkdtemp :: CString -> IO CString
#endif

#if (!defined(__GLASGOW_HASKELL__) && !defined(__HUGS__)) || !HAVE_MKDTEMP
-- | Make a unique file name It is required that the template have six trailing
-- \'X\'s. This function should be considered deprecated.
{-# WARNING mktemp "This function is unsafe; use mkstemp instead" #-}
mktemp :: ByteString -> IO RawFilePath
mktemp template = do
  withFilePath template $ \ ptr -> do
    ptr <- throwErrnoIfNull "mktemp" (c_mktemp ptr)
    peekFilePath ptr

foreign import ccall unsafe "mktemp"
  c_mktemp :: CString -> IO CString
#endif

