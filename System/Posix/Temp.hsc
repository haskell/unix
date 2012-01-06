{-# LANGUAGE ForeignFunctionInterface #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Temp
-- Copyright   :  (c) Volker Stolz <vs@foldr.org>
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX temporary file and directory creation functions.
--
-----------------------------------------------------------------------------

module System.Posix.Temp (
        mkstemp, mkdtemp
    ) where

#include "HsUnix.h"

import System.IO
import System.Posix.IO
import System.Posix.Types
import System.Posix.Directory (createDirectory)
import Foreign.C

#if __GLASGOW_HASKELL__ > 700
import System.Posix.Internals (withFilePath, peekFilePath)

#elif __GLASGOW_HASKELL__ > 611
import System.Posix.Internals (withFilePath)

peekFilePath :: CString -> IO FilePath
peekFilePath = peekCString

#else
withFilePath :: FilePath -> (CString -> IO a) -> IO a
withFilePath = withCString

peekFilePath :: CString -> IO FilePath
peekFilePath = peekCString
#endif

-- | Make a unique filename and open it for reading\/writing. The returned
-- 'FilePath' is the (possibly relative) path of the created file, which is
-- padded with 6 random characters. The argument is the desired prefix of the
-- filepath of the temporary file to be created.
--
-- If you aren't using GHC or Hugs then this function simply wraps mktemp and
-- so shouldn't be considered safe.
mkstemp :: String -> IO (FilePath, Handle)
mkstemp template' = do
  let template = template' ++ "XXXXXX"
#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
  withFilePath template $ \ ptr -> do
    fd <- throwErrnoIfMinus1 "mkstemp" (c_mkstemp ptr)
    name <- peekFilePath ptr
    h <- fdToHandle (Fd fd)
    return (name, h)
#else
  name <- mktemp template
  h <- openFile name ReadWriteMode
  return (name, h)
#endif

-- | Make a unique directory. The returned 'FilePath' is the path of the
-- created directory, which is padded with 6 random characters. The argument is
-- the desired prefix of the filepath of the temporary directory to be created.
--
-- If you aren't using GHC or Hugs then this function simply wraps mktemp and
-- so shouldn't be considered safe.
mkdtemp :: String -> IO FilePath
mkdtemp template' = do
  let template = template' ++ "XXXXXX"
#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
  withFilePath template $ \ ptr -> do
    throwErrnoIfNull "mkdtemp" (c_mkdtemp ptr)
    name <- peekFilePath ptr
    return name
#else
  name <- mktemp template
  h <- createDirectory name (toEnum 0o700)
  return name

-- | Make a unique file name It is required that the template have six trailing
-- \'X\'s. This function should be considered deprecated.
{-# WARNING mktemp "This function is unsafe; use mkstemp instead" #-}
mktemp :: String -> IO String
mktemp template = do
  withFilePath template $ \ ptr -> do
    ptr <- throwErrnoIfNull "mktemp" (c_mktemp ptr)
    peekFilePath ptr

foreign import ccall unsafe "mktemp"
  c_mktemp :: CString -> IO CString
#endif

foreign import ccall unsafe "HsUnix.h __hscore_mkstemp"
  c_mkstemp :: CString -> IO CInt

foreign import ccall unsafe "HsUnix.h __hscore_mkdtemp"
  c_mkdtemp :: CString -> IO CString

