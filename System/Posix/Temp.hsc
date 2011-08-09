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
-- Maintainer  :  vs@foldr.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX environment support
--
-----------------------------------------------------------------------------

module System.Posix.Temp (

    mkstemp

{- Not ported (yet?):
    tmpfile: can we handle FILE*?
    tmpnam: ISO C, should go in base?
    tempname: dito
-}

) where

#include "HsUnix.h"

import System.IO
import System.Posix.IO
import System.Posix.Types
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

-- |'mkstemp' - make a unique filename and open it for
-- reading\/writing (only safe on GHC & Hugs).
-- The returned 'FilePath' is the (possibly relative) path of
-- the created file, which is padded with 6 random characters.
mkstemp :: String -> IO (FilePath, Handle)
mkstemp template = do
#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
  withFilePath template $ \ ptr -> do
    fd <- throwErrnoIfMinus1 "mkstemp" (c_mkstemp ptr)
    name <- peekFilePath ptr
    h <- fdToHandle (Fd fd)
    return (name, h)
#else
  name <- mktemp (template ++ "XXXXXX")
  h <- openFile name ReadWriteMode
  return (name, h)

-- |'mktemp' - make a unique file name
-- This function should be considered deprecated

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

