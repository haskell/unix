{-# OPTIONS -fffi #-}
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

-- |'mkstemp' - make a unique filename and open it for
-- reading\/writing (only safe on GHC & Hugs)

mkstemp :: String -> IO (String, Handle)
mkstemp template = do
#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
  withCString template $ \ ptr -> do
    fd <- throwErrnoIfMinus1 "mkstemp" (c_mkstemp ptr)
    name <- peekCString ptr
    h <- fdToHandle fd
    return (name, h)
#else
  name <- mktemp template
  h <- openFile name ReadWriteMode
  return (name, h)

-- |'mktemp' - make a unique file name
-- This function should be considered deprecated

mktemp :: String -> IO String
mktemp template = do
  withCString template $ \ ptr -> do
    ptr <- throwErrnoIfNull "mktemp" (c_mktemp ptr)
    peekCString ptr

foreign import ccall unsafe "mktemp"
  c_mktemp :: CString -> IO CString
#endif

foreign import ccall unsafe "mkstemp"
  c_mkstemp :: CString -> IO Fd

