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
	mktemp: unsafe
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

-- |'mkstemp' - make a unique filename.

mkstemp :: String -> IO Handle
mkstemp template = do
  withCString template $ \ ptr -> do
    fd <- throwErrnoIfMinus1 "mkstemp" (c_mkstemp ptr)
#ifdef __GLASGOW_HASKELL__
    fdToHandle fd
#else
    closeFd fd
    name <- peekCString ptr
    openFile name ReadWriteMode
#endif

foreign import ccall unsafe "mkstemp"
  c_mkstemp :: CString -> IO Fd
