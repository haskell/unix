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

mkstemp :: String -> IO (String, Handle)
mkstemp template = do
  withCString template $ \ ptr -> do
    fd <- throwErrnoIfMinus1 "mkstemp" (c_mkstemp ptr)
    name <- peekCString ptr
#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
    h <- fdToHandle fd
#else
    closeFd fd
    h <- openFile name ReadWriteMode
#endif
    return (name, h)

foreign import ccall unsafe "mkstemp"
  c_mkstemp :: CString -> IO Fd
