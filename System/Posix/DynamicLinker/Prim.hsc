{-# OPTIONS -fffi #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.DynamicLinker.Prim
-- Copyright   :  (c) Volker Stolz <vs@foldr.org> 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  vs@foldr.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- DLOpen and friend
--  Derived from GModule.chs by M.Weber & M.Chakravarty which is part of c2hs
--  I left the API more or less the same, mostly the flags are different.
--
-----------------------------------------------------------------------------

module System.Posix.DynamicLinker.Prim (
  -- * low level API
  c_dlopen,
  c_dlsym,
  c_dlerror,
  c_dlclose,
  -- dlAddr, -- XXX NYI
  haveRtldNext,
  haveRtldLocal,
  packRTLDFlags,
  RTLDFlags(..),
  packDL,
  DL(..)
 )

where

#include "HsUnix.h"

import Data.Bits	( (.|.) )
import Foreign.Ptr	( Ptr, FunPtr, nullPtr )
import Foreign.C.Types	( CInt )
import Foreign.C.String	( CString )

-- RTLD_NEXT madness
-- On some host (e.g. SuSe Linux 7.2) RTLD_NEXT is not visible
-- without setting _GNU_SOURCE. Since we don't want to set this
-- flag, here's a different solution: You can use the Haskell
-- function 'haveRtldNext' to check wether the flag is available
-- to you. Ideally, this will be optimized by the compiler so
-- that it should be as efficient as an #ifdef.
--    If you fail to test the flag and use it although it is
-- undefined, 'packOneModuleFlag' will bomb.
--    The same applies to RTLD_LOCAL which isn't available on
-- cygwin.

haveRtldNext :: Bool

#ifdef HAVE_RTLDNEXT
haveRtldNext = True

foreign import ccall unsafe "__hsunix_rtldNext" rtldNext :: Ptr a

#else  /* HAVE_RTLDNEXT */
haveRtldNext = False
#endif /* HAVE_RTLDNEXT */

haveRtldLocal :: Bool

#ifdef HAVE_RTLDLOCAL
haveRtldLocal = True
#else /* HAVE_RTLDLOCAL */
haveRtldLocal = False
#endif /* HAVE_RTLDLOCAL */

data RTLDFlags 
  = RTLD_LAZY
  | RTLD_NOW
  | RTLD_GLOBAL 
  | RTLD_LOCAL
    deriving (Show, Read)

foreign import ccall unsafe "dlopen" c_dlopen :: CString -> CInt -> IO (Ptr ())
foreign import ccall unsafe "dlsym"  c_dlsym  :: Ptr () -> CString -> IO (FunPtr a)
foreign import ccall unsafe "dlerror" c_dlerror :: IO CString
foreign import ccall unsafe "dlclose" c_dlclose :: (Ptr ()) -> IO CInt

packRTLDFlags :: [RTLDFlags] -> CInt
packRTLDFlags flags = foldl (\ s f -> (packRTLDFlag f) .|. s) 0 flags

packRTLDFlag :: RTLDFlags -> CInt
packRTLDFlag RTLD_LAZY = #const RTLD_LAZY

#ifdef HAVE_RTLDNOW
packRTLDFlag RTLD_NOW = #const RTLD_NOW
#else /* HAVE_RTLDNOW */
packRTLDFlag RTLD_NOW =  error "RTLD_NOW not available"
#endif /* HAVE_RTLDNOW */

#ifdef HAVE_RTLDGLOBAL
packRTLDFlag RTLD_GLOBAL = #const RTLD_GLOBAL
#else /* HAVE_RTLDGLOBAL */
packRTLDFlag RTLD_GLOBAL = error "RTLD_GLOBAL not available"
#endif

#ifdef HAVE_RTLDLOCAL
packRTLDFlag RTLD_LOCAL = #const RTLD_LOCAL
#else /* HAVE_RTLDLOCAL */
packRTLDFlag RTLD_LOCAL = error "RTLD_LOCAL not available"
#endif /* HAVE_RTLDLOCAL */

-- |Flags for 'dlsym'. Notice that @Next@ might not be available on
-- your particular platform!

data DL = Null | Next | Default | DLHandle (Ptr ()) deriving (Show)

packDL :: DL -> Ptr ()
packDL Null = nullPtr
#ifdef HAVE_RTLDNEXT
packDL Next = rtldNext
#else
packDL Next = error "RTLD_NEXT not available"
#endif
packDL Default = nullPtr
packDL (DLHandle h) = h
