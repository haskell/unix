#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Fcntl
-- Copyright   :  (c) The University of Glasgow 2013
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX file control support
--
-----------------------------------------------------------------------------

#include "HsUnix.h"

module System.Posix.Fcntl (
    -- * File allocation
#ifdef HAVE_POSIX_FADVISE
    Advice(..), posix_fadvise,
#endif
#ifdef HAVE_POSIX_FALLOCATE
    posix_fallocate,
#endif
  ) where

import Foreign.C
import System.Posix.Types

-- -----------------------------------------------------------------------------
-- File control

#ifdef HAVE_POSIX_FADVISE
data Advice
  = AdviceNormal
  | AdviceRandom
  | AdviceSequential
  | AdviceWillNeed
  | AdviceDontNeed
  | AdviceNoReuse
  deriving Eq

posix_fadvise :: Fd -> FileOffset -> FileOffset -> Advice -> IO ()
posix_fadvise fd off len adv = do
  throwErrnoIfMinus1_ "posix_fadvise" (c_posix_fadvise (fromIntegral fd) (fromIntegral off) (fromIntegral len) (packAdvice adv))

foreign import ccall safe "posix_fadvise"
  c_posix_fadvise :: CInt -> COff -> COff -> CInt -> IO CInt

packAdvice :: Advice -> CInt
packAdvice AdviceNormal     = (#const POSIX_FADV_NORMAL)
packAdvice AdviceRandom     = (#const POSIX_FADV_RANDOM)
packAdvice AdviceSequential = (#const POSIX_FADV_SEQUENTIAL)
packAdvice AdviceWillNeed   = (#const POSIX_FADV_WILLNEED)
packAdvice AdviceDontNeed   = (#const POSIX_FADV_DONTNEED)
packAdvice AdviceNoReuse    = (#const POSIX_FADV_NOREUSE)
#endif

#ifdef HAVE_POSIX_FALLOCATE
posix_fallocate :: Fd -> FileOffset -> FileOffset -> IO ()
posix_fallocate fd off len = do
  throwErrnoIfMinus1_ "posix_fallocate" (c_posix_fallocate (fromIntegral fd) (fromIntegral off) (fromIntegral len))

foreign import ccall safe "posix_fallocate"
  c_posix_fallocate :: CInt -> COff -> COff -> IO CInt
#endif
