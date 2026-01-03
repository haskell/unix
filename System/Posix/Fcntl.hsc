{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Fcntl
-- Copyright   :  (c) The University of Glasgow 2014
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX file control support
--
-- @since 2.7.1.0
-----------------------------------------------------------------------------

#include "HsUnix.h"
#include <fcntl.h>

module System.Posix.Fcntl (
    -- * File allocation
    Advice(..), fileAdvise,
    fileAllocate,
    -- * File caching
    fileGetCaching,
    fileSetCaching,
  ) where

import Foreign.C
import System.Posix.Types

#if !HAVE_POSIX_FALLOCATE || !HAVE_DECL_O_DIRECT
import System.IO.Error ( ioeSetLocation )
import GHC.IO.Exception ( unsupportedOperation )
#endif

#if HAVE_DECL_O_DIRECT
import Data.Bits (complement, (.&.), (.|.))
import System.Posix.Internals (c_fcntl_read)
#endif

#if HAVE_DECL_O_DIRECT || HAVE_DECL_F_NOCACHE
import System.Posix.Internals (c_fcntl_write)
#endif

-- -----------------------------------------------------------------------------
-- File control

-- | Advice parameter for 'fileAdvise' operation.
--
-- For more details, see documentation of @posix_fadvise(2)@.
--
-- @since 2.7.1.0
data Advice
  = AdviceNormal
  | AdviceRandom
  | AdviceSequential
  | AdviceWillNeed
  | AdviceDontNeed
  | AdviceNoReuse
  deriving Eq

-- | Performs @posix_fadvise(2)@ operation on file-descriptor.
--
-- If platform does not provide @posix_fadvise(2)@ 'fileAdvise'
-- becomes a no-op.
--
-- (use @#if HAVE_POSIX_FADVISE@ CPP guard to detect availability)
--
-- @since 2.7.1.0
fileAdvise :: Fd -> FileOffset -> FileOffset -> Advice -> IO ()
#if HAVE_POSIX_FADVISE
fileAdvise fd off len adv = do
  throwErrnoIfMinus1_ "fileAdvise" (c_posix_fadvise (fromIntegral fd) (fromIntegral off) (fromIntegral len) (packAdvice adv))

foreign import capi safe "fcntl.h posix_fadvise"
  c_posix_fadvise :: CInt -> COff -> COff -> CInt -> IO CInt

packAdvice :: Advice -> CInt
packAdvice AdviceNormal     = (#const POSIX_FADV_NORMAL)
packAdvice AdviceRandom     = (#const POSIX_FADV_RANDOM)
packAdvice AdviceSequential = (#const POSIX_FADV_SEQUENTIAL)
packAdvice AdviceWillNeed   = (#const POSIX_FADV_WILLNEED)
packAdvice AdviceDontNeed   = (#const POSIX_FADV_DONTNEED)
packAdvice AdviceNoReuse    = (#const POSIX_FADV_NOREUSE)
#else
fileAdvise _ _ _ _ = return ()
#endif

-- | Performs @posix_fallocate(2)@ operation on file-descriptor.
--
-- Throws 'IOError' (\"unsupported operation\") if platform does not
-- provide @posix_fallocate(2)@.
--
-- (use @#if HAVE_POSIX_FALLOCATE@ CPP guard to detect availability).
--
-- @since 2.7.1.0
fileAllocate :: Fd -> FileOffset -> FileOffset -> IO ()
#if HAVE_POSIX_FALLOCATE
fileAllocate fd off len = do
  ret <- c_posix_fallocate (fromIntegral fd) (fromIntegral off) (fromIntegral len)
  if ret == 0
    then pure ()
    else ioError (errnoToIOError "fileAllocate" (Errno ret) Nothing Nothing)

foreign import capi safe "fcntl.h posix_fallocate"
  c_posix_fallocate :: CInt -> COff -> COff -> IO CInt
#else
{-# WARNING fileAllocate
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_POSIX_FALLOCATE@)" #-}
fileAllocate _ _ _ = ioError (ioeSetLocation unsupportedOperation
                              "fileAllocate")
#endif

-- -----------------------------------------------------------------------------
-- File caching

-- | Performs the @fcntl(2)@ operation on a file-desciptor to get the cache mode.
--
-- If the cache mode is 'False', then cache effects for file system reads and
-- writes are minimised or otherwise eliminated. If the cache mode is 'True',
-- then cache effects occur like normal.
--
-- On Linux, FreeBSD, and NetBSD this checks whether the @O_DIRECT@ file flag is
-- set.
--
-- Throws 'IOError' (\"unsupported operation\") if platform does not support
-- getting the cache mode.
--
-- Use @#if HAVE_DECL_O_DIRECT@ CPP guard to detect availability. Use @#include
-- "HsUnix.h"@ to bring @HAVE_DECL_O_DIRECT@ into scope.
--
-- @since 2.8.7.0
fileGetCaching :: Fd -> IO Bool
#if HAVE_DECL_O_DIRECT
fileGetCaching (Fd fd) = do
    r <- throwErrnoIfMinus1 "fileGetCaching" (c_fcntl_read fd #{const F_GETFL})
    return ((r .&. opt_val) == 0)
  where
    opt_val = #{const O_DIRECT}
#else
{-# WARNING fileGetCaching
     "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_DECL_O_DIRECT@)" #-}
fileGetCaching _ = ioError (ioeSetLocation unsupportedOperation "fileGetCaching")
#endif

-- | Performs the @fcntl(2)@ operation on a file-desciptor to set the cache
-- mode.
--
-- If the cache mode is 'False', then cache effects for file system reads and
-- writes are minimised or otherwise eliminated. If the cache mode is 'True',
-- then cache effects occur like normal.
--
-- On Linux, FreeBSD, and NetBSD this sets the @O_DIRECT@ file flag. On OSX,
-- this sets the @F_NOCACHE@ @fcntl@ flag.
--
-- Throws 'IOError' (\"unsupported operation\") if platform does not support
-- setting the cache mode.
--
-- Use @#if HAVE_DECL_O_DIRECT || HAVE_DECL_F_NOCACHE@ CPP guard to detect availability.
-- Use @#include "HsUnix.h"@ to bring @HAVE_DECL_O_DIRECT@ and @HAVE_DECL_F_NOCACHE@ into
-- scope.
--
-- @since 2.8.7.0
fileSetCaching :: Fd -> Bool -> IO ()
#if HAVE_DECL_O_DIRECT
fileSetCaching (Fd fd) val = do
    r <- throwErrnoIfMinus1 "fileSetCaching" (c_fcntl_read fd #{const F_GETFL})
    let r' | val       = fromIntegral r .&. complement opt_val
           | otherwise = fromIntegral r .|. opt_val
    throwErrnoIfMinus1_ "fileSetCaching" (c_fcntl_write fd #{const F_SETFL} r')
  where
    opt_val = #{const O_DIRECT}
#elif HAVE_DECL_F_NOCACHE
fileSetCaching (Fd fd) val = do
    throwErrnoIfMinus1_ "fileSetCaching" (c_fcntl_write fd #{const F_NOCACHE} (if val then 0 else 1))
#else
{-# WARNING fileSetCaching
     "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_DECL_O_DIRECT || HAVE_DECL_F_NOCACHE @)" #-}
fileSetCaching _ _ = ioError (ioeSetLocation unsupportedOperation "fileSetCaching")
#endif
