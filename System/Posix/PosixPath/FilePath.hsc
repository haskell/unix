{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.PosixPath.FilePath
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- Internal stuff: support for ByteString FilePaths
--
-----------------------------------------------------------------------------

module System.Posix.PosixPath.FilePath (
     withFilePath, peekFilePath, peekFilePathLen,
     throwErrnoPathIfMinus1Retry,
     throwErrnoPathIfMinus1Retry_,
     throwErrnoPathIfNullRetry,
     throwErrnoPathIfRetry,
     throwErrnoPath,
     throwErrnoPathIf,
     throwErrnoPathIf_,
     throwErrnoPathIfNull,
     throwErrnoPathIfMinus1,
     throwErrnoPathIfMinus1_,
     throwErrnoTwoPathsIfMinus1_
  ) where

import Foreign hiding ( void )
import Foreign.C hiding (
     throwErrnoPath,
     throwErrnoPathIf,
     throwErrnoPathIf_,
     throwErrnoPathIfNull,
     throwErrnoPathIfMinus1,
     throwErrnoPathIfMinus1_ )

import System.OsPath.Types
import Control.Monad
import Control.Exception
import System.OsPath.Posix as PS
import System.OsPath.Data.ByteString.Short
import Prelude hiding (FilePath)
import System.OsString.Internal.Types (PosixString(..))

#if !MIN_VERSION_base(4, 11, 0)
import Data.Monoid ((<>))
#endif


withFilePath :: PosixPath -> (CString -> IO a) -> IO a
withFilePath = useAsCString . getPosixString

peekFilePath :: CString -> IO PosixPath
peekFilePath = fmap PosixString . packCString

peekFilePathLen :: CStringLen -> IO PosixPath
peekFilePathLen = fmap PosixString . packCStringLen


throwErrnoPathIfMinus1Retry :: (Eq a, Num a)
                            => String -> PosixPath -> IO a -> IO a
throwErrnoPathIfMinus1Retry loc path f = do
  throwErrnoPathIfRetry (== -1) loc path f

throwErrnoPathIfMinus1Retry_ :: (Eq a, Num a)
                             => String -> PosixPath -> IO a -> IO ()
throwErrnoPathIfMinus1Retry_ loc path f =
  void $ throwErrnoPathIfRetry (== -1) loc path f

throwErrnoPathIfNullRetry :: String -> PosixPath -> IO (Ptr a) -> IO (Ptr a)
throwErrnoPathIfNullRetry loc path f =
  throwErrnoPathIfRetry (== nullPtr) loc path f

throwErrnoPathIfRetry :: (a -> Bool) -> String -> PosixPath -> IO a -> IO a
throwErrnoPathIfRetry pr loc rpath f =
  do
    res <- f
    if pr res
      then do
        err <- getErrno
        if err == eINTR
          then throwErrnoPathIfRetry pr loc rpath f
          else throwErrnoPath loc rpath
      else return res

-- | as 'throwErrno', but exceptions include the given path when appropriate.
--
throwErrnoPath :: String -> PosixPath -> IO a
throwErrnoPath loc path =
  do
    errno <- getErrno
    path' <- either (const (_toStr path)) id <$> try @IOException (PS.decodeFS path)
    ioError (errnoToIOError loc errno Nothing (Just path'))

-- | as 'throwErrnoIf', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIf :: (a -> Bool) -> String -> PosixPath -> IO a -> IO a
throwErrnoPathIf cond loc path f =
  do
    res <- f
    if cond res then throwErrnoPath loc path else return res

-- | as 'throwErrnoIf_', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIf_ :: (a -> Bool) -> String -> PosixPath -> IO a -> IO ()
throwErrnoPathIf_ cond loc path f  = void $ throwErrnoPathIf cond loc path f

-- | as 'throwErrnoIfNull', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIfNull :: String -> PosixPath -> IO (Ptr a) -> IO (Ptr a)
throwErrnoPathIfNull  = throwErrnoPathIf (== nullPtr)

-- | as 'throwErrnoIfMinus1', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIfMinus1 :: (Eq a, Num a) => String -> PosixPath -> IO a -> IO a
throwErrnoPathIfMinus1 = throwErrnoPathIf (== -1)

-- | as 'throwErrnoIfMinus1_', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIfMinus1_ :: (Eq a, Num a) => String -> PosixPath -> IO a -> IO ()
throwErrnoPathIfMinus1_  = throwErrnoPathIf_ (== -1)

-- | as 'throwErrnoTwoPathsIfMinus1_', but exceptions include two paths when appropriate.
--
throwErrnoTwoPathsIfMinus1_ :: (Eq a, Num a) => String -> PosixPath -> PosixPath -> IO a -> IO ()
throwErrnoTwoPathsIfMinus1_ loc path1 path2 action = do
    path1' <- either (const (_toStr path1)) id <$> try @IOException (PS.decodeFS path1)
    path2' <- either (const (_toStr path2)) id <$> try @IOException (PS.decodeFS path2)
    throwErrnoIfMinus1_ (loc <> " '" <> path1' <> "' to '" <> path2' <> "'") action

_toStr :: PosixPath -> String
_toStr = fmap PS.toChar . PS.unpack
