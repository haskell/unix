{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.ByteString.FilePath
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

module System.Posix.ByteString.FilePath (
     RawFilePath, withFilePath, peekFilePath, peekFilePathLen,
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

import Control.Monad
import Control.Exception
import Data.ByteString.Internal (c_strlen)
import GHC.Foreign as GHC ( peekCStringLen )
import GHC.IO.Encoding ( getFileSystemEncoding )
import GHC.IO.Exception
import Data.ByteString as B
import Data.ByteString.Char8 as BC
import Prelude hiding (FilePath)
#if !MIN_VERSION_base(4, 11, 0)
import Data.Monoid ((<>))
#endif

-- | A literal POSIX file path
type RawFilePath = ByteString

withFilePath :: RawFilePath -> (CString -> IO a) -> IO a
withFilePath path = useAsCStringSafe path

peekFilePath :: CString -> IO RawFilePath
peekFilePath = packCString

peekFilePathLen :: CStringLen -> IO RawFilePath
peekFilePathLen = packCStringLen


throwErrnoPathIfMinus1Retry :: (Eq a, Num a)
                            => String -> RawFilePath -> IO a -> IO a
throwErrnoPathIfMinus1Retry loc path f = do
  throwErrnoPathIfRetry (== -1) loc path f

throwErrnoPathIfMinus1Retry_ :: (Eq a, Num a)
                             => String -> RawFilePath -> IO a -> IO ()
throwErrnoPathIfMinus1Retry_ loc path f =
  void $ throwErrnoPathIfRetry (== -1) loc path f

throwErrnoPathIfNullRetry :: String -> RawFilePath -> IO (Ptr a) -> IO (Ptr a)
throwErrnoPathIfNullRetry loc path f =
  throwErrnoPathIfRetry (== nullPtr) loc path f

throwErrnoPathIfRetry :: (a -> Bool) -> String -> RawFilePath -> IO a -> IO a
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
throwErrnoPath :: String -> RawFilePath -> IO a
throwErrnoPath loc path =
  do
    errno <- getErrno
    path' <- either (const (BC.unpack path)) id <$> try @IOException (decodeWithBasePosix path)
    ioError (errnoToIOError loc errno Nothing (Just path'))

-- | as 'throwErrnoIf', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIf :: (a -> Bool) -> String -> RawFilePath -> IO a -> IO a
throwErrnoPathIf cond loc path f =
  do
    res <- f
    if cond res then throwErrnoPath loc path else return res

-- | as 'throwErrnoIf_', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIf_ :: (a -> Bool) -> String -> RawFilePath -> IO a -> IO ()
throwErrnoPathIf_ cond loc path f  = void $ throwErrnoPathIf cond loc path f

-- | as 'throwErrnoIfNull', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIfNull :: String -> RawFilePath -> IO (Ptr a) -> IO (Ptr a)
throwErrnoPathIfNull  = throwErrnoPathIf (== nullPtr)

-- | as 'throwErrnoIfMinus1', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIfMinus1 :: (Eq a, Num a) => String -> RawFilePath -> IO a -> IO a
throwErrnoPathIfMinus1 = throwErrnoPathIf (== -1)

-- | as 'throwErrnoIfMinus1_', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIfMinus1_ :: (Eq a, Num a) => String -> RawFilePath -> IO a -> IO ()
throwErrnoPathIfMinus1_  = throwErrnoPathIf_ (== -1)

-- | as 'throwErrnoTwoPathsIfMinus1_', but exceptions include two paths when appropriate.
--
throwErrnoTwoPathsIfMinus1_ :: (Eq a, Num a) => String -> RawFilePath -> RawFilePath -> IO a -> IO ()
throwErrnoTwoPathsIfMinus1_  loc path1 path2 action = do
    path1' <- either (const (BC.unpack path1)) id <$> try @IOException (decodeWithBasePosix path1)
    path2' <- either (const (BC.unpack path2)) id <$> try @IOException (decodeWithBasePosix path2)
    throwErrnoIfMinus1_ (loc <> " '" <> path1' <> "' to '" <> path2' <> "'") action

-- | This mimics the filepath decoder base uses on unix,
-- with the small distinction that we're not truncating at NUL bytes (because we're not at
-- the outer FFI layer).
decodeWithBasePosix :: RawFilePath -> IO String
decodeWithBasePosix ba = B.useAsCStringLen ba $ \fp -> peekFilePathPosix fp
 where
  peekFilePathPosix :: CStringLen -> IO String
  peekFilePathPosix fp = getFileSystemEncoding >>= \enc -> GHC.peekCStringLen enc fp

-- | Wrapper around 'useAsCString', checking the encoded 'FilePath' for internal NUL octets as these are
-- disallowed in POSIX filepaths. See https://gitlab.haskell.org/ghc/ghc/-/issues/13660
useAsCStringSafe :: RawFilePath -> (CString -> IO a) -> IO a
useAsCStringSafe path f = useAsCString path $ \ptr -> do
    let len = B.length path
    clen <- c_strlen ptr
    if clen == fromIntegral len
        then f ptr
        else do
          path' <- either (const (BC.unpack path)) id <$> try @IOException (decodeWithBasePosix path)
          ioError (err path')
  where
    err path' =
        IOError
          { ioe_handle = Nothing
          , ioe_type = InvalidArgument
          , ioe_location = "checkForInteriorNuls"
          , ioe_description = "POSIX filepaths must not contain internal NUL octets."
          , ioe_errno = Nothing
          , ioe_filename = Just path'
          }
