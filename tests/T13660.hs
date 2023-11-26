{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main where

import Data.Maybe
#if !MIN_VERSION_base(4, 11, 0)
import Data.Monoid ((<>))
#endif
import GHC.IO.Exception
import System.IO.Error
import System.OsPath.Posix
import System.OsString.Internal.Types (PosixString(..))
import System.Posix.IO (defaultFileFlags, OpenFileFlags(..), OpenMode(..))
import System.Posix.ByteString.FilePath

import qualified Data.ByteString.Char8 as C
#if MIN_VERSION_filepath(1, 5, 0)
import qualified "os-string" System.OsString.Data.ByteString.Short as SBS
#else
import qualified "filepath" System.OsPath.Data.ByteString.Short as SBS
#endif
import qualified System.Posix.Env.PosixString as PS
import qualified System.Posix.IO.PosixString as PS
import qualified System.Posix.IO.ByteString as BS
import qualified System.Posix.Env.ByteString as BS


main :: IO ()
main = do
  tmp <- getTemporaryDirectory
  let fp = tmp <> fromStr' "/hello\0world"
  res  <- tryIOError $ PS.openFd fp WriteOnly df

  tmp' <- getTemporaryDirectory'
  let fp' = tmp' <> "/hello\0world"
  res' <- tryIOError $ BS.openFd fp' WriteOnly df

  case (res, res') of
    (Left e,  Left e')
      | e == fileError (_toStr fp)
      , e' == fileError (C.unpack fp') -> pure ()
      | otherwise -> fail $ "Unexpected errors: " <> show e <> "\n\t" <> show e'
    (Right _, Left _)  -> fail "System.Posix.IO.PosixString.openFd should not accept filepaths with NUL bytes"
    (Left _,  Right _) -> fail "System.Posix.IO.ByteString.openFd should not accept filepaths with NUL bytes"
    (Right _, Right _) -> fail $ "System.Posix.IO.PosixString.openFd and System.Posix.IO.ByteString.openFd" <>
                                 " should not accept filepaths with NUL bytes"

  where
    df :: OpenFileFlags
    df = defaultFileFlags{ trunc = True, creat = Just 0o666, noctty = True, nonBlock = True }

    getTemporaryDirectory :: IO PosixPath
    getTemporaryDirectory = fromMaybe (fromStr' "/tmp") <$> PS.getEnv (fromStr' "TMPDIR")

    getTemporaryDirectory' :: IO RawFilePath
    getTemporaryDirectory' = fromMaybe "/tmp" <$> BS.getEnv "TMPDIR"

    fromStr' = pack . fmap unsafeFromChar

    _toStr (PosixString sbs) = C.unpack $ SBS.fromShort sbs

    fileError fp = IOError
          { ioe_handle = Nothing
          , ioe_type = InvalidArgument
          , ioe_location = "checkForInteriorNuls"
          , ioe_description = "POSIX filepaths must not contain internal NUL octets."
          , ioe_errno = Nothing
          , ioe_filename = Just fp
          }

