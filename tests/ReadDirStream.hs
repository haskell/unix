module ReadDirStream
  ( emptyDirStream
  , nonEmptyDirStream
  ) where

import System.Posix.Files
import System.Posix.Directory
import System.Posix.IO
import Control.Exception as E
import Test.Tasty.HUnit

emptyDirStream :: IO ()
emptyDirStream = do
  cleanup
  createDirectory dir ownerReadMode
  dir_p <- openDirStream dir
  entries <- readDirStreamEntries dir_p
  closeDirStream dir_p
  cleanup
  entries @?= []
  where
    dir = "emptyDirStream"

    cleanup = do
      ignoreIOExceptions $ removeDirectory dir

nonEmptyDirStream :: IO ()
nonEmptyDirStream = do
  cleanup
  createDirectory dir ownerModes
  _ <- createFile (dir ++ "/file") ownerReadMode
  dir_p <- openDirStream dir
  entries <- readDirStreamEntries dir_p
  closeDirStream dir_p
  cleanup
  entries @?= ["file"]
  where
    dir = "nonEmptyDirStream"

    cleanup = do
      ignoreIOExceptions $ removeLink $ dir ++ "/file"
      ignoreIOExceptions $ removeDirectory dir

readDirStreamEntries :: DirStream -> IO [FilePath]
readDirStreamEntries dir_p = do
  ment <- readDirStreamMaybe dir_p
  case ment of
    Nothing -> return []
    Just "." -> readDirStreamEntries dir_p
    Just ".." -> readDirStreamEntries dir_p
    Just ent -> (ent :) <$> readDirStreamEntries dir_p

ignoreIOExceptions :: IO () -> IO ()
ignoreIOExceptions io = io `E.catch`
                        ((\_ -> return ()) :: E.IOException -> IO ())
