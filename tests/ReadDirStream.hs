module ReadDirStream
  ( emptyDirStream
  , nonEmptyDirStream
  , dirStreamWithTypes
  ) where

import qualified Data.List
import System.Posix.Files
import System.Posix.Directory
import System.Posix.IO
import Control.Exception as E
import Test.Tasty.HUnit

dir :: FilePath
dir = "dir"

emptyDirStream :: IO ()
emptyDirStream = do
  cleanup
  createDirectory dir ownerReadMode
  dir_p <- openDirStream dir
  entries <- readDirStreamEntries dir_p
  closeDirStream dir_p
  cleanup
  entries @?= []

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

dirStreamWithTypes :: IO ()
dirStreamWithTypes = do
  cleanup
  createDirectory dir ownerModes
  createDirectory (dir ++ "/somedir") ownerModes
  _ <- createFile (dir ++ "/somefile") ownerReadMode
  dir_p <- openDirStreamWithPath dir
  entries <- readDirStreamEntriesWithTypes dir_p
  closeDirStream (fromDirStreamWithPath dir_p)
  cleanup
  Data.List.sort entries @?= [("somedir", DirectoryType), ("somefile", RegularFileType)]

readDirStreamEntries :: DirStream -> IO [FilePath]
readDirStreamEntries dir_p = do
  ment <- readDirStreamMaybe dir_p
  case ment of
    Nothing -> return []
    Just "." -> readDirStreamEntries dir_p
    Just ".." -> readDirStreamEntries dir_p
    Just ent -> (ent :) <$> readDirStreamEntries dir_p

readDirStreamEntriesWithTypes :: DirStreamWithPath FilePath -> IO [(FilePath, DirType)]
readDirStreamEntriesWithTypes dir_p = do
  ment <- readDirStreamWithType dir_p
  case ment of
    Nothing -> return []
    Just (".", _) -> readDirStreamEntriesWithTypes dir_p
    Just ("..", _) -> readDirStreamEntriesWithTypes dir_p
    Just ent -> (ent :) <$> readDirStreamEntriesWithTypes dir_p

cleanup :: IO ()
cleanup = do
    ignoreIOExceptions $ removeLink $ dir ++ "/file"
    ignoreIOExceptions $ removeDirectory dir

ignoreIOExceptions :: IO () -> IO ()
ignoreIOExceptions io = io `E.catch`
                        ((\_ -> return ()) :: E.IOException -> IO ())
