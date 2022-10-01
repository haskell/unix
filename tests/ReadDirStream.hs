module ReadDirStream
  ( emptyDirStream
  , nonEmptyDirStream
  ) where

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
  _ <- readDirStreamMaybe dir_p  -- Just "."
  _ <- readDirStreamMaybe dir_p  -- Just ".."
  ment <- readDirStreamMaybe dir_p
  closeDirStream dir_p
  cleanup
  ment @?= Nothing

nonEmptyDirStream :: IO ()
nonEmptyDirStream = do
  cleanup
  createDirectory dir ownerModes
  _ <- createFile (dir ++ "/file") ownerReadMode
  dir_p <- openDirStream dir
  -- We read three entries here since "." and "." are included in the dirstream
  one <- readDirStreamMaybe dir_p
  two <- readDirStreamMaybe dir_p
  three <- readDirStreamMaybe dir_p
  let ment = maximum [one, two, three]
  closeDirStream dir_p
  cleanup
  ment @?= Just "file"

cleanup :: IO ()
cleanup = do
    ignoreIOExceptions $ removeLink $ dir ++ "/file"
    ignoreIOExceptions $ removeDirectory dir

ignoreIOExceptions :: IO () -> IO ()
ignoreIOExceptions io = io `E.catch`
                        ((\_ -> return ()) :: E.IOException -> IO ())
