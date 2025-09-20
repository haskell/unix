{-# LANGUAGE LambdaCase #-}

#include "HsUnixConfig.h"

module Main (main) where
#if defined(NO_DTIR_TEST)
main :: IO ()
main = do
    putStrLn "Skipping DirEnt test, since kernel seems to always return DT_UNKNOWN"
#else

import Control.Exception                (bracket, finally)
import Foreign.C.String                 (peekCString)
import System.Exit
import System.Posix.Directory
import System.Posix.Directory.Internals

peekDirEnt :: DirEnt -> IO (String, DirType)
peekDirEnt dirEnt = do
    dName <- dirEntName dirEnt >>= peekCString
    dType <- dirEntType dirEnt
    return (dName, dType)

testDirTypeOfDot :: DirStream -> IO ()
testDirTypeOfDot dirStream = go where
    go = readDirStreamWith peekDirEnt dirStream >>= \case
        Just (".", DirectoryType) -> do
            putStrLn "Got DirectoryType for . dir"
            exitSuccess
        Just (".", dType) -> die $ "Got " ++ show dType ++ " for . dir!"
        Just _ -> go
        Nothing -> die "Read cwd in Haskell and didn't find . dir!"

main :: IO ()
main = do
    putStrLn "Running Haskell test of dirEntType"
    bracket (openDirStream ".") closeDirStream testDirTypeOfDot
#endif
