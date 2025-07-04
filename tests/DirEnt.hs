{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Exception                (bracket, finally)
import Foreign.C.String                 (peekCString)
import System.Exit
import System.Posix.Directory
import System.Posix.Directory.Internals
import System.Process                   (system)

system_x :: String -> IO ExitCode
system_x cmd = system $ "set -x; " ++ cmd

onFailure :: IO ExitCode -> (ExitCode -> IO ()) -> IO ()
action `onFailure` after = action >>= \case
    ExitSuccess -> return ()
    ec          -> after ec
infixr 9 `onFailure`

prepareTest :: IO ()
prepareTest = do
    system_x "cc --version" `onFailure` exitWith
    system_x "[ -f tests/DirEnt.c ]" `onFailure` \ec -> do
        putStrLn "Not running tests from root of repo?"
        exitWith ec
    system_x "cc tests/DirEnt.c -o DirEnt-test" `onFailure` \_ -> do
        putStrLn "d_type not available? Skipping Haskell test"
        exitSuccess
    -- As written, this C code exits with 2 if it determines the Haskell test
    -- for broken dirEntType will be a false positive
    system_x "./DirEnt-test" `onFailure` \case
        ExitFailure 2 -> putStrLn "Skipping Haskell test" >> exitSuccess
        ec            -> exitWith ec

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
    putStrLn "Preparing Haskell test of dirEntType"
    prepareTest `finally` system_x "rm -f DirEnt-test"

    putStrLn "Running Haskell test of dirEntType"
    bracket (openDirStream ".") closeDirStream testDirTypeOfDot
