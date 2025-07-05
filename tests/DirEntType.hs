{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Exception                (bracket)
import Foreign.C.String                 (peekCString)
import System.Exit                      (die)
import System.Posix.Directory
import System.Posix.Directory.Internals
import Text.Printf                      (printf)

peekDirEnt :: DirEnt -> IO (String, DirType)
peekDirEnt dirEnt = do
    dName <- dirEntName dirEnt >>= peekCString
    dType <- dirEntType dirEnt
    return (dName, dType)

testDirTypeOfProcSelf :: DirStream -> IO ()
testDirTypeOfProcSelf dirStream = go where
    go = readDirStreamWith peekDirEnt dirStream >>= \case
        Just (dName, dType) -> case dName of
            "self" -> case dType of
                SymbolicLinkType -> return ()
                _                -> die $
                    printf "DirEnt of /proc/self has %s; expected %s!"
                        (show dType)
                        (show SymbolicLinkType)
            _ -> go
        Nothing -> die
            "Didn't find \"self\" DirEnt while reading /proc DirStream!"

main :: IO ()
main = bracket (openDirStream "/proc") closeDirStream testDirTypeOfProcSelf
