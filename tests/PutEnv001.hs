{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -O0 -Wno-name-shadowing #-}

module Main (main) where

import Data.String ( fromString )
import System.Mem
import System.Posix.Env.ByteString
import Test.Tasty
import Test.Tasty.HUnit

-- test regression of incorrect 'free': https://github.com/haskell/unix/issues/68#issue-170072591
main :: IO ()
main = do
  putEnv "foo=bar"
  defaultMain $ testGroup "All" [ test ]

test :: TestTree
test = testCase "putEnv" $ do
  performMinorGC
  env <- System.Posix.Env.ByteString.getEnv (fromString "foo")
  performMinorGC
  print env
  env <- System.Posix.Env.ByteString.getEnv (fromString "foo")
  performMinorGC
  print env
  env <- System.Posix.Env.ByteString.getEnv (fromString "foo")
  performMinorGC
  print env
  env <- System.Posix.Env.ByteString.getEnv (fromString "foo")
  print env
  env @?= Just (fromString "bar")
