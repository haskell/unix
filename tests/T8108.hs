module Main where

import Control.Concurrent
import Control.Monad
import System.Posix

main :: IO ()
main = do
  void $ forkIO $ forever $ getGroupEntryForID 0
  void $ forkIO $ forever $ getGroupEntryForID 0
  threadDelay 3000000
