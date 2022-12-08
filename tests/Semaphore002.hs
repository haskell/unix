module Main (main) where

import Control.Concurrent
import System.Posix

main :: IO ()
main = do
  sem <- semOpen "/test-002" OpenSemFlags {semCreate = True, semExclusive = False} stdFileMode 0
  _ <- forkIO $ do
      threadDelay (1000*1000)
      semPost sem

  -- This should succeed after 1 second.
  semThreadWait sem
  semPost sem
