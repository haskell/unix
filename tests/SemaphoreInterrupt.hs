module Main (main) where

import Control.Concurrent
import Control.Monad
import Data.IORef
import System.Posix

main :: IO ()
main = do

  sem <- semOpen "/test-interrupt" OpenSemFlags {semCreate = True, semExclusive = False} stdFileMode 0
  ref <- newIORef False
  _ <- forkIO $ do
    res <- semWaitInterruptible sem
    writeIORef ref res
  threadDelay 100000 -- 100 ms
  semPost sem
  threadDelay 100000 -- 100 ms
  succ1 <- readIORef ref
  unless succ1 $
    error "SemaphoreInterrupt: semWaitInterruptible failed"

  writeIORef ref False
  tid <- forkIO $ do
    res <- semWaitInterruptible sem
    writeIORef ref res
  threadDelay 100000 -- 100 ms
  killThread tid
  threadDelay 100000 -- 100 ms
  succ2 <- readIORef ref
  when succ2 $
    error "SemaphoreInterrupt: semWaitInterruptible not interrupted"
