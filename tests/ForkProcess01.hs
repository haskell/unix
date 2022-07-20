-- Test that we can call exitFailure in a forked process, and have it
-- communicated properly to the parent.

module Main where

import Control.Monad
import System.Exit
import System.Posix.Process

main :: IO ()
main = do
  let exitCode = ExitFailure 72
      expected = Just (Exited exitCode)
  p <- forkProcess $ exitWith exitCode
  actual <- getProcessStatus True False p
  when (actual /= expected) $
    error $ "mismatch: expected = " ++ show expected ++ ", actual = " ++ show actual
