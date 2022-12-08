module Main (main) where

import System.Posix

main :: IO ()
main = do
  sem <- semOpen "/test-001" OpenSemFlags {semCreate = True, semExclusive = False} stdFileMode 1
  semThreadWait sem
  semPost sem
