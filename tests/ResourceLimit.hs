module Main where

import System.Posix
import Test.Tasty.HUnit

main :: IO ()
main = do
  let soft = 5
      hard = 10
  setResourceLimit ResourceCPUTime
    (ResourceLimits (ResourceLimit soft) (ResourceLimit hard))
  r <- getResourceLimit ResourceCPUTime
  soft @?= case softLimit r of
      ResourceLimit l -> l
      _               -> 0
  hard @?= case hardLimit r of
      ResourceLimit l -> l
      _               -> 0
