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
  let (ResourceLimit s) = softLimit r
  let (ResourceLimit h) = hardLimit r
  s @?= soft
  h @?= hard
