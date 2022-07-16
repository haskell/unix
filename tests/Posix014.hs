{-# OPTIONS_GHC -Wno-deprecations #-}
-- !! Basic pipe usage
module Main (main) where

import Control.Monad
import System.Posix

main :: IO ()
main = do
  let str = "Hi, there - forked child calling"
  (rd, wd) <- createPipe
  _ <- forkProcess $ void $ fdWrite wd str
  (str', _) <- fdRead rd (fromIntegral (length str))
  unless (str == str') $
    error "should have received an identical string"
