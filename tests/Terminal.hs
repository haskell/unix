module Main where

import System.Posix
import Test.Tasty.HUnit

main :: IO ()
main = do
  (master, slave) <- openPseudoTerminal
  orig <- getTerminalAttributes slave
  let want = withInputSpeed orig B19200
  setTerminalAttributes slave want Immediately
  post <- getTerminalAttributes slave
  closeFd slave
  closeFd master
  inputSpeed post @?= B19200
