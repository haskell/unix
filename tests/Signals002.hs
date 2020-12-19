module Main where

import Control.Concurrent
import Control.Monad
import System.Posix

-- !!! test blockSignals, raiseSignal, unblockSignals, getPendingSignals

main :: IO ()
main = do
  blockSignals ( userDefinedSignal1 `addSignal` emptySignalSet )
  raiseSignal userDefinedSignal1
  set <- getPendingSignals
  unless (userDefinedSignal1 `inSignalSet` set) $
    fail "signal is missing from the set"
  m <- newEmptyMVar
  _ <- installHandler userDefinedSignal1
    (Catch (putStrLn "hello" >> putMVar m ())) Nothing
  awaitSignal (Just emptySignalSet)
  takeMVar m
