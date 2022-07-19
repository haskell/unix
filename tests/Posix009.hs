{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Main where

import Control.Monad
import System.Posix.Signals
import System.Posix.Unistd

main :: IO ()
main = do
  putStrLn "Blocking real time alarms."
  blockSignals (addSignal realTimeAlarm reservedSignals)
  putStrLn "Scheduling an alarm in 2 seconds..."
  _ <- scheduleAlarm 2
  putStrLn "Sleeping 5 seconds."
  _ <- sleep 5
  putStrLn "Woken up"
  ints <- getPendingSignals
  putStrLn "Checking pending interrupts for RealTimeAlarm"
  unless (inSignalSet realTimeAlarm ints) $
    error "should have a pending real time alarm"
