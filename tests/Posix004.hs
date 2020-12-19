module Main where

import System.Exit
import System.Posix.Process
import System.Posix.Signals

main :: IO ()
main = do
    test1
    test2
    test3
    test4

test1 :: IO ()
test1 = do
    -- Force SIGFPE exceptions to not be ignored.  Under some
    -- circumstances this test will be run with SIGFPE
    -- ignored, see #7399
    _ <- installHandler sigFPE Default Nothing
    _ <- forkProcess $ raiseSignal floatingPointException
    Just (_, tc) <- getAnyProcessStatus True False
    case tc of
        Terminated sig _ | sig == floatingPointException -> return ()
        _ -> error "unexpected termination cause"

test2 :: IO ()
test2 = do
    _ <- forkProcess $ exitImmediately (ExitFailure 42)
    Just (_, tc) <- getAnyProcessStatus True False
    case tc of
        Exited (ExitFailure 42) -> return ()
        _ -> error "unexpected termination cause (2)"

test3 :: IO ()
test3 = do
    _ <- forkProcess $ exitImmediately ExitSuccess
    Just (_, tc) <- getAnyProcessStatus True False
    case tc of
        Exited ExitSuccess -> return ()
        _ -> error "unexpected termination cause (3)"

test4 :: IO ()
test4 = do
    _ <- forkProcess $ raiseSignal softwareStop
    Just (pid, tc) <- getAnyProcessStatus True True
    case tc of
        Stopped sig | sig == softwareStop -> do
            signalProcess killProcess pid
            Just (_, tc') <- getAnyProcessStatus True True
            case tc' of
                Terminated sig' _ | sig' == killProcess -> return ()
                _ -> error "unexpected termination cause (5)"
        _ -> error "unexpected termination cause (4)"

