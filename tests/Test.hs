{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main (main) where

import Control.Applicative
import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import Data.List (sort)
import System.Exit
import System.IO
import System.Posix
import qualified System.Posix.Env.ByteString
import Test.Tasty
import Test.Tasty.HUnit

import qualified FileStatus
import qualified FileStatusByteString
import qualified Signals001

main :: IO ()
main = defaultMain $ testGroup "All"
  [ executeFile001
  , fileExist01
  , fileStatus
  , fileStatusByteString
  , getEnvironment01
  , getEnvironment02
  , getGroupEntry
  , getUserEntry
  , processGroup001
  , processGroup002
  , queryFdOption01
  , signals001
  , t1185
  , t3816
  , user001
  , posix002
  , posix005
  , posix006
  , posix010
  ]

executeFile001 :: TestTree
executeFile001 = testCase "executeFile001" $ do
  actual <- captureStdout $
    executeFile "echo" True ["arg1", "ar   g2"] Nothing
  actual @?= "arg1 ar   g2\n"

fileExist01 :: TestTree
fileExist01 = testCase "fileExist01" $ do
  fileExist "."
    @? "file should exist"
  not <$> fileExist "does not exist"
    @? "file should not exist"

fileStatus :: TestTree
fileStatus = testCase "fileStatus" FileStatus.main

fileStatusByteString :: TestTree
fileStatusByteString = testCase "fileStatusByteString" FileStatusByteString.main

getEnvironment01 :: TestTree
getEnvironment01 = testCase "getEnvironment01" $ do
  env <- getEnvironment
  not (null env)
    @? "environment should be non-empty"

getEnvironment02 :: TestTree
getEnvironment02 = testCase "getEnvironment02" $ do
  env <- System.Posix.Env.ByteString.getEnvironment
  not (null env)
    @? "environment should be non-empty"

getGroupEntry :: TestTree
getGroupEntry = testCase "getGroupEntry" $ do
  let act = False <$ getGroupEntryForName "thisIsNotMeantToExist"
  act `E.catch` (\(_ :: E.SomeException) -> return True)
    @? "group should not exist"

getUserEntry :: TestTree
getUserEntry = testCase "getUserEntry" $ do
  let act = False <$ getUserEntryForName "thisIsNotMeantToExist"
  act `E.catch` (\(_ :: E.SomeException) -> return True)
    @? "user should not exist"

processGroup001 :: TestTree
processGroup001 = testCase "processGroup001" $ do
  pgid <- getProcessGroupID
  pgid' <- getProcessGroupIDOf =<< getProcessID
  pgid @?= pgid'

processGroup002 :: TestTree
processGroup002 = testCase "processGroup002" $ do
  pid <- getProcessID
  ppid <- getParentProcessID
  ppgid <- getProcessGroupIDOf ppid
  -- join the parent process
  joinProcessGroup ppgid
  pgid1 <- getProcessGroupID
  ppgid @?= pgid1
  -- be a leader
  _ <- createProcessGroupFor pid
  pgid2 <- getProcessGroupID
  pid @?= fromIntegral pgid2
  -- and join the parent again
  setProcessGroupIDOf pid ppgid
  pgid3 <- getProcessGroupID
  ppgid @?= pgid3

queryFdOption01 :: TestTree
queryFdOption01 = testCase "queryFdOption01" $ do
  not <$> queryFdOption stdOutput NonBlockingRead
    @? "should be blocking"
  setFdOption stdOutput NonBlockingRead True
  queryFdOption stdOutput NonBlockingRead
    @? "should be non-blocking"

signals001 :: TestTree
signals001 = testCase "signals001" Signals001.main

t1185 :: TestTree
t1185 = testCase "T1185" $ do
  (stdinr, stdinw) <- createPipe
  (stdoutr, stdoutw) <- createPipe
  pid <- forkProcess $ do
    hw <- fdToHandle stdoutw
    hr <- fdToHandle stdinr
    closeFd stdinw
    hGetContents hr >>= hPutStr hw
    hClose hr
    hClose hw
    exitImmediately ExitSuccess
  threadDelay 100000
  closeFd stdoutw
  closeFd stdinw
  hr2 <- fdToHandle stdoutr
  hGetContents hr2 >>= putStr
  actual <- getProcessStatus True False pid
  actual @?= Just (Exited ExitSuccess)

t3816 :: TestTree
t3816 = testCase "T3816" $ do
  not . null <$> getAllGroupEntries
    @? "should be non-empty"
  not . null <$> getAllGroupEntries
    @? "should be non-empty"

user001 :: TestTree
user001 = testCase "user001" $ do
  let force act = do
        x <- act
        x @?= x
  force getRealUserID
  force getRealUserID
  force getRealGroupID
  force getEffectiveUserID
  force getEffectiveGroupID
  force getGroups
  force getEffectiveUserName
  force $ getRealGroupID >>= getGroupEntryForID
  force $ getRealGroupID >>= getGroupEntryForID >>= getGroupEntryForName . groupName
  force getAllGroupEntries
  force $ getRealUserID >>= getUserEntryForID
  force getAllUserEntries

posix002 :: TestTree
posix002 = testCase "posix002" $ do
  actual <- captureStdout $
    executeFile "printenv" True [] (Just [("ONE","1"),("TWO","2")])
  sort (lines actual) @?= ["ONE=1", "TWO=2"]

posix005 :: TestTree
posix005 = testCase "posix005" $ do
    hSetBuffering stdout NoBuffering

    setEnvironment [("one","1"),("two","2")]
    env0 <- getEnvironment
    sort env0 @?= [("one","1"),("two","2")]

    setEnv "foo" "bar" True
    env1 <- getEnvironment
    sort env1 @?= [("foo","bar"),("one","1"),("two","2")]

    setEnv "foo" "baz" True
    env2 <- getEnvironment
    sort env2 @?= [("foo","baz"),("one","1"),("two","2")]

    setEnv "fu" "bar" True
    env3 <- getEnvironment
    sort env3 @?= [("foo","baz"),("fu","bar"),("one","1"),("two","2")]

    unsetEnv "foo"
    env4 <- getEnvironment
    sort env4 @?= [("fu","bar"),("one","1"),("two","2")]

    clearEnv
    env5 <- getEnvironment
    sort env5 @?= []

posix006 :: TestTree
posix006 = testCase "posix006" $ do
  start <- epochTime
  blockSignals reservedSignals -- see #4504
  _ <- sleep 1
  finish <- epochTime
  let slept = finish - start
  (slept >= 1 && slept <= 2)
    @? "should have slept between 1 and 2"

posix010 :: TestTree
posix010 = testCase "posix010" $ do
  root <- getUserEntryForName "root"
  userName root    @?= "root"
  userID root      @?= 0
  userGroupID root @?= 0

  root' <- getUserEntryForID (userID root)
  userName root'    @?= "root"
  userID root'      @?= 0
  userGroupID root' @?= 0

  homeDirectory root @?= homeDirectory root'

-------------------------------------------------------------------------------
-- Utils

captureStdout :: IO () -> IO String
captureStdout = captureFd stdOutput

captureFd :: Fd -> IO () -> IO String
captureFd fd act = do
  (dRead, dWrite) <- createPipe
  _ <- forkProcess $ do
    _ <- dupTo dWrite fd
    act
  closeFd dWrite
  handle <- fdToHandle dRead
  hGetContents handle
