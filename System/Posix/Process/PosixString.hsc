{-# LANGUAGE PackageImports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Process.PosixString
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX process support.  See also the System.Cmd and System.Process
-- modules in the process package.
--
-----------------------------------------------------------------------------

module System.Posix.Process.PosixString (
    -- * Processes

    -- ** Forking and executing
    forkProcess,
    forkProcessWithUnmask,
    executeFile,

    -- ** Exiting
    exitImmediately,

    -- ** Process environment
    getProcessID,
    getParentProcessID,

    -- ** Process groups
    getProcessGroupID,
    getProcessGroupIDOf,
    createProcessGroupFor,
    joinProcessGroup,
    setProcessGroupIDOf,

    -- ** Sessions
    createSession,

    -- ** Process times
    ProcessTimes(..),
    getProcessTimes,

    -- ** Scheduling priority
    nice,
    getProcessPriority,
    getProcessGroupPriority,
    getUserPriority,
    setProcessPriority,
    setProcessGroupPriority,
    setUserPriority,

    -- ** Process status
    ProcessStatus(..),
    getProcessStatus,
    getAnyProcessStatus,
    getGroupProcessStatus,

    -- ** Deprecated
    createProcessGroup,
    setProcessGroupID,

 ) where

#include "HsUnix.h"

import Foreign
import System.Posix.Process.Internals
import System.Posix.Process (ProcessTimes(..), setProcessGroupID, createProcessGroup, getGroupProcessStatus, getAnyProcessStatus, getProcessStatus, setUserPriority, setProcessGroupPriority, setProcessPriority, getUserPriority, getProcessGroupPriority, getProcessPriority, nice, getProcessTimes, createSession, setProcessGroupIDOf, joinProcessGroup, createProcessGroupFor, getProcessGroupIDOf, getProcessGroupID, getParentProcessID, getProcessID, exitImmediately, forkProcessWithUnmask, forkProcess)

import Foreign.C hiding (
     throwErrnoPath,
     throwErrnoPathIf,
     throwErrnoPathIf_,
     throwErrnoPathIfNull,
     throwErrnoPathIfMinus1,
     throwErrnoPathIfMinus1_ )

import System.OsPath.Types
import System.OsString.Internal.Types (PosixString(..))
#if MIN_VERSION_filepath(1, 5, 0)
import qualified "os-string" System.OsString.Data.ByteString.Short as BC
#else
import qualified "filepath" System.OsPath.Data.ByteString.Short as BC
#endif

import System.Posix.PosixPath.FilePath

-- | @'executeFile' cmd args env@ calls one of the
--   @execv*@ family, depending on whether or not the current
--   PATH is to be searched for the command, and whether or not an
--   environment is provided to supersede the process's current
--   environment.  The basename (leading directory names suppressed) of
--   the command is passed to @execv*@ as @arg[0]@;
--   the argument list passed to 'executeFile' therefore
--   begins with @arg[1]@.
executeFile :: PosixPath                        -- ^ Command
            -> Bool                                 -- ^ Search PATH?
            -> [PosixString]                         -- ^ Arguments
            -> Maybe [(PosixString, PosixString)]     -- ^ Environment
            -> IO a
executeFile path search args Nothing = do
  withFilePath path $ \s ->
    withMany withFilePath (path:args) $ \cstrs ->
      withArray0 nullPtr cstrs $ \arr -> do
        pPrPr_disableITimers
        if search
           then throwErrnoPathIfMinus1_ "executeFile" path (c_execvp s arr)
           else throwErrnoPathIfMinus1_ "executeFile" path (c_execv s arr)
        return undefined -- never reached

executeFile path search args (Just env) = do
  withFilePath path $ \s ->
    withMany withFilePath (path:args) $ \cstrs ->
      withArray0 nullPtr cstrs $ \arg_arr ->
    let env' = map (\ (PosixString name, PosixString val) -> PosixString $ name `BC.append` (_equal `BC.cons` val)) env in
    withMany withFilePath env' $ \cenv ->
      withArray0 nullPtr cenv $ \env_arr -> do
        pPrPr_disableITimers
        if search
           then throwErrnoPathIfMinus1_ "executeFile" path
                   (c_execvpe s arg_arr env_arr)
           else throwErrnoPathIfMinus1_ "executeFile" path
                   (c_execve s arg_arr env_arr)
        return undefined -- never reached

foreign import ccall unsafe "execvp"
  c_execvp :: CString -> Ptr CString -> IO CInt

foreign import ccall unsafe "execv"
  c_execv :: CString -> Ptr CString -> IO CInt

foreign import ccall unsafe "execve"
  c_execve :: CString -> Ptr CString -> Ptr CString -> IO CInt

_equal :: Word8
_equal = 0x3d
