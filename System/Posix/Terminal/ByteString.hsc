{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Terminal.ByteString
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX Terminal support
--
-----------------------------------------------------------------------------

module System.Posix.Terminal.ByteString (
  -- * Terminal support

  -- ** Terminal attributes
  TerminalAttributes,
  getTerminalAttributes,
  TerminalState(..),
  setTerminalAttributes,

  TerminalMode(..),
  withoutMode,
  withMode,
  terminalMode,
  bitsPerByte,
  withBits,

  ControlCharacter(..),
  controlChar,
  withCC,
  withoutCC,

  inputTime,
  withTime,
  minInput,
  withMinInput,

  BaudRate(..),
  inputSpeed,
  withInputSpeed,
  outputSpeed,
  withOutputSpeed,

  -- ** Terminal operations
  sendBreak,
  drainOutput,
  QueueSelector(..),
  discardData,
  FlowAction(..),
  controlFlow,

  -- ** Process groups
  getTerminalProcessGroupID,
  setTerminalProcessGroupID,

  -- ** Testing a file descriptor
  queryTerminal,
  getTerminalName,
  getControllingTerminalName,

  -- ** Pseudoterminal operations
  openPseudoTerminal,
  getSlaveTerminalName
  ) where

#include "HsUnix.h"

import Foreign
import System.Posix.Types
import System.Posix.Terminal.Common

import Foreign.C hiding (
     throwErrnoPath,
     throwErrnoPathIf,
     throwErrnoPathIf_,
     throwErrnoPathIfNull,
     throwErrnoPathIfMinus1,
     throwErrnoPathIfMinus1_ )

import System.Posix.ByteString.FilePath


-- | @getTerminalName fd@ calls @ttyname@ to obtain a name associated
--   with the terminal for @Fd@ @fd@. If @fd@ is associated
--   with a terminal, @getTerminalName@ returns the name of the
--   terminal.
getTerminalName :: Fd -> IO RawFilePath
getTerminalName (Fd fd) = do
  s <- throwErrnoIfNull "getTerminalName" (c_ttyname fd)
  peekFilePath s

foreign import ccall unsafe "ttyname"
  c_ttyname :: CInt -> IO CString

-- | @getControllingTerminalName@ calls @ctermid@ to obtain
--   a name associated with the controlling terminal for the process.  If a
--   controlling terminal exists,
--   @getControllingTerminalName@ returns the name of the
--   controlling terminal.
getControllingTerminalName :: IO RawFilePath
getControllingTerminalName = do
  s <- throwErrnoIfNull "getControllingTerminalName" (c_ctermid nullPtr)
  peekFilePath s

foreign import ccall unsafe "ctermid"
  c_ctermid :: CString -> IO CString

-- | @getSlaveTerminalName@ calls @ptsname@ to obtain the name of the
-- slave terminal associated with a pseudoterminal pair.  The file
-- descriptor to pass in must be that of the master.
getSlaveTerminalName :: Fd -> IO RawFilePath

#ifdef HAVE_PTSNAME
getSlaveTerminalName (Fd fd) = do
  s <- throwErrnoIfNull "getSlaveTerminalName" (c_ptsname fd)
  peekFilePath s

foreign import ccall unsafe "__hsunix_ptsname"
  c_ptsname :: CInt -> IO CString
#else
getSlaveTerminalName _ =
    ioError (errnoToIOError "getSlaveTerminalName" eNOSYS Nothing Nothing)
#endif

