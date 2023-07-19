{-# LANGUAGE CApiFFI, CPP, DeriveDataTypeable, NondecreasingIndentation #-}
{-# OPTIONS_GHC -fno-cse #-} -- global variables
{-# LANGUAGE Trustworthy #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Signals
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX signal support
--
-----------------------------------------------------------------------------

#include "HsUnixConfig.h"
##include "HsUnixConfig.h"

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

module System.Posix.Signals (
  -- * The Signal type
  Signal,

  -- * Specific signals
  nullSignal,
  internalAbort, sigABRT,
  realTimeAlarm, sigALRM,
  busError, sigBUS,
  processStatusChanged, sigCHLD,
  continueProcess, sigCONT,
  floatingPointException, sigFPE,
  lostConnection, sigHUP,
  illegalInstruction, sigILL,
  keyboardSignal, sigINT,
  killProcess, sigKILL,
  openEndedPipe, sigPIPE,
  keyboardTermination, sigQUIT,
  segmentationViolation, sigSEGV,
  softwareStop, sigSTOP,
  softwareTermination, sigTERM,
  keyboardStop, sigTSTP,
  backgroundRead, sigTTIN,
  backgroundWrite, sigTTOU,
  userDefinedSignal1, sigUSR1,
  userDefinedSignal2, sigUSR2,
#if CONST_SIGPOLL != -1
  pollableEvent, sigPOLL,
#endif
  profilingTimerExpired, sigPROF,
  badSystemCall, sigSYS,
  breakpointTrap, sigTRAP,
  urgentDataAvailable, sigURG,
  virtualTimerExpired, sigVTALRM,
  cpuTimeLimitExceeded, sigXCPU,
  fileSizeLimitExceeded, sigXFSZ,

  -- * Sending signals
  raiseSignal,
  signalProcess,
  signalProcessGroup,

  -- * Handling signals
  Handler(Default,Ignore,Catch,CatchOnce,CatchInfo,CatchInfoOnce),
  SignalInfo(..), SignalSpecificInfo(..),
  installHandler,

  -- * Signal sets
  SignalSet,
  emptySignalSet, fullSignalSet, reservedSignals,
  addSignal, deleteSignal, inSignalSet,

  -- * The process signal mask
  getSignalMask, setSignalMask, blockSignals, unblockSignals,

  -- * The alarm timer
  scheduleAlarm,

  -- * Waiting for signals
  getPendingSignals,
  awaitSignal,

  -- * The @NOCLDSTOP@ flag
  setStoppedChildFlag, queryStoppedChildFlag,

  -- MISSING FUNCTIONALITY:
  -- sigaction(), (inc. the sigaction structure + flags etc.)
  -- the siginfo structure
  -- sigaltstack()
  -- sighold, sigignore, sigpause, sigrelse, sigset
  -- siginterrupt
  ) where

import Data.Word
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
#if !defined(HAVE_SIGNAL_H) || !defined(HAVE_ALARM)
import System.IO.Error ( ioeSetLocation )
import GHC.IO.Exception ( unsupportedOperation )
#endif
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types
import System.Posix.Internals
import System.Posix.Process
import System.Posix.Process.Internals
import Data.Dynamic

#if defined(javascript_HOST_ARCH)
-- These signals aren't supported by the JS RTS
-- and the header isn't exposed.
##define STG_SIG_DFL   (-1)
##define STG_SIG_IGN   (-2)
##define STG_SIG_ERR   (-3)
##define STG_SIG_HAN   (-4)
##define STG_SIG_RST   (-5)
#else
##include "rts/Signals.h"
#endif

import GHC.Conc hiding (Signal)

#if !defined(HAVE_SIGNAL_H)
import Control.Exception ( throw )
#endif

-- -----------------------------------------------------------------------------
-- Specific signals

nullSignal :: Signal
nullSignal = 0

-- | Process abort signal.
sigABRT   :: CInt
sigABRT   = CONST_SIGABRT

-- | Alarm clock.
sigALRM   :: CInt
sigALRM   = CONST_SIGALRM

-- | Access to an undefined portion of a memory object.
sigBUS    :: CInt
sigBUS    = CONST_SIGBUS

-- | Child process terminated, stopped, or continued.
sigCHLD   :: CInt
sigCHLD   = CONST_SIGCHLD

-- | Continue executing, if stopped.
sigCONT   :: CInt
sigCONT   = CONST_SIGCONT

-- | Erroneous arithmetic operation.
sigFPE    :: CInt
sigFPE    = CONST_SIGFPE

-- | Hangup.
sigHUP    :: CInt
sigHUP    = CONST_SIGHUP

-- | Illegal instruction.
sigILL    :: CInt
sigILL    = CONST_SIGILL

-- | Terminal interrupt signal.
sigINT    :: CInt
sigINT    = CONST_SIGINT

-- | Kill (cannot be caught or ignored).
sigKILL   :: CInt
sigKILL   = CONST_SIGKILL

-- | Write on a pipe with no one to read it.
sigPIPE   :: CInt
sigPIPE   = CONST_SIGPIPE

-- | Terminal quit signal.
sigQUIT   :: CInt
sigQUIT   = CONST_SIGQUIT

-- | Invalid memory reference.
sigSEGV   :: CInt
sigSEGV   = CONST_SIGSEGV

-- | Stop executing (cannot be caught or ignored).
sigSTOP   :: CInt
sigSTOP   = CONST_SIGSTOP

-- | Termination signal.
sigTERM   :: CInt
sigTERM   = CONST_SIGTERM

-- | Terminal stop signal.
sigTSTP   :: CInt
sigTSTP   = CONST_SIGTSTP

-- | Background process attempting read.
sigTTIN   :: CInt
sigTTIN   = CONST_SIGTTIN

-- | Background process attempting write.
sigTTOU   :: CInt
sigTTOU   = CONST_SIGTTOU

-- | User-defined signal 1.
sigUSR1   :: CInt
sigUSR1   = CONST_SIGUSR1

-- | User-defined signal 2.
sigUSR2   :: CInt
sigUSR2   = CONST_SIGUSR2

#if CONST_SIGPOLL != -1
-- | Pollable event.
sigPOLL   :: CInt
sigPOLL   = CONST_SIGPOLL
#endif

-- | Profiling timer expired.
sigPROF   :: CInt
sigPROF   = CONST_SIGPROF

-- | Bad system call.
sigSYS    :: CInt
sigSYS    = CONST_SIGSYS

-- | Trace/breakpoint trap.
sigTRAP   :: CInt
sigTRAP   = CONST_SIGTRAP

-- | High bandwidth data is available at a socket.
sigURG    :: CInt
sigURG    = CONST_SIGURG

-- | Virtual timer expired.
sigVTALRM :: CInt
sigVTALRM = CONST_SIGVTALRM

-- | CPU time limit exceeded.
sigXCPU   :: CInt
sigXCPU   = CONST_SIGXCPU

-- | File size limit exceeded.
sigXFSZ   :: CInt
sigXFSZ   = CONST_SIGXFSZ

-- | Alias for 'sigABRT'.
internalAbort ::Signal
internalAbort = sigABRT

-- | Alias for 'sigALRM'.
realTimeAlarm :: Signal
realTimeAlarm = sigALRM

-- | Alias for 'sigBUS'.
busError :: Signal
busError = sigBUS

-- | Alias for 'sigCHLD'.
processStatusChanged :: Signal
processStatusChanged = sigCHLD

-- | Alias for 'sigCONT'.
continueProcess :: Signal
continueProcess = sigCONT

-- | Alias for 'sigFPE'.
floatingPointException :: Signal
floatingPointException = sigFPE

-- | Alias for 'sigHUP'.
lostConnection :: Signal
lostConnection = sigHUP

-- | Alias for 'sigILL'.
illegalInstruction :: Signal
illegalInstruction = sigILL

-- | Alias for 'sigINT'.
keyboardSignal :: Signal
keyboardSignal = sigINT

-- | Alias for 'sigKILL'.
killProcess :: Signal
killProcess = sigKILL

-- | Alias for 'sigPIPE'.
openEndedPipe :: Signal
openEndedPipe = sigPIPE

-- | Alias for 'sigQUIT'.
keyboardTermination :: Signal
keyboardTermination = sigQUIT

-- | Alias for 'sigSEGV'.
segmentationViolation :: Signal
segmentationViolation = sigSEGV

-- | Alias for 'sigSTOP'.
softwareStop :: Signal
softwareStop = sigSTOP

-- | Alias for 'sigTERM'.
softwareTermination :: Signal
softwareTermination = sigTERM

-- | Alias for 'sigTSTP'.
keyboardStop :: Signal
keyboardStop = sigTSTP

-- | Alias for 'sigTTIN'.
backgroundRead :: Signal
backgroundRead = sigTTIN

-- | Alias for 'sigTTOU'.
backgroundWrite :: Signal
backgroundWrite = sigTTOU

-- | Alias for 'sigUSR1'.
userDefinedSignal1 :: Signal
userDefinedSignal1 = sigUSR1

-- | Alias for 'sigUSR2'.
userDefinedSignal2 :: Signal
userDefinedSignal2 = sigUSR2

#if CONST_SIGPOLL != -1
-- | Alias for 'sigPOLL'.
pollableEvent :: Signal
pollableEvent = sigPOLL
#endif

-- | Alias for 'sigPROF'.
profilingTimerExpired :: Signal
profilingTimerExpired = sigPROF

-- | Alias for 'sigSYS'.
badSystemCall :: Signal
badSystemCall = sigSYS

-- | Alias for 'sigTRAP'.
breakpointTrap :: Signal
breakpointTrap = sigTRAP

-- | Alias for 'sigURG'.
urgentDataAvailable :: Signal
urgentDataAvailable = sigURG

-- | Alias for 'sigVTALRM'.
virtualTimerExpired :: Signal
virtualTimerExpired = sigVTALRM

-- | Alias for 'sigXCPU'.
cpuTimeLimitExceeded :: Signal
cpuTimeLimitExceeded = sigXCPU

-- | Alias for 'sigXFSZ'.
fileSizeLimitExceeded :: Signal
fileSizeLimitExceeded = sigXFSZ

-- -----------------------------------------------------------------------------
-- Signal-related functions

-- | @signalProcess int pid@ calls @kill@ to signal process @pid@
--   with interrupt signal @int@.
signalProcess :: Signal -> ProcessID -> IO ()
#if !defined(HAVE_SIGNAL_H)

{-# WARNING signalProcess
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_SIGNAL_H@)" #-}
signalProcess _ _ = ioError (ioeSetLocation unsupportedOperation "signalProcess")

#else

signalProcess sig pid
 = throwErrnoIfMinus1_ "signalProcess" (c_kill pid sig)

foreign import ccall unsafe "kill"
  c_kill :: CPid -> CInt -> IO CInt

#endif // HAVE_SIGNAL_H


-- | @signalProcessGroup int pgid@ calls @kill@ to signal
--  all processes in group @pgid@ with interrupt signal @int@.
signalProcessGroup :: Signal -> ProcessGroupID -> IO ()
#if !defined(HAVE_SIGNAL_H)

{-# WARNING signalProcessGroup
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_SIGNAL_H@)" #-}
signalProcessGroup _ _ = ioError (ioeSetLocation unsupportedOperation "signalProcessGroup")

#else

signalProcessGroup sig pgid
  = throwErrnoIfMinus1_ "signalProcessGroup" (c_killpg pgid sig)

foreign import ccall unsafe "killpg"
  c_killpg :: CPid -> CInt -> IO CInt

#endif // HAVE_SIGNAL_H

-- | @raiseSignal int@ calls @kill@ to signal the current process
--   with interrupt signal @int@.
raiseSignal :: Signal -> IO ()
#if !defined(HAVE_SIGNAL_H)

{-# WARNING raiseSignal
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_SIGNAL_H@)" #-}
raiseSignal _ = ioError (ioeSetLocation unsupportedOperation "raiseSignal")

#else

raiseSignal sig = throwErrnoIfMinus1_ "raiseSignal" (c_raise sig)

-- See also note in GHC's rts/RtsUtils.c
-- This is somewhat fragile because we need to keep the
-- `#if`-conditional in sync with GHC's runtime.
#if (defined(openbsd_HOST_OS) || defined(freebsd_HOST_OS) || defined(dragonfly_HOST_OS) || defined(netbsd_HOST_OS) || defined(darwin_HOST_OS))
foreign import ccall unsafe "genericRaise"
  c_raise :: CInt -> IO CInt
#else
foreign import ccall unsafe "raise"
  c_raise :: CInt -> IO CInt
#endif

#endif // HAVE_SIGNAL_H

type Signal = CInt

-- | The actions to perform when a signal is received.
data Handler = Default
                 -- ^ Sets the disposition of the signal to @SIG_DFL@, which
                 -- means we want the default action associated with the
                 -- signal. For example, the default action for @SIGTERM@ (and
                 -- various other signals) is to terminate the process.
             | Ignore
                 -- ^ Set the disposition of the signal to @SIG_IGN@, which
                 -- means we want to /ignore/ the signal.  Ignored signals will
                 -- not be delivered to the process, and if also /blocked/ will
                 -- not be added to the pending set for later delivery (if/when
                 -- unblocked).  Some signals (e.g. @SIGSTOP@ and @SIGKILL@)
                 -- cannot be caught or ignored.
             -- not yet: | Hold
             | Catch (IO ())
                 -- ^ signal handler is not reset
             | CatchOnce (IO ())
                 -- ^ signal handler is automatically reset (via @SA_RESETHAND@)
             | CatchInfo (SignalInfo -> IO ())     -- ^ @since 2.7.0.0
             | CatchInfoOnce (SignalInfo -> IO ()) -- ^ @since 2.7.0.0
  deriving (Typeable)

-- | Information about a received signal (derived from @siginfo_t@).
--
-- @since 2.7.0.0
data SignalInfo = SignalInfo {
      siginfoSignal   :: Signal,
      siginfoError    :: Errno,
      siginfoSpecific :: SignalSpecificInfo
  }

-- | Information specific to a particular type of signal
-- (derived from @siginfo_t@).
--
-- @since 2.7.0.0
data SignalSpecificInfo
  = NoSignalSpecificInfo
  | SigChldInfo {
      siginfoPid    :: ProcessID,
      siginfoUid    :: UserID,
      siginfoStatus :: ProcessStatus
    }

-- | @installHandler int handler iset@ calls @sigaction@ to install an
--   interrupt handler for signal @int@.  If @handler@ is @Default@,
--   @SIG_DFL@ is installed; if @handler@ is @Ignore@, @SIG_IGN@ is
--   installed; if @handler@ is @Catch action@, a handler is installed
--   which will invoke @action@ in a new thread when (or shortly after) the
--   signal is received.
--   If @iset@ is @Just s@, then the @sa_mask@ of the @sigaction@ structure
--   is set to @s@; otherwise it is cleared.  The previously installed
--   signal handler for @int@ is returned
installHandler :: Signal
               -> Handler
               -> Maybe SignalSet       -- ^ other signals to block
               -> IO Handler            -- ^ old handler

#ifdef __PARALLEL_HASKELL__
{-# WARNING installHandler "installHandler: not available for Parallel Haskell"
 - #-}
installHandler =
  error "installHandler: not available for Parallel Haskell"
#elif !defined(HAVE_SIGNAL_H)

{-# WARNING installHandler
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_SIGNAL_H@)" #-}
installHandler _ _ _ = ioError (ioeSetLocation unsupportedOperation "installHandler")

#else

installHandler sig handler _maybe_mask = do
  ensureIOManagerIsRunning  -- for the threaded RTS

  -- if we're setting the action to DFL or IGN, we should do that *first*
  -- if we're setting a handler,
  --   if the previous action was handle, then setHandler is ok
  --   if the previous action was IGN/DFL, then setHandler followed by sig_install
  (old_action, old_handler) <-
    case handler of
      Ignore  -> do
        old_action  <- stg_sig_install sig STG_SIG_IGN nullPtr
        old_handler <- setHandler sig Nothing
        return (old_action, old_handler)

      Default -> do
        old_action  <- stg_sig_install sig STG_SIG_DFL nullPtr
        old_handler <- setHandler sig Nothing
        return (old_action, old_handler)

      _some_kind_of_catch -> do
        -- I don't think it's possible to get CatchOnce right.  If
        -- there's a signal in flight, then we might run the handler
        -- more than once.
        let dyn = toDyn handler
        old_handler <- case handler of
            Catch         action -> setHandler sig (Just (const action,dyn))
            CatchOnce     action -> setHandler sig (Just (const action,dyn))
            CatchInfo     action -> setHandler sig (Just (getinfo action,dyn))
            CatchInfoOnce action -> setHandler sig (Just (getinfo action,dyn))
#if __GLASGOW_HASKELL__ < 811
            _                    -> error "installHandler"
#endif

        let action = case handler of
                Catch _         -> STG_SIG_HAN
                CatchOnce _     -> STG_SIG_RST
                CatchInfo _     -> STG_SIG_HAN
                CatchInfoOnce _ -> STG_SIG_RST
#if __GLASGOW_HASKELL__ < 811
                _               -> error "installHandler"
#endif

        old_action <- stg_sig_install sig action nullPtr
                   -- mask is pointless, so leave it NULL

        return (old_action, old_handler)

  case (old_handler,old_action) of
    (_,       STG_SIG_DFL) -> return $ Default
    (_,       STG_SIG_IGN) -> return $ Ignore
    (Nothing, _)           -> return $ Ignore
    (Just (_,dyn),  _)
        | Just h <- fromDynamic dyn  -> return h
        | Just io <- fromDynamic dyn -> return (Catch io)
        -- handlers put there by the base package have type IO ()
        | otherwise                  -> return Default

foreign import ccall unsafe
  stg_sig_install
        :: CInt                         -- sig no.
        -> CInt                         -- action code (STG_SIG_HAN etc.)
        -> Ptr CSigset                  -- (in, out) blocked
        -> IO CInt                      -- (ret) old action code

getinfo :: (SignalInfo -> IO ()) -> ForeignPtr Word8 -> IO ()
getinfo handler fp_info = do
  si <- unmarshalSigInfo fp_info
  handler si

unmarshalSigInfo :: ForeignPtr Word8 -> IO SignalInfo
unmarshalSigInfo fp = do
  withForeignPtr fp $ \p -> do
    sig   <- (#peek siginfo_t, si_signo) p
    errno <- (#peek siginfo_t, si_errno) p
    extra <- case sig of
                _ | sig == sigCHLD -> do
                    pid <- (#peek siginfo_t, si_pid) p
                    uid <- (#peek siginfo_t, si_uid) p
                    wstat <- (#peek siginfo_t, si_status) p
                    pstat <- decipherWaitStatus wstat
                    return SigChldInfo { siginfoPid = pid,
                                         siginfoUid = uid,
                                         siginfoStatus = pstat }
                _ | otherwise ->
                    return NoSignalSpecificInfo
    return
      SignalInfo {
        siginfoSignal = sig,
        siginfoError  = Errno errno,
        siginfoSpecific = extra }

#endif /* !__PARALLEL_HASKELL__ */

#if !defined(HAVE_ALARM)

scheduleAlarm :: Int -> IO Int
{-# WARNING scheduleAlarm
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_ALARM@)" #-}
scheduleAlarm _ = ioError (ioeSetLocation unsupportedOperation "scheduleAlarm")

#else

-- -----------------------------------------------------------------------------
-- Alarms

-- | @scheduleAlarm i@ calls @alarm@ to schedule a real time
--   alarm at least @i@ seconds in the future.
scheduleAlarm :: Int -> IO Int
scheduleAlarm secs = do
   r <- c_alarm (fromIntegral secs)
   return (fromIntegral r)

foreign import ccall unsafe "alarm"
  c_alarm :: CUInt -> IO CUInt

#endif // HAVE_ALARM

-- -----------------------------------------------------------------------------
-- The NOCLDSTOP flag

#if !defined(HAVE_SIGNAL_H)

{-# WARNING setStoppedChildFlag "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_SIGNAL_H@)" #-}
setStoppedChildFlag :: Bool -> IO Bool
setStoppedChildFlag _ = ioError (ioeSetLocation unsupportedOperation "setStoppedChildFlag")

{-# WARNING queryStoppedChildFlag "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_SIGNAL_H@)" #-}
queryStoppedChildFlag :: IO Bool
queryStoppedChildFlag = ioError (ioeSetLocation unsupportedOperation "queryStoppedChildFlag")

#else

foreign import ccall "&nocldstop" nocldstop :: Ptr Int

-- | Tells the system whether or not to set the @SA_NOCLDSTOP@ flag when
-- installing new signal handlers.
setStoppedChildFlag :: Bool -> IO Bool
setStoppedChildFlag b = do
    rc <- peek nocldstop
    poke nocldstop $ fromEnum (not b)
    return (rc == (0::Int))

-- | Queries the current state of the stopped child flag.
queryStoppedChildFlag :: IO Bool
queryStoppedChildFlag = do
    rc <- peek nocldstop
    return (rc == (0::Int))

#endif

-- -----------------------------------------------------------------------------
-- Manipulating signal sets

#if defined(HAVE_SIGNAL_H)

newtype SignalSet = SignalSet (ForeignPtr CSigset)

#else

data SignalSet

#endif // HAVE_SIGNAL_H

#if !defined(HAVE_SIGNAL_H)

emptySignalSet :: SignalSet
{-# WARNING emptySignalSet
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_SIGNAL_H@)" #-}
emptySignalSet = throw (ioeSetLocation unsupportedOperation "emptySignalSet")

fullSignalSet :: SignalSet
{-# WARNING fullSignalSet
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_SIGNAL_H@)" #-}
fullSignalSet = throw (ioeSetLocation unsupportedOperation "fullSignalSet")

reservedSignals :: SignalSet
{-# WARNING reservedSignals
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_SIGNAL_H@)" #-}
reservedSignals = throw (ioeSetLocation unsupportedOperation "reservedSignals")

infixr `addSignal`, `deleteSignal`
addSignal :: Signal -> SignalSet -> SignalSet
{-# WARNING addSignal
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_SIGNAL_H@)" #-}
addSignal _ _ = throw (ioeSetLocation unsupportedOperation "addSignal")

deleteSignal :: Signal -> SignalSet -> SignalSet
{-# WARNING deleteSignal
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_SIGNAL_H@)" #-}
deleteSignal _ _ = throw (ioeSetLocation unsupportedOperation "deleteSignal")

inSignalSet :: Signal -> SignalSet -> Bool
{-# WARNING inSignalSet
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_SIGNAL_H@)" #-}
inSignalSet _ _ = throw (ioeSetLocation unsupportedOperation "inSignalSet")

#else

emptySignalSet :: SignalSet
emptySignalSet = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes sizeof_sigset_t
  throwErrnoIfMinus1_ "emptySignalSet" (withForeignPtr fp $ c_sigemptyset)
  return (SignalSet fp)

fullSignalSet :: SignalSet
fullSignalSet = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes sizeof_sigset_t
  throwErrnoIfMinus1_ "fullSignalSet" (withForeignPtr fp $ c_sigfillset)
  return (SignalSet fp)

-- | A set of signals reserved for use by the implementation.  In GHC, this will normally
-- include either `sigVTALRM` or `sigALRM`.
reservedSignals :: SignalSet
reservedSignals = addSignal rtsTimerSignal emptySignalSet

foreign import ccall rtsTimerSignal :: CInt

infixr `addSignal`, `deleteSignal`
addSignal :: Signal -> SignalSet -> SignalSet
addSignal sig (SignalSet fp1) = unsafePerformIO $ do
  fp2 <- mallocForeignPtrBytes sizeof_sigset_t
  withForeignPtr fp1 $ \p1 ->
    withForeignPtr fp2 $ \p2 -> do
      copyBytes p2 p1 sizeof_sigset_t
      throwErrnoIfMinus1_ "addSignal" (c_sigaddset p2 sig)
  return (SignalSet fp2)

deleteSignal :: Signal -> SignalSet -> SignalSet
deleteSignal sig (SignalSet fp1) = unsafePerformIO $ do
  fp2 <- mallocForeignPtrBytes sizeof_sigset_t
  withForeignPtr fp1 $ \p1 ->
    withForeignPtr fp2 $ \p2 -> do
      copyBytes p2 p1 sizeof_sigset_t
      throwErrnoIfMinus1_ "deleteSignal" (c_sigdelset p2 sig)
  return (SignalSet fp2)

inSignalSet :: Signal -> SignalSet -> Bool
inSignalSet sig (SignalSet fp) = unsafePerformIO $
  withForeignPtr fp $ \p -> do
    r <- throwErrnoIfMinus1 "inSignalSet" (c_sigismember p sig)
    return (r /= 0)

#endif // HAVE_SIGNAL_H

#if !defined(HAVE_SIGNAL_H)

getSignalMask :: IO SignalSet
{-# WARNING getSignalMask
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_SIGNAL_H@)" #-}
getSignalMask = ioError (ioeSetLocation unsupportedOperation "getSignalMask")

setSignalMask :: SignalSet -> IO ()
{-# WARNING setSignalMask
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_SIGNAL_H@)" #-}
setSignalMask _ = ioError (ioeSetLocation unsupportedOperation "setSignalMask")

blockSignals :: SignalSet -> IO ()
{-# WARNING blockSignals
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_SIGNAL_H@)" #-}
blockSignals _ = ioError (ioeSetLocation unsupportedOperation "blockSignals")

unblockSignals :: SignalSet -> IO ()
{-# WARNING unblockSignals
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_SIGNAL_H@)" #-}
unblockSignals _ = ioError (ioeSetLocation unsupportedOperation "unblockSignals")

#else

-- | @getSignalMask@ calls @sigprocmask@ to determine the
--   set of interrupts which are currently being blocked.
getSignalMask :: IO SignalSet
getSignalMask = do
  fp <- mallocForeignPtrBytes sizeof_sigset_t
  withForeignPtr fp $ \p ->
    throwErrnoIfMinus1_ "getSignalMask" (c_sigprocmask 0 nullPtr p)
  return (SignalSet fp)

sigProcMask :: String -> CInt -> SignalSet -> IO ()
sigProcMask fn how (SignalSet set) =
  withForeignPtr set $ \p_set ->
    throwErrnoIfMinus1_ fn (c_sigprocmask how p_set nullPtr)

-- | @setSignalMask mask@ calls @sigprocmask@ with
--   @SIG_SETMASK@ to block all interrupts in @mask@.
setSignalMask :: SignalSet -> IO ()
setSignalMask set = sigProcMask "setSignalMask" (CONST_SIG_SETMASK :: CInt) set

-- | @blockSignals mask@ calls @sigprocmask@ with
--   @SIG_BLOCK@ to add all interrupts in @mask@ to the
--  set of blocked interrupts.
blockSignals :: SignalSet -> IO ()
blockSignals set = sigProcMask "blockSignals" (CONST_SIG_BLOCK :: CInt) set

-- | @unblockSignals mask@ calls @sigprocmask@ with
--   @SIG_UNBLOCK@ to remove all interrupts in @mask@ from the
--   set of blocked interrupts.
unblockSignals :: SignalSet -> IO ()
unblockSignals set = sigProcMask "unblockSignals" (CONST_SIG_UNBLOCK :: CInt) set

#endif // HAVE_SIGNAL_H

#if !defined(HAVE_SIGNAL_H)

getPendingSignals :: IO SignalSet
{-# WARNING getPendingSignals
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_SIGNAL_H@)" #-}
getPendingSignals = ioError (ioeSetLocation unsupportedOperation "getPendingSignals")

awaitSignal :: Maybe SignalSet -> IO ()
{-# WARNING awaitSignal
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_SIGNAL_H@)" #-}
awaitSignal _ = ioError (ioeSetLocation unsupportedOperation "awaitSignal")

#else

-- | @getPendingSignals@ calls @sigpending@ to obtain
--   the set of interrupts which have been received but are currently blocked.
getPendingSignals :: IO SignalSet
getPendingSignals = do
  fp <- mallocForeignPtrBytes sizeof_sigset_t
  withForeignPtr fp $ \p ->
   throwErrnoIfMinus1_ "getPendingSignals" (c_sigpending p)
  return (SignalSet fp)

-- | @awaitSignal iset@ suspends execution until an interrupt is received.
-- If @iset@ is @Just s@, @awaitSignal@ calls @sigsuspend@, installing
-- @s@ as the new signal mask before suspending execution; otherwise, it
-- calls @sigsuspend@ with current signal mask. Note that RTS
-- scheduler signal (either 'virtualTimerExpired' or 'realTimeAlarm')
-- could cause premature termination of this call. It might be necessary to block that
-- signal before invocation of @awaitSignal@ with 'blockSignals' 'reservedSignals'.
--
-- @awaitSignal@ returns when signal was received and processed by a
-- signal handler, or if the signal could not be caught. If you have
-- installed any signal handlers with @installHandler@, it may be wise
-- to call @yield@ directly after @awaitSignal@ to ensure that the
-- signal handler runs as promptly as possible.
awaitSignal :: Maybe SignalSet -> IO ()
awaitSignal maybe_sigset = do
  fp <- case maybe_sigset of
          Nothing -> do SignalSet fp <- getSignalMask; return fp
          Just (SignalSet fp) -> return fp
  withForeignPtr fp $ \p -> do
  _ <- c_sigsuspend p
  return ()
  -- ignore the return value; according to the docs it can only ever be
  -- (-1) with errno set to EINTR.
  -- XXX My manpage says it can also return EFAULT. And why is ignoring
  -- EINTR the right thing to do?

#endif // HAVE_SIGNAL_H

#if defined(HAVE_SIGNAL_H)

foreign import ccall unsafe "sigsuspend"
  c_sigsuspend :: Ptr CSigset -> IO CInt

foreign import capi unsafe "signal.h sigdelset"
  c_sigdelset   :: Ptr CSigset -> CInt -> IO CInt

foreign import capi unsafe "signal.h sigfillset"
  c_sigfillset  :: Ptr CSigset -> IO CInt

foreign import capi unsafe "signal.h sigismember"
  c_sigismember :: Ptr CSigset -> CInt -> IO CInt

foreign import ccall unsafe "sigpending"
  c_sigpending :: Ptr CSigset -> IO CInt

#endif // HAVE_SIGNAL_H
