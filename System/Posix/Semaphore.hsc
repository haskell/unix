{-# LANGUAGE Safe #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE InterruptibleFFI #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Semaphore
-- Copyright   :  (c) Daniel Franke 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires POSIX)
--
-- POSIX named semaphore support.
--
-----------------------------------------------------------------------------

module System.Posix.Semaphore
    (OpenSemFlags(..), Semaphore(),
     semOpen, semUnlink, semWait, semWaitInterruptible, semTryWait, semThreadWait,
     semPost, semGetValue)
    where

#include "HsUnix.h"
#include <semaphore.h>
#include <fcntl.h>

import Foreign.C
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Ptr
import System.Posix.Types
import Control.Concurrent
import Data.Bits
#if !defined(HAVE_SEM_GETVALUE)
import System.IO.Error ( ioeSetLocation )
import GHC.IO.Exception ( unsupportedOperation )
#else
import Foreign.Marshal
import Foreign.Storable
#endif

#if __GLASGOW_HASKELL__ >= 902
import System.Posix.Internals (hostIsThreaded)
#else
hostIsThreaded :: Bool
hostIsThreaded = False
#endif

data OpenSemFlags = OpenSemFlags { semCreate :: Bool,
                                   -- ^ If true, create the semaphore if it
                                   --   does not yet exist.
                                   semExclusive :: Bool
                                   -- ^ If true, throw an exception if the
                                   --   semaphore already exists.
                                 }

newtype Semaphore = Semaphore (ForeignPtr ())

-- | Open a named semaphore with the given name, flags, mode, and initial
--   value.
semOpen :: String -> OpenSemFlags -> FileMode -> Int -> IO Semaphore
semOpen name flags mode value =
    let cflags = (if semCreate flags then #{const O_CREAT} else 0) .|.
                 (if semExclusive flags then #{const O_EXCL} else 0)
        semOpen' cname =
            do sem <- throwErrnoPathIfNull "semOpen" name $
                      sem_open cname (toEnum cflags) mode (toEnum value)
               fptr <- newForeignPtr sem (finalize sem)
               return $ Semaphore fptr
        finalize sem = throwErrnoPathIfMinus1_ "semOpen" name $
                       sem_close sem in
    withCAString name semOpen'

-- | Delete the semaphore with the given name.
semUnlink :: String -> IO ()
semUnlink name = withCAString name semUnlink'
    where semUnlink' cname = throwErrnoPathIfMinus1_ "semUnlink" name $
                             sem_unlink cname

-- | Lock the semaphore, blocking until it becomes available.  Since this
--   is done through a system call, this will block the *entire runtime*,
--   not just the current thread.  If this is not the behaviour you want,
--   use semThreadWait instead.
semWait :: Semaphore -> IO ()
semWait (Semaphore fptr) = withForeignPtr fptr semWait'
    where semWait' sem = throwErrnoIfMinus1Retry_ "semWait" $
                         sem_wait sem

-- | Lock the semaphore, blocking until it becomes available.
--
-- Unlike 'semWait', this wait operation can be interrupted with
-- an asynchronous exception (e.g. a call to 'throwTo' from another thread).
semWaitInterruptible :: Semaphore -> IO Bool
semWaitInterruptible (Semaphore fptr) = withForeignPtr fptr semWait'
    where semWait' sem =
            do res <- sem_wait_interruptible sem
               if res == 0 then return True
                           else do errno <- getErrno
                                   if errno == eINTR
                                     then return False
                                     else throwErrno "semWaitInterrruptible"

-- | Attempt to lock the semaphore without blocking.  Immediately return
--   False if it is not available.
semTryWait :: Semaphore -> IO Bool
semTryWait (Semaphore fptr) = withForeignPtr fptr semTrywait'
    where semTrywait' sem = do res <- sem_trywait sem
                               (if res == 0 then return True
                                else do errno <- getErrno
                                        (if errno == eINTR
                                         then semTrywait' sem
                                         else if errno == eAGAIN
                                              then return False
                                              else throwErrno "semTrywait"))

-- | Poll the semaphore until it is available, then lock it.  Unlike
--   semWait, this will block only the current thread rather than the
--   entire process.
semThreadWait :: Semaphore -> IO ()
semThreadWait sem
  -- N.B. semWait can be safely used in the case of the threaded runtime, where
  -- the safe foreign call will be performed in its own thread, thereby not
  -- blocking the process.
  | hostIsThreaded = semWait sem
  | otherwise = do
      res <- semTryWait sem
      if res then return ()
             else do yield >> semThreadWait sem

-- | Unlock the semaphore.
semPost :: Semaphore -> IO ()
semPost (Semaphore fptr) = withForeignPtr fptr semPost'
    where semPost' sem = throwErrnoIfMinus1Retry_ "semPost" $
                         sem_post sem

-- | Return the semaphore's current value.
semGetValue :: Semaphore -> IO Int
#ifdef HAVE_SEM_GETVALUE
semGetValue (Semaphore fptr) = withForeignPtr fptr semGetValue'
    where semGetValue' sem = alloca (semGetValue_ sem)


semGetValue_ :: Ptr () -> Ptr CInt -> IO Int
semGetValue_ sem ptr = do throwErrnoIfMinus1Retry_ "semGetValue" $
                            sem_getvalue sem ptr
                          cint <- peek ptr
                          return $ fromEnum cint

foreign import capi safe "semaphore.h sem_getvalue"
        sem_getvalue :: Ptr () -> Ptr CInt -> IO Int
#else
{-# WARNING semGetValue "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_SEM_GETVALUE@)" #-}
semGetValue _ = ioError (ioeSetLocation unsupportedOperation "semGetValue")
#endif

foreign import capi safe "semaphore.h sem_open"
        sem_open :: CString -> CInt -> CMode -> CUInt -> IO (Ptr ())
foreign import capi safe "semaphore.h sem_close"
        sem_close :: Ptr () -> IO CInt
foreign import capi safe "semaphore.h sem_unlink"
        sem_unlink :: CString -> IO CInt
foreign import capi safe "semaphore.h sem_wait"
        sem_wait :: Ptr () -> IO CInt
foreign import capi interruptible "semaphore.h sem_wait"
        sem_wait_interruptible :: Ptr () -> IO CInt
foreign import capi safe "semaphore.h sem_trywait"
        sem_trywait :: Ptr () -> IO CInt
foreign import capi safe "semaphore.h sem_post"
        sem_post :: Ptr () -> IO CInt
