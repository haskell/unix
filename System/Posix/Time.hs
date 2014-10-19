{-# LANGUAGE CPP #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Time
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX Time support
--
-----------------------------------------------------------------------------

module System.Posix.Time (

      epochTime

#ifdef HAVE_SETITIMER
    , ITimerType(..)

    , TimeVal(TimeVal)
    , timeValSeconds
    , timeValMicroseconds

    , ITimerVal(ITimerVal)
    , itimerValInterval
    , itimerValValue

    , setitimer
    , getitimer
#endif

  ) where

import System.Posix.Types
import Foreign
import Foreign.C

-- -----------------------------------------------------------------------------
-- epochTime

-- | @epochTime@ calls @time@ to obtain the number of
--   seconds that have elapsed since the epoch (Jan 01 00:00:00 GMT 1970).
epochTime :: IO EpochTime
epochTime = throwErrnoIfMinus1 "epochTime" (c_time nullPtr)

foreign import capi unsafe "HsUnix.h time"
  c_time :: Ptr CTime -> IO CTime

#ifdef HAVE_SETITIMER

-- | Haskell representation of @struct timeval@: seconds and microseconds.
data TimeVal = TimeVal {
    tv_sec :: Int
  , tv_usec :: Int
  }

timeValSeconds :: TimeVal -> Int
timeValSeconds = tv_sec

timeValMicroseconds :: TimeVal -> Int
timeValMicroseconds = tv_usec

instance Storable TimeVal where
  sizeOf _ = (#size struct timeval)
  alignment _ = alignment (undefined :: CLong)
  peek ptr = do
    sec <- (#peek struct timeval, tv_sec) ptr
    usec <- (#peek struct timeval, tv_usec) ptr
    return $ TimeVal sec usec
  poke ptr timeval = do
    (#poke struct timeval, tv_sec) ptr (tv_sec timeval)
    (#poke struct timeval, tv_usec) ptr (tv_usec timeval)

-- | Haskell representation of the timer types:
--   @ITIMER_REAL@, @ITIMER_VIRTUAL@, and @ITIMER_PROF@.
data ITimerType = ITimerReal | ITimerVirtual | ITimerProf

-- | Not exported.
iTimerTypeToCInt :: ITimerType -> CInt
iTimerTypeToCInt ITimerReal = #const ITIMER_REAL
iTimerTypeToCInt ITimerVirtual = #const ITIMER_VIRTUAL
iTimerTypeToCInt ITimerProf = #const ITIMER_PROF

-- | Haskell representation of @struct itimerval@.
data ITimerVal = ITimerVal {
    it_interval :: TimeVal
  , it_value :: TimeVal
  }

itimerValInterval :: ITimerVal -> TimeVal
itimerValInterval = it_interval

itimerValValue :: ITimerVal -> TimeVal
itimerValValue = it_value

instance Storable ITimerVal where
  sizeOf _ = (#size struct itimerval)
  alignment _ = alignment (undefined :: TimeVal)
  peek ptr = do
    interval <- (#peek struct itimerval, it_interval) ptr
    value <- (#peek struct itimerval, it_value) ptr
    return $ ITimerVal interval value
  poke ptr itimerval = do
    (#poke struct itimerval, it_interval) ptr (it_interval itimerval)
    (#poke struct itimerval, it_value) ptr (it_value itimerval)

-- | Works like @getitimer@ from sys/time.h.  Throws an exception in case of
-- error.
getitimer :: ITimerType -> IO ITimerVal
getitimer itimerType = alloca $ \oldptr -> do
    throwErrnoIfMinus1 "setitimer" $
        c_getitimer (iTimerTypeToCInt itimerType) oldptr
    peek oldptr

-- | Works like @setitimer@ from @sys/time.h@.  The old @itimerval@ is returned
-- on success.  An exception is thrown on error.
setitimer :: ITimerType -> ITimerVal -> IO ITimerVal
setitimer itimerType itimerVal = alloca $ \newptr -> do
    poke newptr itimerVal
    alloca $ \oldptr -> do
        throwErrnoIfMinus1 "setitimer" $
            c_setitimer (iTimerTypeToCInt itimerType) newptr oldptr
        peek oldptr

foreign import ccall unsafe "setitimer"
  c_setitimer :: CInt -> Ptr ITimerVal -> Ptr ITimerVal -> IO CInt

foreign import ccall unsafe "getitimer"
  c_getitimer :: CInt -> Ptr ITimerVal -> IO CInt
#endif
