{-# OPTIONS -fffi #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Signals.Exts
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX, includes Linuxisms/BSDisms)
--
-- non-POSIX signal support commonly available
--
-----------------------------------------------------------------------------

#include "HsUnix.h"

module System.Posix.Signals.Exts (
  module System.Posix.Signals

#ifdef SIGINFO
  , infoEvent, sigINFO
#endif
#ifdef SIGWINCH
  , windowChange, sigWINCH
#endif

  ) where

import Foreign.C ( CInt )
import System.Posix.Signals

#ifdef __HUGS__
# ifdef SIGINFO
sigINFO   = (#const SIGINFO)   :: CInt
# endif
# ifdef SIGWINCH
sigWINCH  = (#const SIGWINCH)  :: CInt
# endif
#else /* !HUGS */
# ifdef SIGINFO
foreign import ccall unsafe "__hsunix_SIGINFO"   sigINFO   :: CInt
# endif
# ifdef SIGWINCH
foreign import ccall unsafe "__hsunix_SIGWINCH"   sigWINCH   :: CInt
# endif
#endif /* !HUGS */

#ifdef SIGINFO
infoEvent :: Signal
infoEvent = sigINFO
#endif

#ifdef SIGWINCH
windowChange :: Signal
windowChange = sigWINCH
#endif
