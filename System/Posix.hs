-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX support
--
-----------------------------------------------------------------------------

module System.Posix (
  module System.Posix.Types,
  module System.Posix.Signals,
  module System.Posix.Directory,
  module System.Posix.Files,
  module System.Posix.Unistd,
  module System.Posix.IO,
  module System.Posix.Process,
  -- module System.Posix.Time,
 ) where

import System.Posix.Types
import System.Posix.Signals
import System.Posix.Directory
import System.Posix.Files
import System.Posix.Unistd
import System.Posix.Process
import System.Posix.IO

