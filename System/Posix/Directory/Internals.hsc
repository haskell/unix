-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Directory.Internals
-- Copyright   :  (c) The University of Glasgow 2022
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX directory support (internal module, no PVP guarantees)
--
-----------------------------------------------------------------------------

module System.Posix.Directory.Internals ( DirStream(..), CDir, CDirent, DirStreamOffset(..) ) where

import System.Posix.Directory.Common
