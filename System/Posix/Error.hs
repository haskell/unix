-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Error
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX error support
--
-----------------------------------------------------------------------------

module System.Posix.Error (
	throwErrnoPath,
	throwErrnoPathIf, 
	throwErrnoPathIf_,
	throwErrnoPathIfNull,
	throwErrnoPathIfMinus1,
	throwErrnoPathIfMinus1_
  ) where

import Foreign.C.Error
import Foreign.Ptr
import Foreign.Marshal.Error 	( void )

throwErrnoPath :: String -> FilePath -> IO a
throwErrnoPath loc path =
  do
    errno <- getErrno
    ioError (errnoToIOError loc errno Nothing (Just path))

throwErrnoPathIf :: (a -> Bool) -> String -> FilePath -> IO a -> IO a
throwErrnoPathIf pred loc path f  = 
  do
    res <- f
    if pred res then throwErrnoPath loc path else return res

throwErrnoPathIf_ :: (a -> Bool) -> String -> FilePath -> IO a -> IO ()
throwErrnoPathIf_ pred loc path f  = void $ throwErrnoPathIf pred loc path f

throwErrnoPathIfNull :: String -> FilePath -> IO (Ptr a) -> IO (Ptr a)
throwErrnoPathIfNull  = throwErrnoPathIf (== nullPtr)

throwErrnoPathIfMinus1 :: Num a => String -> FilePath -> IO a -> IO a
throwErrnoPathIfMinus1 = throwErrnoPathIf (== -1)

throwErrnoPathIfMinus1_ :: Num a => String -> FilePath -> IO a -> IO ()
throwErrnoPathIfMinus1_  = throwErrnoPathIf_ (== -1)
