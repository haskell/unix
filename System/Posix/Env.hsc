{-# OPTIONS -fffi #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Env
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX environment support
--
-----------------------------------------------------------------------------

module System.Posix.Env (
	getEnv
	, getEnvDefault
	, getEnvironmentPrim
	, getEnvironment
	, putEnv
	, setEnv
	, unsetEnv
) where

#include "HsUnix.h"

import Foreign.C.Error	( throwErrnoIfMinus1_ )
import Foreign.C.Types	( CInt )
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Control.Monad	( liftM )
import Data.Maybe	( fromMaybe )

-- |'getEnv' looks up a variable in the environment.

getEnv :: String -> IO (Maybe String)
getEnv name = do
  litstring <- withCString name c_getenv
  if litstring /= nullPtr
     then liftM Just $ peekCString litstring
     else return Nothing

-- |'getEnvDefault' is a wrapper around 'getEnvVar' where the
-- programmer can specify a fallback if the variable is not found
-- in the environment.

getEnvDefault :: String -> String -> IO String
getEnvDefault name fallback = liftM (fromMaybe fallback) (getEnv name)

foreign import ccall unsafe "getenv"
   c_getenv :: CString -> IO CString

getEnvironmentPrim :: IO [String]
getEnvironmentPrim = do
  c_environ <- peek c_environ_p
  arr <- peekArray0 nullPtr c_environ
  mapM peekCString arr

foreign import ccall unsafe "&environ"
   c_environ_p :: Ptr (Ptr CString)

-- |'getEnvironment' retrieves the entire environment as a
-- list of @(key,value)@ pairs.

getEnvironment :: IO [(String,String)]
getEnvironment = do
  env <- getEnvironmentPrim
  return $ map (dropEq.(break ((==) '='))) env
 where
   dropEq (x,'=':ys) = (x,ys)
   dropEq (x,_)      = error $ "getEnvironment: insane variable " ++ x

-- |The 'unsetenv' function deletes all instances of the variable name
-- from the environment.

unsetEnv :: String -> IO ()
#ifdef HAVE_UNSETENV

unsetEnv name = withCString name c_unsetenv

foreign import ccall unsafe "unsetenv"
   c_unsetenv :: CString -> IO ()
#else
unsetEnv name = putEnv (name ++ "=")
#endif

-- |'putEnv' function takes an argument of the form @name=value@
-- and is equivalent to @setEnv(key,value,True{-overwrite-})@.

putEnv :: String -> IO ()
putEnv keyvalue = withCString keyvalue $ \s ->
  throwErrnoIfMinus1_ "putenv" (c_putenv s)

foreign import ccall unsafe "putenv"
   c_putenv :: CString -> IO CInt

{- |The 'setenv' function inserts or resets the environment variable name in
     the current environment list.  If the variable @name@ does not exist in the
     list, it is inserted with the given value.  If the variable does exist,
     the argument @overwrite@ is tested; if @overwrite@ is @False@, the variable is
     not reset, otherwise it is reset to the given value.
-}

setEnv :: String -> String -> Bool {-overwrite-} -> IO ()
#ifdef HAVE_SETENV
setEnv key value ovrwrt = do
  withCString key $ \ keyP ->
    withCString value $ \ valueP ->
      throwErrnoIfMinus1_ "putenv" $
	c_setenv keyP valueP (fromIntegral (fromEnum ovrwrt))

foreign import ccall unsafe "setenv"
   c_setenv :: CString -> CString -> CInt -> IO CInt
#else
setEnv key value True = putEnv (key++"="++value)
setEnv key value False = do
  res <- getEnv key
  case res of
    Just _  -> return ()
    Nothing -> putEnv (key++"="++value)
#endif
