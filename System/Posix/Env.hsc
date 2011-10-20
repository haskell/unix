{-# LANGUAGE ForeignFunctionInterface #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
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
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Control.Monad	( liftM )
import Data.Maybe	( fromMaybe )
#if __GLASGOW_HASKELL__ > 700
import System.Posix.Internals (withFilePath, peekFilePath)
#elif __GLASGOW_HASKELL__ > 611
import System.Posix.Internals (withFilePath)

peekFilePath :: CString -> IO FilePath
peekFilePath = peekCString
#else
withFilePath :: FilePath -> (CString -> IO a) -> IO a
withFilePath = withCString

peekFilePath :: CString -> IO FilePath
peekFilePath = peekCString
#endif

-- |'getEnv' looks up a variable in the environment.

getEnv :: String -> IO (Maybe String)
getEnv name = do
  litstring <- withFilePath name c_getenv
  if litstring /= nullPtr
     then liftM Just $ peekFilePath litstring
     else return Nothing

-- |'getEnvDefault' is a wrapper around 'getEnv' where the
-- programmer can specify a fallback if the variable is not found
-- in the environment.

getEnvDefault :: String -> String -> IO String
getEnvDefault name fallback = liftM (fromMaybe fallback) (getEnv name)

foreign import ccall unsafe "getenv"
   c_getenv :: CString -> IO CString

getEnvironmentPrim :: IO [String]
getEnvironmentPrim = do
  c_environ <- getCEnviron
  arr <- peekArray0 nullPtr c_environ
  mapM peekFilePath arr

getCEnviron :: IO (Ptr CString)
#if darwin_HOST_OS
-- You should not access _environ directly on Darwin in a bundle/shared library.
-- See #2458 and http://developer.apple.com/library/mac/#documentation/Darwin/Reference/ManPages/man7/environ.7.html
getCEnviron = nsGetEnviron >>= peek

foreign import ccall unsafe "_NSGetEnviron"
   nsGetEnviron :: IO (Ptr (Ptr CString))
#else
getCEnviron = peek c_environ_p

foreign import ccall unsafe "&environ"
   c_environ_p :: Ptr (Ptr CString)
#endif

-- |'getEnvironment' retrieves the entire environment as a
-- list of @(key,value)@ pairs.

getEnvironment :: IO [(String,String)]
getEnvironment = do
  env <- getEnvironmentPrim
  return $ map (dropEq.(break ((==) '='))) env
 where
   dropEq (x,'=':ys) = (x,ys)
   dropEq (x,_)      = error $ "getEnvironment: insane variable " ++ x

-- |The 'unsetEnv' function deletes all instances of the variable name
-- from the environment.

unsetEnv :: String -> IO ()
#ifdef HAVE_UNSETENV

unsetEnv name = withFilePath name $ \ s ->
  throwErrnoIfMinus1_ "unsetenv" (c_unsetenv s)

foreign import ccall unsafe "__hsunix_unsetenv"
   c_unsetenv :: CString -> IO CInt
#else
unsetEnv name = putEnv (name ++ "=")
#endif

-- |'putEnv' function takes an argument of the form @name=value@
-- and is equivalent to @setEnv(key,value,True{-overwrite-})@.

putEnv :: String -> IO ()
putEnv keyvalue = withFilePath keyvalue $ \s ->
  throwErrnoIfMinus1_ "putenv" (c_putenv s)

foreign import ccall unsafe "putenv"
   c_putenv :: CString -> IO CInt

{- |The 'setEnv' function inserts or resets the environment variable name in
     the current environment list.  If the variable @name@ does not exist in the
     list, it is inserted with the given value.  If the variable does exist,
     the argument @overwrite@ is tested; if @overwrite@ is @False@, the variable is
     not reset, otherwise it is reset to the given value.
-}

setEnv :: String -> String -> Bool {-overwrite-} -> IO ()
#ifdef HAVE_SETENV
setEnv key value ovrwrt = do
  withFilePath key $ \ keyP ->
    withFilePath value $ \ valueP ->
      throwErrnoIfMinus1_ "setenv" $
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
