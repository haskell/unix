{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
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
-- POSIX environment support.  Warning: 'setEnvironment' and 'setEnv' are
-- NOT thread-safe.
--
-----------------------------------------------------------------------------

module System.Posix.Env (
      getEnv
    , getEnvDefault
    , getEnvironmentPrim
    , getEnvironment
    , setEnvironment
    , putEnv
    , setEnv
    , unsetEnv
    , clearEnv
) where

#include "HsUnix.h"

import Prelude hiding (break, head, tail)
import Str

import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types
import Foreign.C.String (CString)
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Control.Monad
import Data.Maybe (fromMaybe)

type OSString = CString

-- |'getEnv' looks up a variable in the environment.

getEnv :: Str -> IO (Maybe Str)
getEnv name = do
  litstring <- useAsOSString name c_getenv
  if litstring /= nullPtr
     then liftM Just $ packOSString litstring
     else return Nothing

-- |'getEnvDefault' is a wrapper around 'getEnv' where the
-- programmer can specify a fallback if the variable is not found
-- in the environment.

getEnvDefault :: Str -> Str -> IO Str
getEnvDefault name fallback = liftM (fromMaybe fallback) (getEnv name)

foreign import ccall unsafe "getenv"
   c_getenv :: OSString -> IO OSString

getEnvironmentPrim :: IO [Str]
getEnvironmentPrim = do
  c_environ <- getCEnviron
  -- environ can be NULL
  if c_environ == nullPtr
    then return []
    else do
      arr <- peekArray0 nullPtr c_environ
      mapM packOSString arr

haveNsGetEnviron :: Bool
nsGetEnviron :: IO (Ptr (Ptr OSString))
c_environ_p :: Ptr (Ptr OSString)

#if HAVE__NSGETENVIRON
haveNsGetEnviron = True
nsGetEnviron = nsGetEnviron'
c_environ_p = error "c_environ_p: HAVE__NSGETENVIRON"
foreign import ccall unsafe "_NSGetEnviron"
   nsGetEnviron' :: IO (Ptr (Ptr OSString))
#else
haveNsGetEnviron = False
nsGetEnviron = error "nsGetEnviron: !HAVE__NSGETENVIRON"
c_environ_p = c_environ_p'
foreign import ccall unsafe "&environ"
   c_environ_p' :: Ptr (Ptr OSString)
#endif

getCEnviron :: IO (Ptr OSString)
-- You should not access @char **environ@ directly on Darwin in a bundle/shared library.
-- See #2458 and http://developer.apple.com/library/mac/#documentation/Darwin/Reference/ManPages/man7/environ.7.html
getCEnviron | haveNsGetEnviron = nsGetEnviron >>= peek
            | otherwise        = peek c_environ_p

-- |'getEnvironment' retrieves the entire environment as a
-- list of @(key,value)@ pairs.

getEnvironment :: IO [(Str,Str)]
getEnvironment = do
  env <- getEnvironmentPrim
  return $ map (dropEq.(break ((==) '='))) env
 where
   dropEq (x,y)
      | head y == '=' = (x,tail y)
      | otherwise       = error $ "getEnvironment: insane variable " ++ unpack x

-- |'setEnvironment' resets the entire environment to the given list of
-- @(key,value)@ pairs.

setEnvironment :: [(Str,Str)] -> IO ()
setEnvironment env = do
  clearEnv
  forM_ env $ \(key,value) ->
    setEnv key value True {-overwrite-}

haveUnsetEnv :: Bool
unsetEnvReturnsVoid :: Bool
c_unsetenv :: OSString -> IO CInt
c_unsetenv_void :: OSString -> IO ()

#if HAVE_UNSETENV
haveUnsetEnv = True
# if !UNSETENV_RETURNS_VOID
unsetEnvReturnsVoid = False
c_unsetenv = c_unsetenv'
c_unsetenv_void = error "c_unsetenv_void: HAVE_UNSETENV && !UNSETENV_RETURNS_VOID"
-- POSIX.1-2001 compliant unsetenv(3)
foreign import capi unsafe "HsUnix.h unsetenv"
   c_unsetenv' :: OSString -> IO CInt
# else
unsetEnvReturnsVoid = True
c_unsetenv = error "c_unsetenv: HAVE_UNSETENV && UNSETENV_RETURNS_VOID"
c_unsetenv_void = c_unsetenv_void'
-- pre-POSIX unsetenv(3) returning @void@
foreign import capi unsafe "HsUnix.h unsetenv"
   c_unsetenv_void' :: OSString -> IO ()
# endif
#else
haveUnsetEnv = False
unsetEnvReturnsVoid = False
c_unsetenv = error "c_unsetenv: !HAVE_UNSETENV"
c_unsetenv_void = error "c_unsetenv_void: !HAVE_UNSETENV"
#endif

-- |The 'unsetEnv' function deletes all instances of the variable name
-- from the environment.

unsetEnv :: Str -> IO ()
unsetEnv name
    | haveUnsetEnv && not unsetEnvReturnsVoid
    = useAsOSString name $ \ s ->
        throwErrnoIfMinus1_ "unsetenv" (c_unsetenv s)
    | haveUnsetEnv && unsetEnvReturnsVoid
    = useAsOSString name c_unsetenv_void
    | otherwise
    = putEnv (append name (pack "="))

-- |'putEnv' function takes an argument of the form @name=value@
-- and is equivalent to @setEnv(key,value,True{-overwrite-})@.

putEnv :: Str -> IO ()
putEnv keyvalue = do s <- newOSString keyvalue
                     -- Do not free `s` after calling putenv.
                     -- According to SUSv2, the string passed to putenv
                     -- becomes part of the environment. #7342
                     throwErrnoIfMinus1_ "putenv" (c_putenv s)

foreign import ccall unsafe "putenv"
   c_putenv :: OSString -> IO CInt

{- |The 'setEnv' function inserts or resets the environment variable name in
     the current environment list.  If the variable @name@ does not exist in the
     list, it is inserted with the given value.  If the variable does exist,
     the argument @overwrite@ is tested; if @overwrite@ is @False@, the variable is
     not reset, otherwise it is reset to the given value.
-}

haveSetEnv :: Bool
c_setenv :: OSString -> OSString -> CInt -> IO CInt
#ifdef HAVE_SETENV
haveSetEnv = True
c_setenv = c_setenv'
foreign import ccall unsafe "setenv"
   c_setenv' :: OSString -> OSString -> CInt -> IO CInt
#else
haveSetEnv = False
c_setenv = error "c_setenv: !HAVE_SETENV"
#endif

setEnv :: Str -> Str -> Bool {-overwrite-} -> IO ()
setEnv key value ovrwrt
 | haveSetEnv = do
  useAsOSString key $ \ keyP ->
    useAsOSString value $ \ valueP ->
      throwErrnoIfMinus1_ "setenv" $
        c_setenv keyP valueP (fromIntegral (fromEnum ovrwrt))
 | otherwise =
  case ovrwrt of
    True -> putEnv (key `append` pack "=" `append` value)
    False -> do
      res <- getEnv key
      case res of
        Just _  -> return ()
        Nothing -> putEnv (key `append` pack "=" `append` value)

haveClearEnv :: Bool
c_clearenv :: IO Int
#if HAVE_CLEARENV
haveClearEnv = True
c_clearenv = c_clearenv'
foreign import ccall unsafe "clearenv"
  c_clearenv' :: IO Int
#else
haveClearEnv = False
c_clearenv = error "c_clearenv: !HAVE_CLEARENV"
#endif

-- |The 'clearEnv' function clears the environment of all name-value pairs.
clearEnv :: IO ()
clearEnv | haveClearEnv = void c_clearenv
-- Fallback to 'environ[0] = NULL'.
         | otherwise = do
  c_environ <- getCEnviron
  unless (c_environ == nullPtr) $
    poke c_environ nullPtr
