{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE PackageImports #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Env.PosixString
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

module System.Posix.Env.PosixString (
       -- * Environment Variables
        getEnv
        , getEnvDefault
        , getEnvironmentPrim
        , getEnvironment
        , setEnvironment
        , putEnv
        , setEnv
        , unsetEnv
        , clearEnv

       -- * Program arguments
       , getArgs
) where

#include "HsUnix.h"

import Control.Monad
import Foreign
import Foreign.C
import Data.Maybe       ( fromMaybe )

import GHC.IO.Encoding.UTF8 ( mkUTF8 )
import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )
import System.Posix.Env ( clearEnv )
import System.OsPath.Posix
import System.OsString.Internal.Types
#if MIN_VERSION_filepath(1, 5, 0)
import qualified "os-string" System.OsString.Data.ByteString.Short as B
#else
import qualified "filepath" System.OsPath.Data.ByteString.Short as B
#endif
import Data.ByteString.Short.Internal ( copyToPtr )

import qualified System.Posix.Env.Internal as Internal

-- |'getEnv' looks up a variable in the environment.

getEnv ::
  PosixString            {- ^ variable name  -} ->
  IO (Maybe PosixString) {- ^ variable value -}
getEnv (PS name) = do
  litstring <- B.useAsCString name c_getenv
  if litstring /= nullPtr
     then (Just . PS) <$> B.packCString litstring
     else return Nothing

-- |'getEnvDefault' is a wrapper around 'getEnv' where the
-- programmer can specify a fallback as the second argument, which will be
-- used if the variable is not found in the environment.

getEnvDefault ::
  PosixString    {- ^ variable name                    -} ->
  PosixString    {- ^ fallback value                   -} ->
  IO PosixString {- ^ variable value or fallback value -}
getEnvDefault name fallback = fromMaybe fallback <$> getEnv name

foreign import ccall unsafe "getenv"
   c_getenv :: CString -> IO CString

getEnvironmentPrim :: IO [PosixString]
getEnvironmentPrim = Internal.getEnvironmentPrim >>= mapM (fmap PS . B.packCString)

-- |'getEnvironment' retrieves the entire environment as a
-- list of @(key,value)@ pairs.

getEnvironment :: IO [(PosixString,PosixString)] {- ^ @[(key,value)]@ -}
getEnvironment = do
  env <- getEnvironmentPrim
  return $ map (dropEq . (B.break ((==) _equal)) . getPosixString) env
 where
   dropEq (x,y)
      | B.head y == _equal = (PS x, PS (B.tail y))
      | otherwise          = error $ "getEnvironment: insane variable " ++ _toStr x

-- |'setEnvironment' resets the entire environment to the given list of
-- @(key,value)@ pairs.
setEnvironment ::
  [(PosixString,PosixString)] {- ^ @[(key,value)]@ -} ->
  IO ()
setEnvironment env = do
  clearEnv
  forM_ env $ \(key,value) ->
    setEnv key value True {-overwrite-}

-- |The 'unsetEnv' function deletes all instances of the variable name
-- from the environment.

unsetEnv :: PosixString {- ^ variable name -} -> IO ()
#if HAVE_UNSETENV
# if !UNSETENV_RETURNS_VOID
unsetEnv (PS name) = B.useAsCString name $ \ s ->
  throwErrnoIfMinus1_ "unsetenv" (c_unsetenv s)

-- POSIX.1-2001 compliant unsetenv(3)
foreign import capi unsafe "HsUnix.h unsetenv"
   c_unsetenv :: CString -> IO CInt
# else
unsetEnv name = B.useAsCString name c_unsetenv

-- pre-POSIX unsetenv(3) returning @void@
foreign import capi unsafe "HsUnix.h unsetenv"
   c_unsetenv :: CString -> IO ()
# endif
#else
unsetEnv name = putEnv (name <> PosixString (B.pack "="))
#endif

-- |'putEnv' function takes an argument of the form @name=value@
-- and is equivalent to @setEnv(key,value,True{-overwrite-})@.
putEnv :: PosixString {- ^ "key=value" -} -> IO ()
putEnv (PS sbs) = do
  buf <- mallocBytes (l+1)
  copyToPtr sbs 0 buf (fromIntegral l)
  pokeByteOff buf l (0::Word8)
  throwErrnoIfMinus1_ "putenv" (c_putenv buf)
 where l = B.length sbs


foreign import ccall unsafe "putenv"
   c_putenv :: CString -> IO CInt

{- |The 'setEnv' function inserts or resets the environment variable name in
     the current environment list.  If the variable @name@ does not exist in the
     list, it is inserted with the given value.  If the variable does exist,
     the argument @overwrite@ is tested; if @overwrite@ is @False@, the variable is
     not reset, otherwise it is reset to the given value.
-}

setEnv ::
  PosixString {- ^ variable name  -} ->
  PosixString {- ^ variable value -} ->
  Bool       {- ^ overwrite      -} ->
  IO ()
#ifdef HAVE_SETENV
setEnv (PS key) (PS value) ovrwrt = do
  B.useAsCString key $ \ keyP ->
    B.useAsCString value $ \ valueP ->
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

-- | Computation 'getArgs' returns a list of the program's command
-- line arguments (not including the program name), as 'PosixString's.
--
-- Unlike 'System.Environment.getArgs', this function does no Unicode
-- decoding of the arguments; you get the exact bytes that were passed
-- to the program by the OS.  To interpret the arguments as text, some
-- Unicode decoding should be applied.
--
getArgs :: IO [PosixString]
getArgs =
  alloca $ \ p_argc ->
  alloca $ \ p_argv -> do
   getProgArgv p_argc p_argv
   p    <- fromIntegral <$> peek p_argc
   argv <- peek p_argv
   peekArray (p - 1) (advancePtr argv 1) >>= mapM (fmap PS . B.packCString)

foreign import ccall unsafe "getProgArgv"
  getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

_equal :: Word8
_equal = 0x3d

_toStr :: B.ShortByteString -> String
_toStr = either (error . show) id . decodeWith (mkUTF8 TransliterateCodingFailure) . PosixString
