-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.DynamicLinker
-- Copyright   :  (c) Volker Stolz <vs@foldr.org> 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  vs@foldr.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- Dynamic linker support through dlopen()
-----------------------------------------------------------------------------

module System.Posix.DynamicLinker (

    module System.Posix.DynamicLinker.Prim,
    dlopen,
    dlsym,
    dlerror,
    dlclose,
    withDL, withDL_,
    undl,
    )

--  Usage:
--  ******
--  
--  Let's assume you want to open a local shared library \'foo\' (.\/libfoo.so)
--  offering a function
--    @char \* mogrify (char\*,int)@
--  and invoke @str = mogrify("test",1)@:
-- 
--  
--  type Fun = CString -> Int -> IO CString
--  foreign import dynamic unsafe fun__ :: FunPtr Fun -> Fun
-- 
--  withDL "libfoo.so" [RTLD_NOW] \$ \\ mod -> do
--     funptr <- dlsym mod "mogrify"
--     let fun = fun__ funptr
--     withCString "test" \$ \\ str -> do
--       strptr <- fun str 1
--       strstr <- peekCString strptr
--       ...
--  

where

#include "HsUnix.h"

import System.Posix.DynamicLinker.Prim
import Control.Exception	( bracket )
import Control.Monad	( liftM )
import Foreign.Ptr	( Ptr, nullPtr, FunPtr, nullFunPtr )
import Foreign.C.String	( withCString, peekCString )

dlopen :: String -> [RTLDFlags] -> IO DL
dlopen path flags = do
  withCString path $ \ p -> do
    liftM DLHandle $ throwDLErrorIf "dlopen" (== nullPtr) $ c_dlopen p (packRTLDFlags flags)

dlclose :: DL -> IO ()
dlclose (DLHandle h) = throwDLErrorIf_ "dlclose" (== 0) $ c_dlclose h
dlclose h = error $ "dlclose: invalid argument" ++ (show h)

dlerror :: IO String
dlerror = c_dlerror >>= peekCString 

-- |'dlsym' returns the address binding of the symbol described in @symbol@,
-- as it occurs in the shared object identified by @source@.

dlsym :: DL -> String -> IO (FunPtr a)
dlsym source symbol = do
  withCString symbol $ \ s -> do
    throwDLErrorIf "dlsym" (== nullFunPtr) $ c_dlsym (packDL source) s

withDL :: String -> [RTLDFlags] -> (DL -> IO a) -> IO a
withDL mod flags f = bracket (dlopen mod flags) (dlclose) f

withDL_ :: String -> [RTLDFlags] -> (DL -> IO a) -> IO ()
withDL_ mod flags f = withDL mod flags f >> return ()

-- |'undl' obtains the raw handle. You mustn't do something like
-- @withDL mod flags $ liftM undl >>= \ p -> use p@

undl :: DL -> Ptr ()
undl = packDL

throwDLErrorIf :: String -> (a -> Bool) -> IO a -> IO a
throwDLErrorIf s p f = do
  r <- f
  if (p r)
    then dlerror >>= \ err -> ioError (userError ( s ++ ": " ++ err))
    else return r

throwDLErrorIf_ s p f = throwDLErrorIf s p f >> return ()
