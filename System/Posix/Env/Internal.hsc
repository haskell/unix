module System.Posix.Env.Internal where

#include "HsUnix.h"

import Foreign
import Foreign.C

getEnvironmentPrim :: IO [Ptr CChar]
getEnvironmentPrim = do
  c_environ <- getCEnviron
  if c_environ == nullPtr
    then return []
    else do
      peekArray0 nullPtr c_environ

getCEnviron :: IO (Ptr CString)
#if HAVE__NSGETENVIRON
-- You should not access @char **environ@ directly on Darwin in a bundle/shared library.
-- See #2458 and http://developer.apple.com/library/mac/#documentation/Darwin/Reference/ManPages/man7/environ.7.html
getCEnviron = nsGetEnviron >>= peek

foreign import ccall unsafe "_NSGetEnviron"
   nsGetEnviron :: IO (Ptr (Ptr CString))
#else
getCEnviron = _getCEnviron

-- N.B. we cannot import `environ` directly in Haskell as it may be a weak symbol
-- which requires special treatment by the compiler, which GHC is not equipped to
-- provide. See GHC #24011.
foreign import ccall unsafe "__hsunix_get_environ"
   _getCEnviron :: IO (Ptr CString)
#endif
