{-# LANGUAGE CPP #-}
module Str.String (
    Str,
    useAsOSString,
    newOSString,
    packOSString,
    break,
    tail,
    head,
    unpack,
    append,
    pack
) where

import Prelude hiding (break, head, tail)
import qualified Prelude as P
import Foreign.C.String
import System.Posix.Internals
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (getFileSystemEncoding)

type Str = String

-- NB: I copypasted this from Str.hsig

-- | Marshal a 'Str' into a NUL terminated C string using temporary
-- storage.
--
-- * if 'Str' is Unicode, it is encoded to bytes using PEP 383
--   ('GHC.IO.Encoding.getFileSystemEncoding'), which will interpret
--   special use of surrogates as otherwise unrepresentable bytes.
--
-- * the 'Str' may /not/ contain any NUL characters. However, it may
--   optionally be NUL terminated.
--
-- * the memory is freed when the subcomputation terminates (either
--   normally or via an exception), so the pointer to the temporary
--   storage must /not/ be used after this.
--
useAsOSString :: Str -> (CString -> IO a) -> IO a

-- | Marshal a 'Str' into a NUL terminated C string. However, it may
--   optionally be NUL terminated.
--
-- * if 'Str' is Unicode, it is encoded to bytes using PEP 383
--   ('GHC.IO.Encoding.getFileSystemEncoding'), which will interpret
--   special use of surrogates as otherwise unrepresentable bytes.
--
-- * the 'Str' may /not/ contain any NUL characters
--
-- * new storage is allocated for the C string and must be
--   explicitly freed using 'Foreign.Marshal.Alloc.free' or
--   'Foreign.Marshal.Alloc.finalizerFree'.
--
newOSString :: Str -> IO CString

-- | Marshal a NUL terminated C string into a 'Str'.
--
-- * if 'Str' is Unicode, we will decode using PEP 383
--   ('GHC.IO.Encoding.getFileSystemEncoding'), which decode
--   otherwise uninterpretable bytes as surrogate sequences,
--   which can be round-tripped back.
--
packOSString :: CString -> IO Str

break :: (Char -> Bool) -> Str -> (Str, Str)
tail :: Str -> Str
head :: Str -> Char
unpack :: Str -> String -- or call this toString?
append :: Str -> Str -> Str
pack :: String -> Str

useAsOSString = withFilePath
newOSString   = newFilePath
packOSString  = peekFilePath

break  = P.break
tail   = P.tail
head   = P.head
unpack = id
append = (++)
pack   = id

-- Some internal backwards-compatibility goo.

newFilePath' :: FilePath -> IO CString
newFilePath' fp = getFileSystemEncoding >>= \enc -> GHC.newCString enc fp

#if !MIN_VERSION_base(4,7,0)
newFilePath :: FilePath -> IO CString
newFilePath = newFilePath'
#endif
