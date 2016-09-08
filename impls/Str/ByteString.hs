module Str.ByteString (
    Str,
    useAsOSString,
    newOSString,
    packOSString,
    module Data.ByteString.Char8
) where

import Foreign.C.String (CString)
import Data.Word (Word8)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import Data.ByteString.Char8
import Data.ByteString.Unsafe

type Str = ByteString

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

useAsOSString = useAsCString
newOSString s = unsafeUseAsCStringLen s $ \(c,l) -> do
                    p <- mallocBytes (l+1)
                    copyBytes p c (fromIntegral l)
                    pokeByteOff p l (0::Word8)
                    return p
packOSString  = packCString
