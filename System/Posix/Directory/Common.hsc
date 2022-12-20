{-# LANGUAGE CPP, Safe, CApiFFI, PatternSynonyms #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Directory.Common
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX directory support
--
-----------------------------------------------------------------------------

#include "HsUnix.h"
#include "HsUnixConfig.h"
##include "HsUnixConfig.h"

module System.Posix.Directory.Common (
       DirStream(..), DirEnt(..), CDir, CDirent, DirStreamOffset(..),
       DirType( DirType
              , DtUnknown
#ifdef CONST_DT_FIFO
              , DtFifo
#endif
#ifdef CONST_DT_CHR
              , DtChr
#endif
#ifdef CONST_DT_DIR
              , DtDir
#endif
#ifdef CONST_DT_BLK
              , DtBlk
#endif
#ifdef CONST_DT_REG
              , DtReg
#endif
#ifdef CONST_DT_LNK
              , DtLnk
#endif
#ifdef CONST_DT_SOCK
              , DtSock
#endif
#ifdef CONST_DT_WHT
              , DtWht
#endif
              ),
       unsafeOpenDirStreamFd,
       readDirStreamWith,
       readDirStreamWithPtr,
       rewindDirStream,
       closeDirStream,
#ifdef HAVE_SEEKDIR
       seekDirStream,
#endif
#ifdef HAVE_TELLDIR
       tellDirStream,
#endif
       changeWorkingDirectoryFd,
  ) where

import Control.Exception (mask_)
import Control.Monad (void, when)
import System.Posix.Types
import Foreign hiding (void)
import Foreign.C

#if !defined(HAVE_FCHDIR)
import System.IO.Error ( ioeSetLocation )
import GHC.IO.Exception ( unsupportedOperation )
#endif

newtype DirStream = DirStream (Ptr CDir)

newtype DirEnt = DirEnt (Ptr CDirent)

-- We provide a hand-written instance here since GeneralizedNewtypeDeriving and
-- DerivingVia are not allowed in Safe Haskell.
instance Storable DirEnt where
  sizeOf _ = sizeOf (undefined :: Ptr CDirent)
  {-# INLINE sizeOf #-}

  alignment _ = alignment (undefined :: Ptr CDirent)
  {-# INLINE alignment #-}

  peek ptr = DirEnt <$> peek (castPtr ptr)
  {-# INLINE peek #-}

  poke ptr (DirEnt dEnt) = poke (castPtr ptr) dEnt
  {-# INLINE poke#-}

data {-# CTYPE "DIR" #-} CDir
data {-# CTYPE "struct dirent" #-} CDirent

newtype DirType = DirType CChar

pattern DtUnknown :: DirType
#ifdef CONST_DT_UNKNOWN
pattern DtUnknown = DirType CONST_DT_UNKNOWN
#else
pattern DtUnknown = DirType 0
#endif

#ifdef CONST_DT_FIFO
pattern DtFifo :: DirType
pattern DtFifo = DirType CONST_DT_FIFO
#endif

#ifdef CONST_DT_CHR
pattern DtChr :: DirType
pattern DtChr = DirType CONST_DT_CHR
#endif

#ifdef CONST_DT_DIR
pattern DtDir :: DirType
pattern DtDir = DirType CONST_DT_DIR
#endif

#ifdef CONST_DT_BLK
pattern DtBlk :: DirType
pattern DtBlk = DirType CONST_DT_BLK
#endif

#ifdef CONST_DT_REG
pattern DtReg :: DirType
pattern DtReg = DirType CONST_DT_REG
#endif

#ifdef CONST_DT_LNK
pattern DtLnk :: DirType
pattern DtLnk = DirType CONST_DT_LNK
#endif

#ifdef CONST_DT_SOCK
pattern DtSock :: DirType
pattern DtSock = DirType CONST_DT_SOCK
#endif

#ifdef CONST_DT_WHT
pattern DtWht :: DirType
pattern DtWht = DirType CONST_DT_WHT
#endif

-- | Call @fdopendir@ to obtain a directory stream for @fd@. @fd@ must not be
-- otherwise used after this.
--
-- On success, it is owned by the returned 'DirStream', which should be closed
-- via 'closeDirStream' when no longer needed.  On error, the file descriptor
-- is automatically closed and then an exception is thrown.  There is no code
-- path in which the file descriptor remains open and yet not owned by a
-- returned 'DirStream'.
--
-- The input file descriptor must not have been used with @threadWaitRead@ or
-- @threadWaitWrite@.
unsafeOpenDirStreamFd :: Fd -> IO DirStream
unsafeOpenDirStreamFd (Fd fd) = mask_ $ do
    ptr <- c_fdopendir fd
    when (ptr == nullPtr) $ do
        errno <- getErrno
        void $ c_close fd
        ioError (errnoToIOError "openDirStreamFd" errno Nothing Nothing)
    return $ DirStream ptr

-- We need c_close here, because 'closeFd' throws exceptions on error,
-- but we want to silently close the (presumably directory) descriptor.
foreign import ccall unsafe "HsUnix.h close"
   c_close :: CInt -> IO CInt

-- NOTE: It is /critical/ to use "capi" and "dirent.h" here, because system
-- headers on e.g. macOS alias this function, and linking directly to the
-- "fdopendir" symbol in libc leads to a crash!
--
foreign import capi unsafe "dirent.h fdopendir"
    c_fdopendir :: CInt -> IO (Ptr CDir)

-- | @readDirStreamWith f dp@ calls @readdir@ to obtain the next directory entry
--   (@struct dirent@) for the open directory stream @dp@. If an entry is read,
--   it passes the pointer to that structure to the provided function @f@ for
--   processing. It returns the result of that function call wrapped in a @Just@
--   if an entry was read and @Nothing@ if the end of the directory stream was
--   reached.
--
--   __NOTE:__ The lifetime of the pointer wrapped in the `DirEnt` is limited to
--   invocation of the callback and it will be freed automatically after. Do not
--   pass it to the outside world!
readDirStreamWith :: (DirEnt -> IO a) -> DirStream -> IO (Maybe a)
readDirStreamWith f dstream = alloca
  (\ptr_dEnt  -> readDirStreamWithPtr ptr_dEnt f dstream)

-- | A version of 'readDirStreamWith' that takes a pre-allocated pointer in
--   addition to the other arguments. This pointer is used to store the pointer
--   to the next directory entry, if there is any. This function is intended for
--   usecases where you need to read a lot of directory entries and want to
--   reuse the pointer for each of them. Using for example 'readDirStream' or
--   'readDirStreamWith' in this scenario would allocate a new pointer for each
--   call of these functions.
--
--   __NOTE__: You are responsible for releasing the pointer after you are done.
readDirStreamWithPtr :: Ptr DirEnt -> (DirEnt -> IO a) -> DirStream -> IO (Maybe a)
readDirStreamWithPtr ptr_dEnt f dstream@(DirStream dirp) = do
  resetErrno
  r <- c_readdir dirp (castPtr ptr_dEnt)
  if (r == 0)
       then do dEnt@(DirEnt dEntPtr) <- peek ptr_dEnt
               if (dEntPtr == nullPtr)
                  then return Nothing
                  else do
                   res <- f dEnt
                   c_freeDirEnt dEntPtr
                   return (Just res)
       else do errno <- getErrno
               if (errno == eINTR)
                  then readDirStreamWithPtr ptr_dEnt f dstream
                  else do
                   let (Errno eo) = errno
                   if (eo == 0)
                      then return Nothing
                      else throwErrno "readDirStream"

-- traversing directories
foreign import ccall unsafe "__hscore_readdir"
  c_readdir  :: Ptr CDir -> Ptr (Ptr CDirent) -> IO CInt

foreign import ccall unsafe "__hscore_free_dirent"
  c_freeDirEnt  :: Ptr CDirent -> IO ()

foreign import ccall unsafe "__hscore_d_name"
  d_name :: Ptr CDirent -> IO CString

foreign import ccall unsafe "__hscore_d_type"
  d_type :: Ptr CDirent -> IO CChar

-- | @rewindDirStream dp@ calls @rewinddir@ to reposition
--   the directory stream @dp@ at the beginning of the directory.
rewindDirStream :: DirStream -> IO ()
rewindDirStream (DirStream dirp) = c_rewinddir dirp

foreign import ccall unsafe "rewinddir"
   c_rewinddir :: Ptr CDir -> IO ()

-- | @closeDirStream dp@ calls @closedir@ to close
--   the directory stream @dp@.
closeDirStream :: DirStream -> IO ()
closeDirStream (DirStream dirp) = do
  throwErrnoIfMinus1Retry_ "closeDirStream" (c_closedir dirp)

foreign import ccall unsafe "closedir"
   c_closedir :: Ptr CDir -> IO CInt

newtype DirStreamOffset = DirStreamOffset COff

#ifdef HAVE_SEEKDIR
seekDirStream :: DirStream -> DirStreamOffset -> IO ()
seekDirStream (DirStream dirp) (DirStreamOffset off) =
  c_seekdir dirp (fromIntegral off) -- TODO: check for CLong/COff overflow

foreign import ccall unsafe "seekdir"
  c_seekdir :: Ptr CDir -> CLong -> IO ()
#endif

#ifdef HAVE_TELLDIR
tellDirStream :: DirStream -> IO DirStreamOffset
tellDirStream (DirStream dirp) = do
  off <- c_telldir dirp
  return (DirStreamOffset (fromIntegral off)) -- TODO: check for overflow

foreign import ccall unsafe "telldir"
  c_telldir :: Ptr CDir -> IO CLong
#endif

#if defined(HAVE_FCHDIR)

changeWorkingDirectoryFd :: Fd -> IO ()
changeWorkingDirectoryFd (Fd fd) =
  throwErrnoIfMinus1Retry_ "changeWorkingDirectoryFd" (c_fchdir fd)

foreign import ccall unsafe "fchdir"
  c_fchdir :: CInt -> IO CInt

#else

{-# WARNING changeWorkingDirectoryFd "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_FCHDIR@)" #-}
changeWorkingDirectoryFd :: Fd -> IO ()
changeWorkingDirectoryFd _ = ioError (ioeSetLocation unsupportedOperation "changeWorkingDirectoryFd")

#endif // HAVE_FCHDIR
