{-# OPTIONS -fglasgow-exts -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Sendfile
-- Copyright   :  (c) Volker Stolz 2003
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  fallback NYI
--
-- "System.Sendfile.sendfile" is a low-level method for efficently passing
-- data from one file descriptor to another.
-- The intended audience includes for example web server authors. Please
-- note that this function is highly platform dependent.
--
-----------------------------------------------------------------------------

module System.Sendfile (sendfile) where

#include "config.h"
#include "HsUnix.h"

import Foreign
import Foreign.C
import System.IO		(Handle)
import System.Posix.IO		(handleToFd)
import System.Posix.Types	(Fd, COff)

-- |'sendfile' transmits the contents of an open file to a stream socket
-- opened for writing with as little overhead as possible. This function
-- is not defined by any standard!
-- Notice that passing 0 as 'count' may result in undefined behaviour and
-- should be avoided. On Linux, nothing is transmitted while on FreeBSD
-- the entire file will be sent.

sendfile :: Handle	-- Input
         -> Handle	-- Output
         -> Integer	-- Offset
         -> Integer	-- Nr. of bytes to transmit
         -> IO ()
sendfile inH outH startpos count = do

#ifdef HAVE_LINUX_SENDFILE

  inFd  <- handleToFd inH
  outFd <- handleToFd outH
  offsetptr <- malloc
  poke offsetptr (fromIntegral startpos)
  throwErrnoIfMinus1_ "sendfile" $ c_sendfile outFd inFd offsetptr (fromIntegral count)
  free offsetptr
  return ()

foreign import ccall unsafe "sendfile"
  c_sendfile :: Fd -> Fd -> Ptr COff -> CSize -> IO CSize

#else /* Linux */
# ifdef HAVE_BSD_SENDFILE

  inFd  <- handleToFd inH
  outFd <- handleToFd outH
  offsetptr <- malloc
  rc <- sendfileLoop inFd outFd (fromIntegral startpos) (fromIntegral count) offsetptr
  free offsetptr
  return ()
 where
  sendfileLoop :: Fd -> Fd -> COff -> CSize -> Ptr COff -> IO CInt
  sendfileLoop inFd outFd start c offsetptr = do
    rc <- c_sendfile inFd outFd start c nullPtr offsetptr 0
    if rc == -1
     then do
       err <- getErrno
       case err of
         eAGAIN -> do
           offset <- peek offsetptr -- now contains # of bytes written
           sendfileLoop inFd outFd (start+offset)  (c-(fromIntegral offset)) offsetptr
         otherwise -> throwErrno "sendfile"
     else 
       return rc

foreign import ccall unsafe "sendfile"
  c_sendfile :: Fd -> Fd -> COff -> CSize -> Ptr a -> Ptr COff -> CInt -> IO CInt

# else /* BSD */
-- squirt data from 'rd' into 'wr' as fast as possible.  We use a 4k
-- single buffer. Stolen from Simon M.'s Haskell Web Server fptools/hws
  hSeek inH RelativeSeek startpos
  arr <- stToIO (newCharArray (0, bufsize-1))
  let loop remaining = do r <- hGetBufBA rd arr (min bufsize remaining)
		if (r == 0) 
		   then return ()
		   else if (r < bufsize) 
     			    then hPutBufBA wr arr r
     			    else hPutBufBA wr arr bufsize >> loop (remaining-bufsize)
  loop count

bufsize = 4 * 1024 :: Int
# endif /* no native */
#endif
