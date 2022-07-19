module Main where

import System.Posix
import Control.Monad
import Foreign hiding (void)
import Control.Concurrent
import Data.Char
import System.Exit

main :: IO ()
main = do
  let size  = 10000
      block = 512
  (rd,wr) <- createPipe
  let bytes = take size (map (fromIntegral.ord) (cycle ['a'..'z']))
  void $ forkIO $ allocaBytes size $ \p -> do
        pokeArray p bytes
        r <- fdWriteBuf wr p (fromIntegral size)
        when (fromIntegral r /= size) $ error "fdWriteBuf failed"
  allocaBytes block $ \p -> do
    let loop text = do
           r <- fdReadBuf rd p (fromIntegral block)
           let (chunk,rest) = splitAt (fromIntegral r) text
           chars <- peekArray (fromIntegral r) p
           when (chars /= chunk) $ error $ "mismatch: expected="++show chunk++", found="++show chars
           when (null rest) $ exitWith ExitSuccess
           loop rest
    loop bytes
