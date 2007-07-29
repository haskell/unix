
module System.Posix.Process.Internals (pPrPr_disableITimers, c_execvpe) where

import Foreign
import Foreign.C

-- this function disables the itimer, which would otherwise cause confusing
-- signals to be sent to the new process.
foreign import ccall unsafe "pPrPr_disableITimers"
  pPrPr_disableITimers :: IO ()

foreign import ccall unsafe "execvpe"
  c_execvpe :: CString -> Ptr CString -> Ptr CString -> IO CInt
