import Control.Concurrent
import System.Posix
import Control.Monad

-- signal stress test: threads installing signal handlers while
-- signals are being constantly thrown and caught.

installers = 50
sigs = 10000

main = do
  c <- newChan
  m <- newEmptyMVar
  installHandler sigUSR1 (handler c) Nothing
  replicateM_ installers (forkIO $ do replicateM_ 1000 (install c); putMVar m ())
  replicateM_ sigs (forkIO $ raiseSignal sigUSR1)
  replicateM_ installers (takeMVar m)
  replicateM_ sigs (readChan c)

handler c = Catch (writeChan c ())

install c = do
  old <- installHandler sigUSR1 (handler c) Nothing
  installHandler sigUSR1 old Nothing
