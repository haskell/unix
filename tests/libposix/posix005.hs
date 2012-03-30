
import System.IO
import System.Posix.Env

main = do
    hSetBuffering stdout NoBuffering
    term <- getEnv "TERM"
    maybe (return ()) putStrLn term
    setEnvironment [("one","1"),("two","2")]
    getEnvironment >>= print
    setEnv "foo" "bar" True
    getEnvironment >>= print
    setEnv "foo" "baz" True
    getEnvironment >>= print
    setEnv "fu" "bar" True
    getEnvironment >>= print
    unsetEnv "foo"
    getEnvironment >>= print
    clearEnv
    getEnvironment >>= print

