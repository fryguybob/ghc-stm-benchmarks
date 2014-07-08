import Throughput
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

import System.IO
import System.Environment

main = do
    as <- getArgs
    n <- getNumCapabilities

    case as of
        [timeout] -> timeTest n (read timeout)
        _ -> putStrLn "Unknown arguments"

timeTest n timeout = do
    a <- newTVarIO 0

    let tasks = replicate n (forever $ atomically (modifyTVar' a (+1)))

    t <- throughputMain timeout tasks
    
    print t
    v <- readTVarIO a
    print v


