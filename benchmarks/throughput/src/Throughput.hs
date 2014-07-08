{-# LANGUAGE ForeignFunctionInterface #-}
module Throughput 
    ( throughputMain
    ) where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Applicative
import Control.Exception (finally)

import System.IO

throughputMain timeout ws = do
    -- Record start time.
    start <- getTime

    -- Start threads
    (ts,vs) <- fmap unzip . forM ws $ \w -> do
        v <- newEmptyMVar
        t <- forkIO (finally w (putMVar v ()))
        return (t,v)

    
    -- Wait for time
    threadDelay timeout

    -- Kill threads
    mapM_ killThread ts
    mapM_ takeMVar vs   -- Wait for the finish signal

    -- result in total time
    end <- getTime

    return $! end - start

foreign import ccall unsafe "throughput_gettime" getTime :: IO Double
