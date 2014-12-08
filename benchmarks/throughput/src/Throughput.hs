{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ViewPatterns #-}
module Throughput 
    ( throughputMain
    , throughputMain'
    , locallyCountingForever
    , locallyCountingIterate
    , voidForever
    , ThroughputAction
    ) where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Applicative
import Control.Exception (finally)

import Data.IORef

import System.IO

-- Here we provide a framework for running some worker threads for a given
-- amount of time.  We may also want to collect data as these threads work in a
-- thread local way.  To achieve this we will run the worker action forever and
-- kill all the worker threads after the desired time has elapsed.  The
-- framework will wait for all the threads to complete after killing and then
-- record and result in the actual elapsed time.  Each worker action given must
-- spawn its own looping thread.  Wrappers for this looping and spawning are
-- given as 'locallyCountingForever' and 'voidForever'.  The first one counts
-- every time the action runs in a thread local counter and gives access to
-- that counter with an 'IO' action.  The second doesn't do any reporting, but
-- simply loops forever in a thread.

type ThroughputAction a = IO (ThreadId, a)

locallyCountingIterate :: (a -> IO a) -> a -> ThroughputAction (IO Int)
locallyCountingIterate step initial = do
    c <- newIORef 0
    t <- forkIO $ act c initial
    return (t, readIORef c)
  where
    act c x = do
        x' <- step x
        modifyIORef' c (+1)
        act c x'


locallyCountingForever :: IO () -> ThroughputAction (IO Int)
locallyCountingForever act = do
    c <- newIORef 0
    t <- forkIO . forever $ (modifyIORef' c (+1) >> act)
    return (t, readIORef c)

voidForever :: IO () -> ThroughputAction ()
voidForever act = do
    t <- forkIO . forever $ act
    return (t, ())

throughputMain :: Int -> [IO ()] -> IO Double
throughputMain timeout ws = fst <$> throughputMain' timeout (map voidForever ws)

throughputMain' :: Int -> [ThroughputAction a] -> IO (Double, [a])
throughputMain' timeout ws = do
    -- Record start time.
    start <- getTime

    -- Start threads
    (unzip -> (ts,rs),vs) <- fmap unzip . forM ws $ \w -> do
        v <- newEmptyMVar
        r <- finally w (putMVar v ())
        return (r,v)

    -- Wait for time
    threadDelay timeout

    -- Kill threads
    mapM_ killThread ts
    mapM_ takeMVar vs   -- Wait for the finish signal

    -- result in total time
    end <- getTime

    return $! (end - start, rs)

foreign import ccall unsafe "throughput_gettime" getTime :: IO Double
