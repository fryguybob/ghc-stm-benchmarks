{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
module Throughput 
    ( throughputMain
    , throughputMain'
    , locallyCountingForever
    , locallyCountingForever'
    , locallyCountingIterate
    , locallyCountingIterate'
    , voidForever
    , ThroughputAction

    , newCount
    , readCount
    , incCount
    , Count(..)
    , CountIO
    ) where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Primitive
import Control.Applicative
import Control.Exception (finally)

import Data.IORef
import Data.Primitive.ByteArray
import Data.Primitive.Types

import GHC.Word

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
#ifdef BYTECOUNTER
newtype Count s = C (MutableByteArray s)

type CountIO = Count RealWorld

newCount :: PrimMonad m => Word64 -> m (Count (PrimState m))
newCount i = do
    a <- newByteArray 48 -- 8 (fill a whole cacheline instead)
    writeByteArray a 0 i
    return $! C a
{-# INLINE newCount #-}

readCount :: PrimMonad m => Count (PrimState m) -> m Word64
readCount (C a) = readByteArray a 0
{-# INLINE readCount #-}

incCount :: PrimMonad m => Count (PrimState m) -> m ()
incCount (C a) = do
    c <- readByteArray a 0
    writeByteArray a 0 (c+1::Word64)
{-# INLINE incCount #-}
#else
newtype Count = C (IORef Word64)
type CountIO = Count

newCount :: Word64 -> IO Count
newCount i = do
    a <- newIORef i
    return $! C a
{-# INLINE newCount #-}

readCount :: Count -> IO Word64
readCount (C a) = readIORef a
{-# INLINE readCount #-}

incCount :: Count -> IO ()
incCount (C a) = do
    modifyIORef' a succ
{-# INLINE incCount #-}
#endif


locallyCountingIterate' :: Int -> (a -> IO a) -> a -> ThroughputAction (IO Word64)
locallyCountingIterate' i step initial = do
    c <- newCount 0
    t <- forkOn i $ act c initial
    return (t, readCount c)
  where
    act c x = do
        x' <- step x
        incCount c
        act c x'

locallyCountingIterate :: (a -> IO a) -> a -> ThroughputAction (IO Word64)
locallyCountingIterate step initial = do
    c <- newCount 0
    t <- forkIO $ act c initial
    return (t, readCount c)
  where
    act c x = do
        x' <- step x
        incCount c
        act c x'


locallyCountingForever' :: Int -> IO () -> ThroughputAction (IO Word64)
locallyCountingForever' i act = do
    c <- newCount 0
    t <- forkOn i . forever $ (incCount c >> act)
    return (t, readCount c)

locallyCountingForever :: IO () -> ThroughputAction (IO Word64)
locallyCountingForever act = do
    c <- newCount 0
    t <- forkIO . forever $ (incCount c >> act)
    return (t, readCount c)

voidForever :: Int -> IO () -> ThroughputAction ()
voidForever i act = do
    t <- forkOn i . forever $ act
    return (t, ())

throughputMain :: Int -> [IO ()] -> IO (Double,Double)
throughputMain timeout ws = fst <$> throughputMain' timeout (zipWith voidForever [0..length ws-1] ws)

throughputMain' :: Int -> [ThroughputAction a] -> IO ((Double,Double), [a])
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

    endA <- getTime

    -- Kill threads
    mapM_ killThread ts
    mapM_ takeMVar vs   -- Wait for the finish signal

    -- result in total time
    end <- getTime

    return $! ((end - start, endA - start), rs)


foreign import ccall unsafe "throughput_gettime" getTime :: IO Double
