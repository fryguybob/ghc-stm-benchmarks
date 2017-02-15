{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE CPP                #-}
module Main where

import Control.Applicative
import Control.Monad
-- import Control.Monad.Random

import Control.Concurrent
import Control.Concurrent.STM

import Data.Maybe
import Data.Word

#ifdef RBTREE
#ifdef TSTRUCT
import RBTreeTStruct
#else
import RBTree
#endif
#elif STMTRIE
import STMTrie
#elif CUCKOO
import Cuckoo
#elif SKIPLIST
import SkipList
#elif CTRIE
import CTrie
#elif HASHMAP
import HashMap
#endif
import Throughput
import Opts

import System.Environment

import Debug.Trace

import PCG

type RGen = GenIO

initGens :: Int -> IO [RGen]
initGens threads = mapM initialize (map fromIntegral [1..threads])

samples :: Word -> Word -> RGen -> IO (Word,Word)
samples sampleMax total g = do
    r <- uniformB sampleMax g
    v <- uniformB (total - 2) g
    return (r, v+1)

#define VALUE 0
#define ATOMIC atomically

#ifdef RBTREE
type BenchTree = RBTree
mkBenchTree = mkRBTree
#elif CUCKOO
type BenchTree = RBTree
#elif SKIPLIST
type BenchTree = RBTree
#elif STMTRIE
#elif CTRIE
type BenchTree = RBTree
#define ATOMIC id
#elif HASHMAP
type BenchTree = AtomicTree ()
mkBenchTree = mkAtomicTree
#define VALUE ()
#define ATOMIC id
#endif

runRSTMEmpty :: CountIO -> RGen -> BenchTree -> Word -> Double -> IO ()
runRSTMEmpty count g t total readRate = go
  where
    insertRate = ((100 - readRate) / 2) + readRate
    sampleMax = 100000 :: Word

    toPercent :: Word -> Double
    toPercent r = fromIntegral r * 100 / fromIntegral sampleMax

    go = do
      !(toPercent -> !r,!v) <- samples sampleMax total g
      case () of
        () | r <= readRate   -> ATOMIC (doNothing t v)
           | r <= insertRate -> ATOMIC (doNothing t v)
           | otherwise       -> ATOMIC (doNothing t v)
      incCount count
      go
    {-# NOINLINE go #-}

    doNothing t 0 = return ()
    doNothing t _ = return ()


runRSTMSingle :: CountIO -> RGen -> BenchTree -> Word -> Double -> IO ()
runRSTMSingle count g t total readRate = go
  where
    insertRate = ((100 - readRate) / 2) + readRate
    sampleMax = 100000 :: Word

    readLevel :: Word
    !readLevel   = floor $ readRate / 100.0 * fromIntegral sampleMax
    insertLevel :: Word
    !insertLevel = floor $ insertRate / 100.0 * fromIntegral sampleMax

    toPercent :: Word -> Double
    toPercent r = fromIntegral r * 100 / fromIntegral sampleMax

    go = do
      !(!r,!v) <- samples sampleMax total g
      case () of
        () | r <= readLevel   -> ATOMIC (get t v          >> return ())
           | r <= insertLevel -> ATOMIC (insert t v VALUE >> return ())
           | otherwise        -> ATOMIC (delete t v       >> return ())
      incCount count
      go
    {-# NOINLINE go #-}

runRSTMSingle' :: CountIO -> RGen -> BenchTree -> Word -> Double -> IO ()
runRSTMSingle' count g t total readRate = go
  where
    go = do
      ATOMIC (get t 1 >> return ())
      go
    {-# NOINLINE go #-}

runRSTMSingleNoWrite :: Word -> Word -> CountIO -> RGen -> BenchTree -> Word -> Double -> IO ()
runRSTMSingleNoWrite vi vd count g t total readRate = go
  where
    insertRate = ((100 - readRate) / 2) + readRate
    sampleMax = 100000 :: Word

    readLevel :: Word
    !readLevel   = floor $ readRate / 100.0 * fromIntegral sampleMax
    insertLevel :: Word
    !insertLevel = floor $ insertRate / 100.0 * fromIntegral sampleMax

    toPercent :: Word -> Double
    toPercent r = fromIntegral r * 100 / fromIntegral sampleMax

    go = do
      !(!r,!v) <- samples sampleMax total g
      case () of
        () | r <= readLevel   -> ATOMIC (get t v           >> return ())
           | otherwise        -> ATOMIC (insert t vi VALUE >> return ())
      incCount count
      go
    {-# NOINLINE go #-}


main :: IO ()
main = do
    prog <- getProgName
    opts <- getOpts

    setNumCapabilities (_threads opts)

    let !e = _entries    opts
        !m = _mix        opts
        !s = _throughput opts

    (g':gs) <- initGens (_threads opts + 1)

    (_,v')  <- samples 100000 e g'
    (_,v'') <- samples 100000 e g'

    when (v' == v'') $ putStrLn $ "v' == v'' (" ++ show v' ++ ")"

    t <- ATOMIC mkBenchTree
    forM_ [0,2..e] $ \a -> ATOMIC $ insert t a VALUE

    cs <- replicateM (_threads opts) $ newCount 0

    unless (_initOnly opts) $ do 
      -- loop forever, stopping after s milliseconds.
      (t,ta) <- case () of
            () | _withoutTM opts -> 
                    throughputMain (s * 1000) (zipWith (\c g -> runRSTMEmpty  c g t e m) cs gs)
               | _nowrites opts ->
                    throughputMain (s * 1000)
                        (zipWith (\c g -> runRSTMSingleNoWrite v' v'' c g t e m) cs gs)
               | otherwise ->
                    throughputMain (s * 1000) (zipWith (\c g -> runRSTMSingle c g t e m) cs gs)
      trans <- sum <$> forM cs readCount
      putStrLn $ unwords [ "benchdata:"
                         , "run-time"    , show t
                         , "no-kill-time", show ta
                         , "transactions", show trans
                         , "prog"        , prog
                         , "threads"     , show (_threads opts)
                         , "entries"     , show e
                         ]
