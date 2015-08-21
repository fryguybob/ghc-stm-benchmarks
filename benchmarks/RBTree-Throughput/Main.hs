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

#ifdef TSTRUCT
import RBTreeTStruct
#else
import RBTree
#endif
import Throughput

import Options.Applicative

import System.Environment

import Debug.Trace
import qualified Data.Vector as V
#ifdef MWC
#undef MWC
import System.Random.MWC

type RGen = GenIO

initGens :: Int -> IO [RGen]
initGens threads = mapM initialize (map (V.singleton . fromIntegral) [1..threads])

samples :: Word -> Word -> RGen -> IO ((Word,Word), RGen)
samples sampleMax total g = do
    r <- uniformR (0, sampleMax  ) g
    v <- uniformR (1, (total - 1)) g
    return ((r, v), g)
#else
import System.Random.PCG.Fast.Pure

type RGen = GenIO

initGens :: Int -> IO [RGen]
initGens threads = mapM initialize (map fromIntegral [1..threads])

samples :: Word -> Word -> RGen -> IO ((Word,Word), RGen)
samples sampleMax total g = do
    r <- uniformB sampleMax g
    v <- uniformB (total - 2) g
    return ((r, v+1), g)
#endif

#ifdef TSTRUCT
type BenchTree = RBTree
#define VALUE 0
#else
type BenchTree = RBTree Word ()
#define VALUE ()
#endif

data RBTreeOpts = RBTreeOpts
    { _entries      :: Word
    , _threads      :: Int
    , _initOnly     :: Bool
    , _withoutTM    :: Bool
    , _atomicGroups :: Int
    , _mix          :: Double
    , _throughput   :: Int
    } deriving (Show)

rbTreeOpts :: Parser RBTreeOpts
rbTreeOpts = RBTreeOpts
    <$> (option auto)
        (value 800 <> long "entries"      <> short 'e' <> help "Number of values in the tree")
    <*> (option auto)
        (value 8   <> long "threads"      <> short 't' <> help "Number of threads")
    <*> switch
        (             long "initOnly"     <> short 'i' <> help "Initialize only")
    <*> switch
        (             long "withoutTM"    <> short 'w' <> help "No transactions")
    <*> (option auto)
        (value 1   <> long "atomicGroups" <> short 'g' <> help "Lookups per transaction")
    <*> (option auto)
        (value 90  <> long "mix"          <> short 'm' <> help "Read mix percent")
    <*> (option auto)
        (value 1000<> long "throughput"   <> short 's' <> help "Throughput runtime in milliseconds")

runRSTMEmpty :: CountIO -> RGen -> BenchTree -> Word -> Double -> IO ()
runRSTMEmpty count g t total readRate = go g
  where
    insertRate = ((100 - readRate) / 2) + readRate
    sampleMax = 100000 :: Word

    toPercent :: Word -> Double
    toPercent r = fromIntegral r * 100 / fromIntegral sampleMax

    go g = do
      (!(toPercent -> !r,!v),!g') <- samples sampleMax total g
--      traceEventIO "beginT"
      case () of
        () | r <= readRate   -> atomically (doNothing t v)
           | r <= insertRate -> atomically (doNothing t v)
           | otherwise       -> atomically (doNothing t v)
--      traceEventIO "endT"
      incCount count
      go g'
    {-# NOINLINE go #-}

    doNothing :: BenchTree -> Word -> STM ()
    doNothing t 0 = return ()
    doNothing t _ = return ()

runRSTMSingle :: CountIO -> RGen -> BenchTree -> Word -> Double -> IO ()
runRSTMSingle count g t total readRate = go g
  where
    insertRate = ((100 - readRate) / 2) + readRate
    sampleMax = 100000 :: Word

    readLevel :: Word
    !readLevel   = floor $ readRate / 100.0 * fromIntegral sampleMax
    insertLevel :: Word
    !insertLevel = floor $ insertRate / 100.0 * fromIntegral sampleMax

    toPercent :: Word -> Double
    toPercent r = fromIntegral r * 100 / fromIntegral sampleMax

    go g = do
      (!(!r,!v),!g') <- samples sampleMax total g
      --      traceEventIO "beginT"
      case () of
        () | r <= readLevel   -> atomically (get t v          >> return ())
           | r <= insertLevel -> atomically (insert t v VALUE >> return ())
           | otherwise        -> atomically (delete t v       >> return ())
--      traceEventIO "endT"
      incCount count
      go g'
    {-# NOINLINE go #-}

main :: IO ()
main = do
    prog <- getProgName
    let p = info (helper <*> rbTreeOpts)
                (fullDesc <> progDesc "RBTree benchmark." <> header prog)
    opts <- execParser p

    setNumCapabilities (_threads opts)

    let !e = _entries    opts
        !m = _mix        opts
        !s = _throughput opts

    gs <- initGens (_threads opts)

    t <- atomically mkRBTree
    forM_ [0,2..e] $ \a -> atomically $ insert t a VALUE

    cs <- replicateM (_threads opts) $ newCount 0

    unless (_initOnly opts) $ do 
      -- loop forever, stopping after s milliseconds.
      (t,ta) <- case () of
            () | _withoutTM opts -> 
                    throughputMain (s * 1000) (zipWith (\c g -> runRSTMEmpty  c g t e m) cs gs)
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
