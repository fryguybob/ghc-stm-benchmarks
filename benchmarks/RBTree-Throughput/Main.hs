{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE CPP                #-}
module Main where

import Control.Applicative
import Control.Monad
-- import Control.Monad.Random

import Control.Concurrent
#ifdef PASTMTL2
import Control.TL2.STM
#else
import Control.Concurrent.STM
#endif

import Data.Maybe
import Data.Word

#ifdef TSTRUCT
import RBTreeTStruct
#elif STMTRIE
import RBTreeSTMTrie
#elif CUCKOOTSTRUCT
import RBTreeCuckoo
#elif SKIPLIST
import RBTreeSkipList
#elif CTRIE
import RBTreeCTrie
#elif HASHMAP
import RBTreeHashMap
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
#define ATOMIC atomically
#elif CUCKOOTSTRUCT
type BenchTree = RBTree
#define VALUE 0
#define ATOMIC atomically
#elif SKIPLIST
type BenchTree = RBTree
#define VALUE 0
#define ATOMIC atomically
#elif STMTRIE
type BenchTree = RBTree
#define VALUE 0
#define ATOMIC atomically
#elif CTRIE
type BenchTree = RBTree
#define VALUE 0
#define ATOMIC id
#elif HASHMAP
type BenchTree = AtomicTree ()
mkRBTree = mkAtomicTree
#define VALUE ()
#define ATOMIC id
#else
type BenchTree = RBTree Word ()
#define VALUE ()
#define ATOMIC atomically
#endif

data RBTreeOpts = RBTreeOpts
    { _entries      :: Word
    , _threads      :: Int
    , _initOnly     :: Bool
    , _withoutTM    :: Bool
    , _nowrites     :: Bool
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
    <*> switch
        (             long "nowrites"     <> short 'n' <> help "Noish writes.  Inserts all use same key x and deletes all use same key y across all threads. ")
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
        () | r <= readRate   -> ATOMIC (doNothing t v)
           | r <= insertRate -> ATOMIC (doNothing t v)
           | otherwise       -> ATOMIC (doNothing t v)
--      traceEventIO "endT"
      incCount count
      go g'
    {-# NOINLINE go #-}

--    doNothing :: BenchTree -> Word -> STM ()
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
        () | r <= readLevel   -> ATOMIC (get t v          >> return ())
           | r <= insertLevel -> ATOMIC (insert t v VALUE >> return ())
           | otherwise        -> ATOMIC (delete t v       >> return ())
--      traceEventIO "endT"
      incCount count
      go g'
    {-# NOINLINE go #-}

runRSTMSingle' :: CountIO -> RGen -> BenchTree -> Word -> Double -> IO ()
runRSTMSingle' count g t total readRate = go
  where
    go = do
      ATOMIC (get t 1 >> return ())
      go
    {-# NOINLINE go #-}

runRSTMSingleNoWrite :: Word -> Word -> CountIO -> RGen -> BenchTree -> Word -> Double -> IO ()
runRSTMSingleNoWrite vi vd count g t total readRate = go g
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
        () | r <= readLevel   -> ATOMIC (get t v           >> return ())
           -- | r <= insertLevel -> ATOMIC (insert t vi VALUE >> return ())
           | otherwise        -> ATOMIC (insert t vi VALUE >> return ())
--            | otherwise        -> ATOMIC (delete t vi       >> return ())
--           | otherwise        -> ATOMIC (delete t vd       >> return ()) -- <-- different v's inserted and deleted ensure inserts and deletes are read-only
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

    (g':gs) <- initGens (_threads opts + 1)

    ((_,v'), g'') <- samples 100000 e g'
    ((_,v''),_ )  <- samples 100000 e g''

    when (v' == v'') $ putStrLn $ "v' == v'' (" ++ show v' ++ ")"

    t <- ATOMIC mkRBTree
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
