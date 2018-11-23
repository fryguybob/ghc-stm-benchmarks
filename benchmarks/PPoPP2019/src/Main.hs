{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE UnboxedTuples      #-}
module Main where

import Control.Applicative
import Control.Monad

import GHC.Conc.Sync
import Control.Concurrent

import Data.Maybe
import Data.Word

#ifdef RBTREE
import RBTree
#elif HAMT
import Hamt
#elif TREAP
import Treap
#else
#error Unknown Variant for Import
#endif

import Throughput
import System.Random.PCG.Fast.Pure

import System.Console.GetOpt
import System.Environment

import GHC.Prim (resetSTMStats#)
import GHC.Types

type RGen = GenIO

initGens :: Int -> IO [RGen]
initGens threads = mapM initialize (map fromIntegral [1..threads])

samples :: Word -> Word -> RGen -> IO ((Word,Word), RGen)
samples sampleMax total g = do
    r <- uniformB sampleMax g
    v <- uniformB (total - 2) g
    return ((r, v+1), g)

type BenchTree = Tree
#define VALUE 0
#define ATOMIC atomically

data BenchOpts = BenchOpts
    { _entries      :: Word
    , _threads      :: Int
    , _initOnly     :: Bool
    , _withoutTM    :: Bool
    , _mix          :: Double
    , _throughput   :: Int
    } deriving (Show)

benchDefs :: BenchOpts
benchDefs = BenchOpts
    { _entries    = 800
    , _threads    = 8
    , _initOnly   = False
    , _withoutTM  = False
    , _mix        = 90
    , _throughput = 1000
    }

benchOpts :: [OptDescr (BenchOpts -> BenchOpts)]
benchOpts =
  [ Option ['e'] ["entries"]
      (ReqArg (\v o -> o { _entries = read v}) "ENTRIES") "Number of values in the tree"
  , Option ['t'] ["threads"]
      (ReqArg (\v o -> o { _threads = read v}) "THREADS") "Number of threads"
  , Option ['i'] ["initOnly"]
      (NoArg (\o -> o { _initOnly = True }))         "Initialize only"
  , Option ['w'] ["withoutTM"]
      (NoArg (\o -> o { _withoutTM = True }))        "No transactions"
  , Option ['m'] ["mix"]
      (ReqArg (\v o -> o { _mix = read v}) "MIX")         "Read mix percent"
  , Option ['s'] ["throughput"]
      (ReqArg (\v o -> o { _throughput = read v}) "TIME")
      "Throughput runtime in milliseconds"
  ]

getBenchOpts = do
    prog <- getProgName
    let header = "Usage: " ++ prog
    argv <- getArgs
    case getOpt Permute benchOpts argv of
      (o, n, []) -> return (foldl (flip id) benchDefs o)
      (_, _, es) -> ioError (userError (concat es ++ usageInfo header benchOpts))

runRSTMEmpty :: CountIO -> RGen -> BenchTree -> Word -> Double -> IO ()
runRSTMEmpty count g t total readRate = go g
  where
    insertRate = ((100 - readRate) / 2) + readRate
    sampleMax = 100000 :: Word

    toPercent :: Word -> Double
    toPercent r = fromIntegral r * 100 / fromIntegral sampleMax

    go g = do
      (!(toPercent -> !r,!v),!g') <- samples sampleMax total g
      case () of
        () | r <= readRate   -> ATOMIC (doNothing t v)
           | r <= insertRate -> ATOMIC (doNothing t v)
           | otherwise       -> ATOMIC (doNothing t v)
      incCount count
      go g'
    {-# NOINLINE go #-}

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

#ifndef TESTCODE
    go g = do
      (!(!r,!v),!g') <- samples sampleMax total g
      case () of
        () | r <= readLevel   -> ATOMIC (get t v          >> return ())
           | r <= insertLevel -> ATOMIC (insert t v VALUE >> return ())
           | otherwise        -> ATOMIC (delete t v       >> return ())
      incCount count
      go g'
    {-# NOINLINE go #-}

#else
    go g = do
      (!(!r,!v),!g') <- samples sampleMax total g
      case () of
        () | r <= readLevel   -> ATOMIC (get t v          >> return ())
           | r <= insertLevel -> ATOMIC (insert t v VALUE >> return ()) >> hasValue t v -- single-threaded sanity check
           | otherwise        -> ATOMIC (delete t v       >> return ()) >> noValue t v
      incCount count
      go g'
    {-# NOINLINE go #-}

hasValue t v = do
  x <- ATOMIC (get t v)
  case x of
    Just _  -> return ()
    Nothing -> print (v, "missing")

noValue t v = do
  x <- ATOMIC (get t v)
  case x of
    Nothing -> return ()
    _       -> print (v, "found")
#endif

resetSTMStats :: IO ()
resetSTMStats = IO $ \s# -> case resetSTMStats# s# of s'# -> (# s'#, () #)

main :: IO ()
main = do
    prog <- getProgName
    opts <- getBenchOpts

    setNumCapabilities (_threads opts)

    let !e = _entries    opts
        !m = _mix        opts
        !s = _throughput opts

    (g':gs) <- initGens (_threads opts + 1)

    ((_,v'), g'') <- samples 100000 e g'
    ((_,v''),_ )  <- samples 100000 e g''

    when (v' == v'') $ putStrLn $ "v' == v'' (" ++ show v' ++ ")"

    t <- ATOMIC mkTree
    forM_ [0,2..e] $ \a -> ATOMIC $ insert t a VALUE

    unless (_initOnly opts) $ resetSTMStats

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
                         , "code"        , benchCode
                         ]
