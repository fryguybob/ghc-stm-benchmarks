{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE ViewPatterns       #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Random

import Control.Concurrent

import Data.IORef
import qualified Data.IntMap as M
import Data.Maybe

import Throughput

import Options.Applicative

import System.Environment

import Debug.Trace

data BenchOpts = BenchOpts
    { _entries      :: Int
    , _threads      :: Int
    , _initOnly     :: Bool
    , _mix          :: Double
    , _throughput   :: Int
    } deriving (Show)

benchOpts :: Parser BenchOpts
benchOpts = BenchOpts
    <$> (option auto)
        (value 800 <> long "entries"      <> short 'e' <> help "Number of values in the tree")
    <*> (option auto)
        (value 8   <> long "threads"      <> short 't' <> help "Number of threads")
    <*> switch
        (             long "initOnly"     <> short 'i' <> help "Initialize only")
    <*> (option auto)
        (value 90  <> long "mix"          <> short 'm' <> help "Read mix percent")
    <*> (option auto)
        (value 1000<> long "throughput"   <> short 's' <> help "Throughput runtime in milliseconds")

newtype AtomicTree v = AtomicTree { runAtomicTree :: IORef (M.IntMap v) }

get :: AtomicTree v -> Int -> IO (Maybe v)
get (AtomicTree p) k = (M.lookup k) <$> readIORef p

insert :: AtomicTree v -> Int -> v -> IO ()
insert (AtomicTree p) k v = atomicModifyIORef' p (\m -> (M.insert k v m,()))

delete :: AtomicTree v -> Int -> IO ()
delete (AtomicTree p) k = atomicModifyIORef' p (\m -> (M.delete k m, ()))

mkAtomicTree :: IO (AtomicTree v)
mkAtomicTree = AtomicTree <$> newIORef M.empty

runRSTM :: IORef Int -> StdGen -> AtomicTree () -> Int -> Double -> IO ()
runRSTM count g t total readRate = go g
  where
    insertRate = ((100 - readRate) / 2) + readRate

    sampleMax = 100000 :: Int

    toPercent :: Int -> Double
    toPercent r = fromIntegral r * 100 / fromIntegral sampleMax

    go g = do
      let (!(toPercent -> !r,!v),!g') = flip runRand g
                    $ (,) <$> getRandomR (1,sampleMax) <*> getRandomR (0,total-1)
--      traceEventIO "beginT"
      case () of
        () | r <= readRate   -> get t v       >> return ()
           | r <= insertRate -> insert t v () >> return ()
           | otherwise       -> delete t v    >> return ()
--      traceEventIO "endT"
--      atomicModifyIORef' count (\a -> (a+1,()))
      modifyIORef' count succ
      go g'

main :: IO ()
main = do
    prog <- getProgName
    let p = info (helper <*> benchOpts)
                (fullDesc <> progDesc "Atomic IntMap benchmark." <> header prog)
    opts <- execParser p

    setNumCapabilities (_threads opts)

    let !e = _entries    opts
        !m = _mix        opts
        !s = _throughput opts

    -- this mirrors RSTM's TreeBench
    let g  = mkStdGen 42
        f _ 0 = []
        f !g !n = let (g1,g2) = split g
                in  g1 : f g2 (n - 1)
        !gs = f g (_threads opts)

    t <- mkAtomicTree
    forM_ [0,2..e] $ \a -> insert t a ()

    cs <- replicateM (_threads opts) $ newIORef 0
        
    unless (_initOnly opts) $ do
        -- loop forever, stopping after s milliseconds.
        t <- throughputMain (s * 1000) (zipWith (\c g -> runRSTM c g t e m) cs gs)
        trans <- sum <$> forM cs readIORef
        putStrLn $ unwords [ "benchdata:"
                           , "run-time"   , show t
                           , "transactions", show trans
                           , "prog"        , prog
                           , "threads"     , show (_threads opts)
                           , "entries"     , show e
                           ]

