{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE ViewPatterns       #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Random

import Control.Concurrent
import Control.Concurrent.STM

import Data.Maybe

import qualified STMContainers.Map as M
import Throughput

import Options.Applicative

import System.Environment

import Debug.Trace

type RBTree k v = M.Map k v

get t k = M.lookup k t
insert t k v = M.insert v k t
delete t k   = M.delete k t

mkRBTree = M.new

data RBTreeOpts = RBTreeOpts
    { _entries      :: Int
    , _threads      :: Int
    , _initOnly     :: Bool
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
    <*> (option auto)
        (value 1   <> long "atomicGroups" <> short 'g' <> help "Lookups per transaction")
    <*> (option auto)
        (value 90  <> long "mix"          <> short 'm' <> help "Read mix percent")
    <*> (option auto)
        (value 1000<> long "throughput"   <> short 's' <> help "Throughput runtime in milliseconds")

runRSTMSingle :: StdGen -> RBTree Int () -> Int -> Double -> IO ()
runRSTMSingle g t total readRate = go g
  where
    insertRate = ((100 - readRate) / 2) + readRate

    sampleMax = 100000 :: Int

    toPercent :: Int -> Double
    toPercent r = fromIntegral r * 100 / fromIntegral sampleMax

    go g = do
      let (!(toPercent -> !r,!v),!g') = flip runRand g $ (,) <$> getRandomR (1,sampleMax) <*> getRandomR (0,total-1)
      traceEventIO "beginT"
      case () of
        () | r <= readRate   -> atomically (get t v       >> return ())
           | r <= insertRate -> atomically (insert t v () >> return ())
           | otherwise       -> atomically (delete t v    >> return ())
      traceEventIO "endT"
      go g'

runRSTM :: StdGen -> RBTree Int () -> Int -> Double -> Int -> IO ()
runRSTM g t total readRate groups = go g
  where
    insertRate = ((100 - readRate) / 2) + readRate

    sampleMax = 100000 :: Int

    toPercent :: Int -> Double
    toPercent r = fromIntegral r * 100 / fromIntegral sampleMax

    go g = do
      let (!ps,!g') = flip runRand g . replicateM groups 
                    $ (,) <$> getRandomR (1,sampleMax) <*> getRandomR (0,total-1)
      traceEventIO "beginT"
      atomically . forM_ ps $ \(toPercent -> r,v) -> do
          case () of
            () | r <= readRate   -> get t v       >> return ()
               | r <= insertRate -> insert t v () >> return ()
               | otherwise       -> delete t v    >> return ()
      traceEventIO "endT"
      go g'

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

    -- this mirrors RSTM's TreeBench
    let g  = mkStdGen 42
        f _ 0   = []
        f !g !n = let (g1,g2) = split g
                in  g1 : f g2 (n - 1)
        !gs = f g (_threads opts)

    t <- atomically mkRBTree
    forM_ [0,2..e] $ \a -> atomically $ insert t a ()
        
    unless (_initOnly opts) $ do 

      -- loop forever, stopping after s milliseconds.
      t <- if _atomicGroups opts == 1
             then throughputMain (s * 1000) (map (\g -> runRSTMSingle g t e m) gs)
             else throughputMain (s * 1000)
                         (map (\g -> runRSTM g t e m (_atomicGroups opts)) gs)
      print t
