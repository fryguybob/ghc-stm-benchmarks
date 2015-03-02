{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE ViewPatterns       #-}
module Main where

import Control.Applicative
import Control.Monad

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue

import Data.Maybe

import Throughput

import Options.Applicative

import System.Environment

import Debug.Trace
import Data.IORef

data BenchOpts = BenchOpts
    { _entries      :: Int
    , _threads      :: Int
    , _initOnly     :: Bool
    , _throughput   :: Int
    } deriving (Show)

benchOpts :: Parser BenchOpts
benchOpts = BenchOpts
    <$> (option auto)
        (value 800 <> long "entries"      <> short 'e' <> help "Number of values in each queue (one queue per thread)")
    <*> (option auto)
        (value 8   <> long "threads"      <> short 't' <> help "Number of threads")
    <*> switch
        (             long "initOnly"     <> short 'i' <> help "Initialize only")
    <*> (option auto)
        (value 1000<> long "throughput"   <> short 's' <> help "Throughput runtime in milliseconds")

initQueues :: Int -> Int -> IO ([IORef Int], [TBQueue Int], [IO ()])
initQueues queues entries = do
    (rs,qs) <- unzip <$> replicateM queues ((,) <$> newIORef 0 <*> newTBQueueIO (entries * 2))
    let ps = (last qs, head qs) : (zip <*> tail $ qs)
        acts = (flip map) (zip rs ps) $ \(count,(a,b)) -> forever $ do
--            traceEventIO "beginT"
            atomically $ do
                v <- readTBQueue a
                writeTBQueue b v
--            traceEventIO "endT"
            atomicModifyIORef' count (\a -> (a+1,()))

    forM_ qs $ \q -> atomically $ do
        forM_ [1..entries] $ \i-> writeTBQueue q i

    return (rs,qs,acts)

main :: IO ()
main = do
    prog <- getProgName
    let p = info (helper <*> benchOpts)
                (fullDesc <> progDesc "Queue benchmark." <> header prog)
    opts <- execParser p

    setNumCapabilities (_threads opts)

    let !e = _entries    opts
        !s = _throughput opts

    (rs,qs,acts) <- initQueues (_threads opts) e

    unless (_initOnly opts) $ do 

      -- loop forever, stopping after s milliseconds.
      t <- throughputMain (s * 1000) (acts)

      trans <- sum <$> forM rs readIORef 
      putStrLn $ unwords [ "benchdata:"
                         , "run-time"    , show t
                         , "transactions", show trans
                         , "prog"        , prog
                         , "threads"     , show (_threads opts)
                         , "entries"     , show e
                         ]
