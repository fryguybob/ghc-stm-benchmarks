{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE BangPatterns       #-}
module Main (main) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State

import Data.Acid
import Data.Acid.Remote
import qualified Data.Map as Map
import Data.SafeCopy
import Data.Typeable

import Debug.Trace

import System.Console.CmdArgs.Implicit
import System.Environment
import System.Exit
import System.IO


import Throughput

data BenchOpts = BenchOpts
    { _entries    :: Int
    , _threads    :: Int
    , _initOnly   :: Bool
    , _mix        :: Int
    , _throughput :: Int
    } deriving (Show, Data, Typeable)

makeLenses ''BenchOpts

benchOpts :: String -> BenchOpts
benchOpts prog = BenchOpts
    { _entries      = 800
                   &= help "Number of values starting in key value store"
                   &= name "e"
    , _threads      = 8
                   &= help "Number of threads"
                   &= name "t"
    , _initOnly     = False
                   &= help "Initialize only"
                   &= name "i"
    , _mix          = 90
                   &= help "Read mix percent"
                   &= name "m"
    , _throughput   = 1000
                   &= help "Throughput runtime in milliseconds"
                   &= name "s"
    }
    &= program prog

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

type Key = Int
type Value = ()

data KeyValue = KeyValue !(Map.Map Key Value)
    deriving (Typeable)

$(deriveSafeCopy 0 'base ''KeyValue)

------------------------------------------------------
-- The transactions we will execute over the state.

insertKey :: Key -> Value -> Update KeyValue ()
insertKey key value
    = do KeyValue m <- get
         put (KeyValue (Map.insert key value m))

lookupKey :: Key -> Query KeyValue (Maybe Value)
lookupKey key
    = do KeyValue m <- ask
         return (Map.lookup key m)

deleteKey :: Key -> Update KeyValue ()
deleteKey key
    = do KeyValue m <- get
         put (KeyValue (Map.delete key m))

$(makeAcidic ''KeyValue ['insertKey, 'lookupKey, 'deleteKey])

--------------------------------------------------------
-- Benchmark

runRSTM g acid total readRate = go g
  where
    insertRate = ((100 - readRate) `div` 2) + readRate
    go g = do
      let (!(!r,!v),!g') = flip runRand g
                    $ (,) <$> getRandomR (1,100) <*> getRandomR (0,total-1)
      traceEventIO "beginT"
      case () of
        () | r <= readRate   -> query  acid (LookupKey v)    >> return ()
           | r <= insertRate -> update acid (InsertKey v ()) >> return ()
           | otherwise       -> update acid (DeleteKey v)    >> return ()
      traceEventIO "endT"
      go g'

main :: IO ()
main = do 
    prog <- getProgName
    opts <- cmdArgs (benchOpts prog)

    setNumCapabilities (opts^.threads)

    let g  = mkStdGen 42
        f _ 0 = []
        f g n = let (g1,g2) = split g
                in  g1 : f g2 (n - 1)
        gs = f g (opts^.threads)

    l <- newEmptyMVar

    acid <- openLocalState (KeyValue Map.empty)

    forM_ [0,2..opts^.entries] $ \a -> update acid (InsertKey a ())

    unless (opts^.initOnly) $ do
      t <- throughputMain
             (opts^.throughput * 1000)
             (map (\g -> runRSTM g acid (opts^.entries) (opts^.mix)) gs)

      print t

    closeAcidState acid

