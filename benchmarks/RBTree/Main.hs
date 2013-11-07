{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Random

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM

import Data.List.Split

import RBTree

import System.Console.CmdArgs.Implicit
import System.Environment
import System.Random.Shuffle

data RBTreeOpts = RBTreeOpts
    { entries      :: Int
    , threads      :: Int
    , initOnly     :: Bool
    } deriving (Show, Data, Typeable)

rbTreeOpts :: String -> RBTreeOpts
rbTreeOpts prog = RBTreeOpts
    { entries      = 800
                  &= help "Number of values in the tree"
    , threads      = 8
                  &= help "Number of threads"
    , initOnly     = False
                  &= help "Initialize only"
    }
    &= program prog

run :: Ord a => RBTree a () -> [(a,a)] -> IO ()
run t vs = do
    forM_ vs $ \(a,b) -> do
        atomically $ delete t a
        atomically $ insert t b ()

main :: IO ()
main = do
    prog <- getProgName
    RBTreeOpts{..} <- cmdArgs (rbTreeOpts prog)

    setNumCapabilities threads

    let g       = mkStdGen 42
        (as,bs) = flip evalRand g  $ (,) <$> shuffleM [0..entries-1]
                                         <*> shuffleM [entries..entries*2-1]

    t <- atomically mkRBTree
    forM_ as $ \a -> atomically $ insert t a ()

    unless initOnly $ do
        ts <- forM (chunksOf (entries `div` threads) (zip as bs)) $ \a -> async (run t a)

        forM_ ts wait
