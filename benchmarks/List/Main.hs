{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Random

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM

import Data.List.Split

import TList

import System.Console.CmdArgs.Implicit
import System.Environment
import System.Random.Shuffle

data Opts = Opts
    { entries      :: Int
    , threads      :: Int
    , initOnly     :: Bool
    , grow         :: Bool
    } deriving (Show, Data, Typeable)

opts :: String -> Opts
opts prog = Opts
    { entries      = 800
                  &= help "Number of values in the list"
    , threads      = 8
                  &= help "Number of threads"
    , initOnly     = False
                  &= help "Initialize only"
    , grow         = False
                  &= help "Grow the list by increasing values for each insert"
    }
    &= program prog

run :: Ord a => TList a -> [(a,a)] -> IO ()
run l vs = do
    forM_ vs $ \(a,b) -> do
        atomically $ remove l a
        atomically $ insert l b

main :: IO ()
main = do
    prog <- getProgName
    Opts{..} <- cmdArgs (opts prog)

    setNumCapabilities threads

    let g       = mkStdGen 42
        (as,bs) = if grow
                    then ([0..entries-1],[])
                    else flip evalRand g  $ (,) <$> shuffleM [0..entries-1]
                                                <*> shuffleM [entries..entries*2-1]

    t <- atomically mkTList
    forM_ as $ \a -> atomically $ insert t a

    unless initOnly $ do
        ts <- forM (chunksOf (entries `div` threads) (zip as bs)) $ \a -> async (run t a)

        forM_ ts wait
