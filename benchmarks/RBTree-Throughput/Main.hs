{-# LANGUAGE RecordWildCards, DeriveDataTypeable, BangPatterns #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Random

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM

import Data.List.Split hiding (split)
import Data.Maybe

import RBTree
import Throughput

import System.Console.CmdArgs.Implicit
import System.Environment
import System.Random.Shuffle

data RBTreeOpts = RBTreeOpts
    { entries      :: Int
    , threads      :: Int
    , initOnly     :: Bool
    , repeats      :: Int
    , readOnly     :: Bool
    , atomicGroups :: Int
    , mix          :: Maybe Int
    , throughput   :: Maybe Int
    } deriving (Show, Data, Typeable)

rbTreeOpts :: String -> RBTreeOpts
rbTreeOpts prog = RBTreeOpts
    { entries      = 800
                  &= help "Number of values in the tree"
    , threads      = 8
                  &= help "Number of threads"
    , initOnly     = False
                  &= help "Initialize only"
    , repeats      = 1
                  &= help "Repeat count"
    , readOnly     = False
                  &= help "Read only"
                  &= name "l"
    , atomicGroups = 1
                  &= help "Lookups per transaction"
                  &= name "g"
    , mix          = Nothing
                  &= help "Read mix percent"
    , throughput   = Nothing
                  &= help "Throughput runtime in milliseconds"
                  &= name "s"
    }
    &= program prog

swap ~(a,b) = (b,a)

run :: Ord a => RBTree a () -> [[(a,a)]] -> Int -> IO ()
run _ _ 0 = return ()
run t kss repeats = do
    forM_ kss $ \ks -> do
        atomically $ forM_ ks $ \(a,b) -> do
            delete t a
            insert t b ()
    run t (map (map swap) kss) (repeats - 1)

runReads :: Ord a => RBTree a () -> [[a]] -> Int -> IO ()
runReads _ _ 0 = return ()
runReads t kss repeats = do
    vss <- forM kss $ \ks -> do
        vs <- atomically $ forM ks (get t)
        return (catMaybes vs)
    case sum . map length $ vss of
        0 -> print "None"
        _ -> return ()
    runReads t kss (repeats - 1)

runRSTM :: StdGen -> RBTree Int () -> Int -> Int -> Int -> Int -> IO ()
runRSTM g t total readRate repeats groups = go g repeats
  where
    insertRate = ((100 - readRate) `div` 2) + readRate
    go _ 0 = return ()
    go g i = do
      let (!ps,!g') = flip runRand g . replicateM groups 
                    $ (,) <$> getRandomR (1,100) <*> getRandomR (0,total-1)
      atomically . forM_ ps $ \(r,v) -> do
          case () of
            () | r <= readRate   -> get t v       >> return ()
               | r <= insertRate -> insert t v () >> return ()
               | otherwise       -> delete t v    >> return ()
      go g' (i - 1) 

main :: IO ()
main = do
    prog <- getProgName
    RBTreeOpts{..} <- cmdArgs (rbTreeOpts prog)

    setNumCapabilities threads
    l <- newEmptyMVar

    case (readOnly,mix) of -- this mirrors RSTM's TreeBench
      (_, Just m) -> do
        let g  = mkStdGen 42
            f _ 0 = []
            f g n = let (g1,g2) = split g
                    in  g1 : f g2 (n - 1)
            gs = f g threads 

        t <- atomically mkRBTree
        forM_ [0,2..entries] $ \a -> atomically $ insert t a ()
        
        unless initOnly $ case throughput of
            Nothing -> do
                forM_ gs $ \g -> forkIO $ (runRSTM g t entries m repeats atomicGroups >> putMVar l ())

                replicateM threads (takeMVar l) >> return ()
            Just s -> do
                -- loop forever, stopping after s milliseconds.
                t <- throughputMain (s * 1000) (map (\g -> runRSTM g t entries m (-1) atomicGroups) gs)
                print t

      (True, _) -> do
        let g  = mkStdGen 42
            as = flip evalRand g $ shuffleM [0..entries-1]

        t <- atomically mkRBTree
        forM_ as $ \a -> atomically $ insert t a ()

        unless initOnly $ do
            forM_ (chunksOf atomicGroups . chunksOf (entries `div` threads) $ as)
                  $ \as -> forkIO $ (runReads t as repeats >> putMVar l ())

            replicateM threads (takeMVar l) >> return ()

      _    -> do
        let g       = mkStdGen 42
            (as,bs) = flip evalRand g  $ (,) <$> shuffleM [0..entries-1]
                                             <*> shuffleM [entries..entries*2-1]
    
        t <- atomically mkRBTree
        forM_ as $ \a -> atomically $ insert t a ()
    
        unless initOnly $ do
            forM_ (chunksOf atomicGroups . chunksOf (entries `div` threads) $ (zip as bs)) 
                  $ \a -> forkIO $ (run t a repeats >> putMVar l ())
    
            replicateM threads (takeMVar l) >> return ()