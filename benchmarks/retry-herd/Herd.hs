{-# LANGUAGE BangPatterns       #-}
module Herd where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Control.Applicative

import Data.IORef

import Options.Applicative

import System.Environment

import Throughput


data HerdOpts = HerdOpts
    { _entries      :: Int
    , _threads      :: Int
    , _initOnly     :: Bool
    , _throughput   :: Int
    } deriving (Show)

herdOpts :: Parser HerdOpts
herdOpts = HerdOpts
    <$> (option auto)
        (value 800 <> long "entries" <> short 'e' <> help "Number of initial blocked transactions")
    <*> (option auto)
        (value   8 <> long "threadss" <> short 't' <> help "Number of threads")
    <*> switch (long "init-only" <> short 'i' <> help "Initialize Only")
    <*> (option auto)
        (value 1000 <> long "throughput" <> short 's' <> help "Throughput runtime in milliseconds")

data Herd a = Herd { _next :: (TVar (Herd a)), _value :: TVar a }

initHerd :: a -> Int -> IO (Herd a)
initHerd a 1 = mfix $ \l -> do
    t <- newTVarIO l
    v <- newTVarIO a
    return $ Herd t v

initHerd a n = insertHerd a =<< initHerd a (n-1)

insertHerd :: a -> Herd a -> IO (Herd a)
insertHerd a l@(Herd tn _) = atomically $ do
    n <- readTVar tn
    l' <- Herd <$> newTVar n <*> newTVar a 
    writeTVar tn l'
    return l'

foldHerd :: (a -> b -> b) -> b -> Herd a -> STM b
foldHerd f z h = readTVar end >>= go z
  where
    end = _next h
    go !b (Herd t tv)
        | t == end  = do
            v <- readTVar tv
            return $ f v b
        | otherwise = do
            v <- readTVar tv
            readTVar t >>= go (f v b)

sumHerd :: Num a => Herd a -> STM a
sumHerd = foldHerd (+) 0

stepHerd :: (a -> a) -> Herd a -> STM ()
stepHerd f (Herd end tv) = modifyTVar tv f >> readTVar end >>= go
  where
    go (Herd t tv)
        | t == end  = return ()
        | otherwise = modifyTVar tv f >> readTVar t >>= go

windowHerd :: (a -> a -> (a, a)) -> Herd a -> STM ()
windowHerd f h = do
    a  <- readTVar (_value h)
    h' <- readTVar (_next h)
    b  <- readTVar (_value h')
    let (a', b') = f a b
    writeTVar (_value h ) a
    writeTVar (_value h') b

process :: Enum a => Herd a -> IO (Herd a)
process (Herd t tv) = atomically $ do
    modifyTVar tv succ
    readTVar t


stepHerdIO :: (TVar a -> IO ()) -> Herd a -> IO [IO ()]
stepHerdIO act (Herd end tv) = do
    next <- readTVarIO end
    (act tv :) <$> go next
  where
    go (Herd t tv)
        | t == end  = return []
        | otherwise = do
            next <- readTVarIO t
            (act tv :) <$> go next

threadInits :: (Ord a, Num a) => Herd a -> IO [IO ()]
threadInits h = do
    flip stepHerdIO h $ \t -> atomically $ do
            x <- readTVar t
            if x > 0
              then writeTVar t (x - 1)
              else retry

main :: IO ()
main = do
    prog <- getProgName
    let p = info (helper <*> herdOpts)
                (fullDesc <> progDesc "retry hurd benchmark." <> header prog)
    opts <- execParser p

    setNumCapabilities (_threads opts)

    h <- initHerd 0 (_entries opts)
    stop <- newTVarIO False

    mkThreads <- threadInits h

    unless (_initOnly opts) $ do

-- The following counts and tracks all the threads, but I think
-- killing the threads incurs a lot of overhead :(.  So I will
-- let the small threads live and live with a racy read of the
-- transaction count.
--
--     (t,cs) <- throughputMain' 1000000 
--             $ locallyCountingIterate process h 
--             : (map locallyCountingForever $ mkThreads)

      readCounts <- map snd <$> sequence
        (zipWith locallyCountingForever' [1..] mkThreads)

      (t,[c]) <- throughputMain' (_throughput opts*1000) [locallyCountingIterate' 0 process h]
  
      let cs = c : readCounts
      
      v <- sum <$> sequence cs
  
      putStrLn . unlines . map concat $
          [ ["Time: ", show t , " seconds"]
          , ["Transactions: ", show v]
          , ["Throughput: ", show (fromIntegral v / t)]
          ] 
