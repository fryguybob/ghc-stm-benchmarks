{-# LANGUAGE BangPatterns #-}
module Herd where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Control.Applicative

import Throughput

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



process :: Enum a => TVar Bool -> Herd a -> IO ()
process stop (Herd t tv) = (process' =<<) . atomically $ do
    modifyTVar tv succ
    b <- readTVar stop
    if b 
      then return Nothing
      else Just <$> readTVar t
  where
    process' Nothing  = return ()
    process' (Just h) = process stop h

main' = do
    h <- initHerd 0 1000
    stop <- newTVarIO False

    (atomically $ sumHerd h) >>= print

    forkIO $ process stop h

    threadDelay 1000

    atomically $ writeTVar stop True

    (atomically $ sumHerd h) >>= print


stepHerdIO :: (TVar a -> IO ()) -> Herd a -> IO ()
stepHerdIO act (Herd end tv) = act tv >> readTVarIO end >>= go
  where
    go (Herd t tv)
        | t == end  = return ()
        | otherwise = act tv >> readTVarIO t >>= go

forkIO_ x = do
    _ <- forkIO x
    return ()

initThreads :: (Ord a, Num a) => Herd a -> IO ()
initThreads h = do
    flip stepHerdIO h $ \t -> forkIO_ . forever . atomically $ do
            x <- readTVar t
            if x > 0
              then writeTVar t (x - 1)
              else retry


main = do
    h <- initHerd 0 1000
    stop <- newTVarIO False

    initThreads h

    t <- throughputMain 1000000 [process stop h]
    putStrLn ("Throughput: " ++ show t)



    
