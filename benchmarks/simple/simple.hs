{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Control.Applicative

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async

outer :: Int
outer = 1000
-- outer = 1

runA :: [TVar Int] -> [TVar Int] -> IO ()
runA xs ys = case length zs of 
  _ -> forM_ zs $ \((x0,x1),y) -> atomically $ do
    a <- readTVar x0
    b <- readTVar x1
    modifyTVar y (+(a+b))
    writeTVar x0 0
    writeTVar x1 0
  where
    xs' = zip <*> tail $ cycle xs
    ys' = cycle ys

    zs = take (outer * length xs) $ zip xs' ys'

runB :: [TVar Int] -> [TVar Int] -> IO ()
runB xs ys = case length zs of 
  _ -> forM_ zs $ \((x0,x1),y) -> atomically $ do
    a <- readTVar x0
    b <- readTVar x1
    modifyTVar y (+(a+b))
    writeTVar x0 0
    writeTVar x1 0
  where
    xs' = zip <*> tail $ cycle $ reverse xs
    ys' = cycle $ reverse ys

    zs = take (outer * length xs) $ zip xs' ys'

main :: IO ()
main = do
    xs <- replicateM 4096 (newTVarIO 1)
    ys <- replicateM 4096 (newTVarIO 0)

    a <- async (runA xs ys)
    b <- async (runB ys xs)

    wait a
    wait b

    s <- sum <$> forM (xs ++ ys) readTVarIO
    putStrLn ("Total: " ++ show s)
