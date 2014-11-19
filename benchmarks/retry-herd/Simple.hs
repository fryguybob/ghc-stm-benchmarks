module Simple where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Control.Applicative

-- n = 1000 :: Int
n = 1 :: Int

main = do
    xs <- forM [1..n] $ \i -> newTVarIO 0

    forkIO $ forM_ xs $ \x -> atomically $ do 
        v <- readTVar x
        if v == 0
          then retry
          else return ()
    
    forkIO $ forM_ xs $ \x -> atomically $ writeTVar x 1

    print =<< (atomically $ do
        s <- sum <$> forM xs readTVar
        if s /= n
          then retry
          else return s)

    putStrLn "Done."
