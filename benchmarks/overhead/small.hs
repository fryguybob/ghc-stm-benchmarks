{-# LANGUAGE CPP #-}
import Control.Concurrent.STM
import Control.Monad
import Control.Applicative
import System.IO
import Data.IORef
import System.Environment

runSTM n = do
    a <- newTVarIO 1
    b <- newTVarIO 0

    replicateM_ n (atomically $ t a b)

    readTVarIO b
  where
    t a b = do
        x <- readTVar a
        y <- readTVar b
        case x+y of
            0 -> return ()
            v -> writeTVar b v

runIO n = do
    a <- newIORef 1
    b <- newIORef 0

    replicateM_ n (t a b)

    readIORef b
  where
    t a b = do
        x <- readIORef a
        y <- readIORef b
        case x+y of
            0 -> return ()
            v -> writeIORef b v

main = do
    [n] <- getArgs

#ifdef RUN_STM
    x <- runSTM (read n)
#elif RUN_IO
    x <- runIO (read n)
#else
    x <- return (read n :: Int)
#endif

    print x
