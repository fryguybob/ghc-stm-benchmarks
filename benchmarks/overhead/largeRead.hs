{-# LANGUAGE CPP #-}
import Control.Concurrent.STM
import Control.Monad
import Control.Applicative
import System.IO
import Data.IORef
import System.Environment

runSTM n m = do
    as <- replicateM m (newTVarIO 1)
    b  <- newTVarIO 0

    replicateM_ n (atomically $ t as b)

    readTVarIO b
  where
    t as b = do
        xs <- mapM readTVar as
        y  <- readTVar b
        case sum xs + y of
            0 -> return ()
            v -> writeTVar b v

runIO n m = do
    as <- replicateM m (newIORef 1)
    b  <- newIORef 0

    replicateM_ n (t as b)

    readIORef b
  where
    t a b = do
        xs <- mapM readIORef a
        y <- readIORef b
        case sum xs + y of
            0 -> return ()
            v -> writeIORef b v

main = do
    [n,m] <- getArgs

#ifdef RUN_STM
    x <- runSTM (read n) (read m)
#elif RUN_IO
    x <- runIO (read n) (read m)
#else
    x <- return (read n * read m :: Int)
#endif

    print x
