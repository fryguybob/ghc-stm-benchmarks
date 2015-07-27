import TStruct
import System.Mem
import GHC.Conc
import Control.Monad
{-
main' = do
    a <- newTStructIO 1 1 "hello"
    performGC
    print (lengthTStruct a)
    print (lengthTStructWords a)
-}

{-
main = do
    let n = 3

    a <- newTStructIO n n "hello"

    forM_ [0..n-1] $ \i -> do
        x <- atomically $ unsafeReadTStructWord a i
        y <- atomically $ unsafeReadTStruct a i
        print (x,y)

    atomically $ forM_ [0..n-1] $ \i -> do
        unsafeWriteTStructWord a i (fromIntegral i)
        unsafeWriteTStruct a i (show i)

    forM_ [0..n-1] $ \i -> do
        x <- atomically $ unsafeReadTStructWord a i
        y <- atomically $ unsafeReadTStruct a i
        print (x,y)

-}
{-

main = do
    a <- newTStructIO 1 1 "hello"
    atomically $ unsafeWriteTStruct a 0 "world"

    x <- atomically $ unsafeReadTStruct a 0
    print x
-}

data Node = Node (TStruct Node) | Nil

test k x y = do
    a <- atomically $ newTStruct 3 3 Nil
    putStrLn "Writing"
    atomically $ unsafeWriteTStructWord a 0 k
    putStrLn "Written"
    return (Node a)

main = do
    forM_ [0..100] $ \_ -> do
        a <- test 1 1 0
        return ()
    putStrLn "done"
    performGC
    return ()
