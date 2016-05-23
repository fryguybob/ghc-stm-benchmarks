import TStruct
import GHC.Conc

main'' = do
    r <- atomically $ do
        a <- newTStruct 1 0 "hello"
        unsafeReadTStruct a 0
    print r

main = do
    r <- atomically $ do
        a <- newTStruct 0 2 "hello"
        x <- unsafeReadTStructWord a 0
        y <- unsafeReadTStructWord a 1
        return (x,y)
    print r


main' = do
    r <- atomically $ do
        a <- newTStruct 1 1 "hello"
        x <- unsafeReadTStruct a 0
        unsafeWriteTStructWord a 0 42
        y <- unsafeReadTStructWord a 0
        return (x, y)
    print r
