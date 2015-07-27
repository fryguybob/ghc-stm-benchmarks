import TStruct
import GHC.Conc

main = do
    r <- atomically $ do
        a <- newTStruct 1 1 "hello"
        x <- unsafeReadTStruct a 0
        unsafeWriteTStructWord a 0 42
        y <- unsafeReadTStructWord a 0
        return (x, y)
    print r
