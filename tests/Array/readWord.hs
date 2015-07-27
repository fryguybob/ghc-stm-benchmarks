{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
import GHC.Num
import GHC.ST
import GHC.Base
import GHC.Conc
import GHC.Word

data TStruct a = TStruct { unTStruct :: STMMutableArray# RealWorld a }

unsafeReadTStructWord :: TStruct a -> Int -> IO Word
unsafeReadTStructWord marr i@(I# i#) = IO $ \s# ->
    case readSTMArrayWord# (unTStruct marr) i# s# of
        (# s2#, w# #) -> (# s2#, W# w# #)

newTStructIO :: Int -> Int -> a -> IO (TStruct a)
newTStructIO (I# ptrs#) (I# words#) a = IO $ \s1# ->
    case newSTMArray# ptrs# words# a s1# of
              (# s2#, marr# #) -> (# s2#, TStruct marr# #)


main = do
    a <- newTStructIO 81 83 "blah"
    x <- unsafeReadTStructWord a 42
    print x

