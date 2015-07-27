{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
import GHC.Num
import GHC.ST
import GHC.Base

data STMArray s a = STMArray { unSTMArray :: STMMutableArray# s a }

newSTMArrayST :: Int -> Int -> a -> ST s (STMArray s a)
newSTMArrayST (I# ptrs#) (I# words#) a = ST $ \s1# ->
    case newSTMArray# ptrs# words# a s1# of
      (# s2#, marr# #) -> (# s2#, STMArray marr# #)

lengthSTMArray :: STMArray s a -> Int
lengthSTMArray marr = I# (sizeofSTMMutableArray# (unSTMArray marr))

lengthSTMArrayWords :: STMArray s a -> Int
lengthSTMArrayWords marr = I# (sizeofSTMMutableArrayWords# (unSTMArray marr))

unsafeReadSTMArrayST :: STMArray s a -> Int -> ST s a
unsafeReadSTMArrayST marr i@(I# i#) = ST $ \s# -> readSTMArray# (unSTMArray marr) i# s#

unsafeWriteSTMArrayST :: STMArray s a -> Int -> a -> ST s ()
unsafeWriteSTMArrayST marr (I# i#) a = ST $ \s# ->
    case writeSTMArray# (unSTMArray marr) i# a s# of
      s2# -> (# s2#, () #)

test1 = runST $ do
    a <- newSTMArrayST 3 4 'a'

    r <- unsafeReadSTMArrayST a 1

    unsafeWriteSTMArrayST a 1 'b'

    rw <- unsafeReadSTMArrayST a 1

    return (lengthSTMArray a, lengthSTMArrayWords a, r, rw)

main = print test1
