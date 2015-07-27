{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import System.Mem
import GHC.Conc
import Control.Monad

import GHC.Num
import GHC.ST
import GHC.Base
import GHC.Word

data MArray a = MArray { unMArray :: SmallMutableArray# RealWorld a }

new :: Int -> a -> IO (MArray a)
new (I# n#) a = IO $ \s1# ->
    case newSmallArray# n# a s1# of
        (# s2#, marr# #) -> (# s2#, MArray marr# #)

write :: MArray a -> Int -> a -> IO ()
write marr i@(I# i#) a = IO $ \s1# ->
    case writeSmallArray# (unMArray marr) i# a s1# of
        s2# -> (# s2#, () #)

data Node = Node (MArray Node) | Nil

test k x y = do
    a <- atomically $ unsafeIOToSTM $ new 3 Nil
    putStrLn "Writing"
    atomically $ unsafeIOToSTM $ write a 0 k
    putStrLn "Written"
    return (Node a)

main = do
    forM_ [0..10000] $ \_ -> do
        a <- test Nil 1 0
        return ()
    putStrLn "done"
    performGC
    return ()
