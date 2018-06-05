{-# LANGUAGE MutableFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import GHC.Types
import GHC.Prim
import GHC.IO
import GHC.Int
import GHC.Conc

data M where
  MkM :: mutable Int# -> Int -> IO M

type MutableInt = RefU# RealWorld Int#

readRefInt :: MutableInt -> IO Int
readRefInt r = IO $ \s1# -> 
   case readRefInt# r s1# of
       (# s2#, i# #) -> (# s2#, I# i# #)

writeRefInt :: MutableInt -> Int -> IO ()
writeRefInt r (I# i#) = IO $ \s1# ->
    case writeRefInt# r i# s1# of
        s2# -> (# s2#, () #)

main = do
    m <- MkM 100000# 0x7654321
    case m of
        MkM r b -> do
            print b
            readRefInt r >>= print
            writeRefInt r 42
            readRefInt r >>= print
