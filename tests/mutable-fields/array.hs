{-# LANGUAGE MutableFields #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import GHC.Types
import GHC.Prim
import GHC.IO
import GHC.Int
import GHC.Conc

import Control.Monad

data M where
  MkM :: String -> [Int] -> mutableArray Int -> IO M


data UM = UM String [Int] Int# Int# Int# Int#
    deriving (Show)

type MutableArray a = RefArray# RealWorld a

readRefArray :: MutableArray a -> Int -> IO a
readRefArray r (I# i#)  = IO $ \s1# -> readRefArray# r i# s1#

writeRefArray :: MutableArray a -> Int -> a -> IO ()
writeRefArray r (I# i#) v = IO $ \s1# -> 
    case writeRefArray# r i# v s1# of
        s2# -> (# s2#, () #)

{-# NOINLINE debug #-}
debug :: M -> IO ()
debug m = do
    print "Debug:"
    let um :: UM
        !um@(UM _ _ _ x0# x1# x2#) = unsafeCoerce# m
    print um
 

main = do
    m <- MkM "hello" [1000] 12#
    let is = [0..11]
    case m of
        MkM s l r -> do
            print s
            print l
            debug m
            forM_ is $ \i -> do
                writeRefArray r i (i*2)
            debug m
            forM_ is $ \i -> do
                readRefArray r i >>= print
            forM_ is $ \i -> do
                writeRefArray r i (12-i)
            forM_ is $ \i -> do
                readRefArray r i >>= print
    print "x"
