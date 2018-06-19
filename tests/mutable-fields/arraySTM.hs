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
  MkM :: String -> [Int] -> mutableArray Int -> STM M

type MutableArray a = RefArray# RealWorld a

readRefArray :: MutableArray a -> Int -> STM a
readRefArray r (I# i#)  = STM $ \s1# -> readTRefArray# r i# s1#

writeRefArray :: MutableArray a -> Int -> a -> STM ()
writeRefArray r (I# i#) v = STM $ \s1# -> 
    case writeTRefArray# r i# v s1# of
        s2# -> (# s2#, () #)

main = do
    m <- atomically $ MkM "hello" [1000] 12#
    let is = [0..11]
    case m of
        MkM s l r -> do
            print s
            print l
            atomically $ forM_ is $ \i -> do
                writeRefArray r i (i*2)
            vs <- atomically $ forM is $ \i -> do
                readRefArray r i
            print vs
            atomically $ forM_ is $ \i -> do
                writeRefArray r i (12-i)
            vs <- atomically $ forM is $ \i -> do
                readRefArray r i
            print vs
    print "x"
