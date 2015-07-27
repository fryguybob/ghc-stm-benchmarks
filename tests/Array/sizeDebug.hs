{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Main ( main ) where

import GHC.Exts
import GHC.Prim
import GHC.ST

foreign import prim "stg_doSTMSizezh" doSTMSize# :: STMMutableArray# d a -> Int#

sizeSTM# :: STMMutableArray# d a -> Int#
-- sizeSTM# marr# = sizeofSTMMutableArray# marr#
sizeSTM# marr# = sizeofSTMMutableArrayWords# marr#
{-# NOINLINE sizeSTM# #-}

sizeSmall# :: SmallMutableArray# d a -> Int#
sizeSmall# marr# = sizeofSmallMutableArray# marr#
{-# NOINLINE sizeSmall# #-}

main = putStr
       (test_sizeofSTMArrayPtrs
        ++ "\n" ++ test_sizeofMutableArray
        ++ "\n"
       )

test_sizeofMutableArray :: String
test_sizeofMutableArray = flip shows "\n" $ runST $ ST $ \ s# -> go 0 [] s#
  where
    go i@(I# i#) acc s#
        | i < 1000 = case newSmallArray# i# 0 s# of
            (# s2#, marr# #) -> case sizeSmall# marr# of
                    j# -> go (i+1) ((I# j#):acc) s2#
        | otherwise = (# s#, reverse acc #)

test_sizeofSTMArrayPtrs :: String
test_sizeofSTMArrayPtrs = flip shows "\n" $ runST $ ST $ \ s# -> go 0 [] s#
  where
    !(I# ptrs#)  = 11
    !(I# words#) = 13
    go i@(I# i#) acc s#
        | i < 1 = case newSTMArray# ptrs# words# 0 s# of
--            (# s2#, marr# #) -> case sizeofSTMMutableArray# marr# of
--            (# s2#, marr# #) -> case doSTMSize# marr# of
            (# s2#, marr# #) -> case sizeSTM# marr# of
                j# -> go (i+1) ((I# j#):acc) s2#
        | otherwise = (# s#, reverse acc #)

