{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
module Main ( main ) where

import GHC.Exts
import GHC.Prim
import GHC.ST

main = putStr
       (test_sizeofSTMArrayPtrs
        ++ "\n" ++ test_sizeofSTMArrayWords
        ++ "\n"
       )

test_sizeofSTMArrayPtrs :: String
test_sizeofSTMArrayPtrs = flip shows "\n" $ runST $ ST $ \ s# -> go 0 [] s#
  where
    !(I# z#) = 0
    go i@(I# i#) acc s#
        | i < 1000 = case newSTMArray# i# z# 0 s# of
            (# s2#, marr# #) -> case sizeofSTMMutableArray# marr# of
                j# -> go (i+1) ((I# j#):acc) s2#
        | otherwise = (# s#, reverse acc #)

test_sizeofSTMArrayWords :: String
test_sizeofSTMArrayWords = flip shows "\n" $ runST $ ST $ \ s# -> go 0 [] s#
  where
    !(I# z#) = 0
    go i@(I# i#) acc s#
        | i < 1000 = case newSTMArray# z# i# 0 s# of
            (# s2#, marr# #) -> case sizeofSTMMutableArrayWords# marr# of
                j# -> go (i+1) ((I# j#):acc) s2#
        | otherwise = (# s#, reverse acc #)

