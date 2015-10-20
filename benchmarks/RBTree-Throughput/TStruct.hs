{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module TStruct
    ( newTStructIO

    , newTStruct
    , unsafeReadTStruct
    , unsafeReadTStructWord
    , unsafeWriteTStruct
    , unsafeWriteTStructWord

    , readTStruct
    , readTStructWord
    , writeTStruct
    , writeTStructWord

    , lengthTStruct
    , lengthTStructWords

    , TStruct

    ) where

import GHC.Num
import GHC.ST
import GHC.Base
import GHC.Conc
import GHC.Word

data TStruct a = TStruct { unTStruct :: STMMutableArray# RealWorld a }

newTStructIO :: Int -> Int -> a -> IO (TStruct a)
newTStructIO (I# ptrs#) (I# words#) a = IO $ \s1# ->
    case newSTMArray# ptrs# words# a s1# of
          (# s2#, marr# #) -> (# s2#, TStruct marr# #)

newTStruct :: Int -> Int -> a -> STM (TStruct a)
newTStruct (I# ptrs#) (I# words#) a = STM $ \s1# ->
    case newSTMArray# ptrs# words# a s1# of
          (# s2#, marr# #) -> (# s2#, TStruct marr# #)

-- Unsafe variations do not perform a bounds check
unsafeReadTStruct :: TStruct a -> Word -> STM a
unsafeReadTStruct marr (W# w#) = STM $ \s# -> readTArray# (unTStruct marr) w# s#

unsafeWriteTStruct :: TStruct a -> Word -> a -> STM ()
unsafeWriteTStruct marr (W# w#) a = STM $ \s# ->
    case writeTArray# (unTStruct marr) w# a s# of
      s2# -> (# s2#, () #)

unsafeReadTStructWord :: TStruct a -> Word -> STM Word
unsafeReadTStructWord marr (W# wi#) = STM $ \s# ->
    case readTArrayWord# (unTStruct marr) wi# s# of
        (# s2#, w# #) -> (# s2#, W# w# #)

unsafeWriteTStructWord :: TStruct a -> Word -> Word -> STM ()
unsafeWriteTStructWord marr (W# wi#) (W# w#) = STM $ \s# ->
    case writeTArrayWord# (unTStruct marr) wi# w# s# of
      s2# -> (# s2#, () #)

-- Safe variations perform a bounds check.
readTStruct :: TStruct a -> Word -> STM a
readTStruct marr (W# w#) = STM $ \s# -> 
    if isTrue# (w# `geWord#` int2Word# (sizeofSTMMutableArray# (unTStruct marr)))
      then error "readTStruct out of bounds."
      else readTArray# (unTStruct marr) w# s#

writeTStruct :: TStruct a -> Word -> a -> STM ()
writeTStruct marr (W# w#) a = STM $ \s# ->
    if isTrue# (w# `geWord#` int2Word# (sizeofSTMMutableArray# (unTStruct marr)))
      then error "writeTStruct out of bounds."
      else case writeTArray# (unTStruct marr) w# a s# of
                s2# -> (# s2#, () #)

readTStructWord :: TStruct a -> Word -> STM Word
readTStructWord marr (W# wi#) = STM $ \s# ->
    if isTrue# (wi# `geWord#` int2Word# (sizeofSTMMutableArrayWords# (unTStruct marr)))
      then error "readTStructWord out of bounds."
      else case readTArrayWord# (unTStruct marr) wi# s# of
                (# s2#, w# #) -> (# s2#, W# w# #)

writeTStructWord :: TStruct a -> Word -> Word -> STM ()
writeTStructWord marr (W# wi#) (W# w#) = STM $ \s# ->
    if isTrue# (wi# `geWord#` int2Word# (sizeofSTMMutableArrayWords# (unTStruct marr)))
      then error "writeTStructWord out of bounds."
      else case writeTArrayWord# (unTStruct marr) wi# w# s# of
                s2# -> (# s2#, () #)


lengthTStruct :: TStruct a -> Int
lengthTStruct marr = I# (sizeofSTMMutableArray# (unTStruct marr))

lengthTStructWords :: TStruct a -> Int
lengthTStructWords marr = I# (sizeofSTMMutableArrayWords# (unTStruct marr))

instance Eq (TStruct a) where
  (TStruct t) == (TStruct t') =
    case sameSTMMutableArray# t t' of
      0# -> False
      _  -> True


