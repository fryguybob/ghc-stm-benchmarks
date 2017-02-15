{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module SkipListNode
    ( Node(..)
    , Key
    , Value
    , writeKey
    , writeValue
    , unsafeWriteNodeP
    , unsafeWriteNode
    , unsafeReadNode
    , readKey
    , readKeyP
    , readValue
    , levels
    , mkNodeP
    , newNodeP
    , nil
    , isNil
    ) where

import GHC.Num
import GHC.ST
import GHC.Base
import GHC.Conc
import GHC.Word
import GHC.Prim

import System.IO.Unsafe (unsafePerformIO)

type Key = Word
type Value = Word

-- TODO: This unpack is ignored, but it might be enabled in the future:
-- https://ghc.haskell.org/trac/ghc/wiki/UnpackedSumTypes
data Node = Node {-# UNPACK #-} !(STMMutableArray# RealWorld Node) | Nil

{-# INLINE nil #-}
nil :: Node
nil = Nil

{-# INLINE isNil #-}
isNil :: Node -> Bool
isNil Nil = True
isNil _   = False

{-# INLINE unNode #-}
unNode (Node a#) = a#

newNodeIO :: Int -> IO Node
newNodeIO (I# ptrs#) = IO $ \s1# ->
    case newSTMArray# ptrs# 2# Nil s1# of
          (# s2#, marr# #) -> (# s2#, Node marr# #)
{-# INLINE newNodeIO #-}

newNodeP :: Int -> STM Node
newNodeP (I# ptrs#) = STM $ \s1# ->
    case newSTMArray# ptrs# 2# Nil s1# of
          (# s2#, marr# #) -> (# s2#, Node marr# #)
{-# INLINE newNodeP #-}

unsafeReadNode :: Node -> Int -> STM Node
unsafeReadNode marr (I# i#) = STM $ \s# ->
    case readTArray# (unNode marr) (int2Word# i#) s# of
      (# s2#, a #) -> (# s2#, a #)
{-# INLINE unsafeReadNode #-}

unsafeWriteNode :: Node -> Int -> Node -> STM ()
unsafeWriteNode marr (I# i#) a = STM $ \s# ->
    case writeTArray# (unNode marr) (int2Word# i#) a s# of
      s2# -> (# s2#, () #)
{-# INLINE unsafeWriteNode #-}

unsafeWriteNodeP :: Node -> Int -> Node -> STM ()
unsafeWriteNodeP marr (I# i#) a = STM $ \s# ->
    case writeSTMArray# (unNode marr) i# a s# of
      s2# -> (# s2#, () #)
{-# INLINE unsafeWriteNodeP #-}

unsafeReadNodeWord :: Node -> Int -> STM Word
unsafeReadNodeWord marr (I# i#) = STM $ \s# ->
    case readTArrayWord# (unNode marr) (int2Word# i#) s# of
        (# s2#, w# #) -> (# s2#, W# w# #)
{-# INLINE unsafeReadNodeWord #-}

unsafeReadNodeWordP :: Node -> Int -> STM Word
unsafeReadNodeWordP marr (I# i#) = STM $ \s# ->
    case readSTMArrayWord# (unNode marr) i# s# of
        (# s2#, w# #) -> (# s2#, W# w# #)
{-# INLINE unsafeReadNodeWordP #-}

unsafeWriteNodeWord :: Node -> Int -> Word -> STM ()
unsafeWriteNodeWord marr (I# i#) (W# w#) = STM $ \s# ->
    case writeTArrayWord# (unNode marr) (int2Word# i#) w# s# of
      s2# -> (# s2#, () #)
{-# INLINE unsafeWriteNodeWord #-}

levels :: Node -> Int
levels marr = I# (sizeofSTMMutableArray# (unNode  marr))
{-# INLINE levels #-}

instance Eq Node where
  (Node t) == (Node t') =
    case sameSTMMutableArray# t t' of
      0# -> False
      _  -> True
  Nil      == Nil       = True
  _        == _         = False

#define KEY   0
#define VALUE 1

writeKey :: Node -> Word -> STM ()
writeKey s x = unsafeWriteNodeWord s KEY x
{-# INLINE writeKey #-}

writeValue :: Node -> Word -> STM ()
writeValue s x = unsafeWriteNodeWord s VALUE x
{-# INLINE writeValue #-}

writeKeyP :: Node -> Key -> STM ()
writeKeyP marr (W# w#) = STM $ \s1# ->
    case writeSTMArrayWord# (unNode marr) KEY# w# s1# of
      s2# -> (# s2#, () #)
{-# INLINE writeKeyP #-}

writeValueP :: Node -> Value -> STM ()
writeValueP marr (W# w#) = STM $ \s1# ->
    case writeSTMArrayWord# (unNode marr) VALUE# w# s1# of
      s2# -> (# s2#, () #)
{-# INLINE writeValueP #-}

mkNodeP :: Key -> Value -> Int -> STM Node
mkNodeP k v levels = do
    s <- newNodeP levels
    writeKeyP s k
    writeValueP s v
    return s
{-# INLINE mkNodeP #-}

readKey :: Node -> STM Word
readKey s = unsafeReadNodeWord s KEY
{-# INLINE readKey #-}

readKeyP :: Node -> STM Word
readKeyP s = unsafeReadNodeWordP s KEY
{-# INLINE readKeyP #-}

readValue :: Node -> STM Word
readValue s = unsafeReadNodeWord s VALUE
{-# INLINE readValue #-}


