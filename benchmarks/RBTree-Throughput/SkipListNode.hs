{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
module SkipListNode
    ( Node
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
    , newNodeIO
    , nil
    , isNil
    , getNode
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

#define KEY       0
#define VALUE     1
#define KEYZH     0#
#define VALUEZH   1#
#define VALUEZHZH 1##

-- TODO: This unpack is ignored, but it might be enabled in the future:
-- https://ghc.haskell.org/trac/ghc/wiki/UnpackedSumTypes
-- data Node = Node {-# UNPACK #-} !(STMMutableArray# RealWorld Node) | Nil

data Node = Node { unNode :: !(STMMutableArray# RealWorld Any) }

{-# NOINLINE nil #-}
nil :: Node
nil = unsafePerformIO $ rawNil
  where
    rawNil = IO $ \s1# ->
      case newSTMArray# 0# 2# undefined s1# of
        (# s2#, marr# #) -> (# s2#, Node marr# #)

{-# INLINE isNil #-}
isNil :: Node -> Bool
isNil (Node marr#) = case sameSTMMutableArray# marr# (unNode nil) of
                        0# -> False
                        _  -> True

getNode :: Node -> Int -> Key -> STM Node
getNode !(Node !nodes#) !(I# height#) !(W# k#) = STM $ \s1# -> loop height# nodes# s1#
  where
   !(Node !nil#) = nil

   loop :: Int# -> STMMutableArray# RealWorld Any -> State# RealWorld
        -> (# State# RealWorld, Node #)
   loop 0#    _       s1# = (# s1#, nil #)
   loop !lvl# !nodes# s1# = 
      let !l# = lvl# -# 1# in
      case readTArray# nodes# (int2Word# l#) s1# of
          (# s2#, n #) ->
            case sameSTMMutableArray# (unsafeCoerce# n) nil# of
              1# -> loop l# nodes# s2#
              _  ->
                case readSTMArrayWord# (unsafeCoerce# n) KEYZH s2# of -- Non-transactional
                  (# s3#, k'# #) ->
                    case k'# `gtWord#` k# of
                      1# -> loop l# nodes# s3#
                      _  ->
                        case k'# `ltWord#` k# of
                          1# -> loop lvl# (unsafeCoerce# n) s3#
                          _  -> (# s3#, Node (unsafeCoerce# n) #)

--  case readTArrayWord# (unsafeCoerce# n) VALUEZHZH s3# of
--     (# s4#, v# #) -> (# s4#, Just (W# v#) #)

newNodeIO :: Int -> IO Node
newNodeIO (I# ptrs#) = IO $ \s1# ->
    case unNode nil of
      n# -> case newSTMArray# ptrs# 2# (unsafeCoerce# n#) s1# of
              (# s2#, marr# #) -> (# s2#, Node marr# #)
{-# INLINE newNodeIO #-}

newNodeP :: Int -> STM Node
newNodeP (I# ptrs#) = STM $ \s1# ->
    case unNode nil of
      n# -> case newSTMArray# ptrs# 2# (unsafeCoerce# n#) s1# of
              (# s2#, marr# #) -> (# s2#, Node marr# #)
{-# INLINE newNodeP #-}

unsafeReadNode :: Node -> Int -> STM Node
unsafeReadNode marr (I# i#) = STM $ \s# ->
    case readTArray# (unNode marr) (int2Word# i#) s# of
      (# s2#, a #) -> (# s2#, Node (unsafeCoerce# a) #)
{-# INLINE unsafeReadNode #-}

unsafeWriteNode :: Node -> Int -> Node -> STM ()
unsafeWriteNode marr (I# i#) !(Node !a#) = STM $ \s# ->
    case writeTArray# (unNode marr) (int2Word# i#) (unsafeCoerce# a#) s# of
      s2# -> (# s2#, () #)
{-# INLINE unsafeWriteNode #-}

unsafeWriteNodeP :: Node -> Int -> Node -> STM ()
unsafeWriteNodeP marr (I# i#) !(Node !a#) = STM $ \s# ->
    case writeSTMArray# (unNode marr) i# (unsafeCoerce# a#) s# of
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


