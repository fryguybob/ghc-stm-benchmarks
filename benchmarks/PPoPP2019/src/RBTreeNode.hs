{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module RBTreeNode
    ( Node(..)
    , Key
    , Value
    , Color(..)
    , writeKey
    , writeColor
    , writeValue
    , writeParent
    , writeLeft
    , writeRight

    , writeKeyP
    , writeColorP
    , writeValueP
    , writeParentP
    , writeLeftP
    , writeRightP

    , parent
    , left
    , right
    , key
    , value
    , color
    , mkNode
    , nil

    , lookupNode'
    ) where

import GHC.Num
import GHC.ST
import GHC.Base
import GHC.Conc
import GHC.Word
import GHC.Prim

import System.IO.Unsafe (unsafePerformIO)

#define KEY   0
#define VALUE 1
#define COLOR 2

-- #define SEPARATE_POINTERS
#ifdef SEPARATE_POINTERS
#define PARENT   0
#define LEFT     8
#define RIGHT   16
#define PTRS    24
#define WORDS    3
#define PTRSZH  24#
#define WORDSZH  3#
#else
#define PARENT  0
#define LEFT    1
#define RIGHT   2
#define PTRS    3
#define WORDS   3
#define PTRSZH  3#
#define WORDSZH 3#
#endif

type Key = Word
type Value = Word

-- data Node = Node {-# UNPACK #-} !(STMMutableArray# RealWorld Node) | Nil
data Node = Node { unNode :: !(STMMutableArray# RealWorld Any) }

{-# NOINLINE nil' #-}
nil' :: Node
nil' = unsafePerformIO $ do
    !n <- rawNil
    writePIO n LEFT   n
    writePIO n RIGHT  n
    writePIO n PARENT n
    return n
  where
    writePIO marr (W# w#) (Node !a) = IO $ \s# ->
        case writeSTMArray# (unNode marr) (word2Int# w#) (unsafeCoerce# a) s# of
              s2# -> (# s2#, () #)

    rawNil = IO $ \s1# ->
      case newSTMArray# PTRSZH WORDSZH undefined s1# of
        (# s2#, marr# #) -> (# s2#, Node marr# #)

{-# NOINLINE nil #-}
nil :: Node
nil = unsafePerformIO $ rawNil
  where
    rawNil = IO $ \s1# ->
      case newSTMArray# PTRSZH WORDSZH undefined s1# of
        (# s2#, marr# #) ->
          case writeSTMArray# marr# 0# (unsafeCoerce# marr#) s2# of
            s3# -> 
              case writeSTMArray# marr# 1# (unsafeCoerce# marr#) s3# of
                s4# -> 
                  case writeSTMArray# marr# 2# (unsafeCoerce# marr#) s4# of
                    s5# -> (# s5#, Node marr# #)

lookupNode' :: Key -> Node -> STM Node
lookupNode' (W# k#) !(Node !a#) = STM $ \s# -> go a# s#
  where
    !(Node !nil#) = nil

    go a# s1# =
      case sameSTMMutableArray# a# nil# of
        1# -> (# s1#, nil #)
        _  -> case readTArrayWord# a# 0## s1# of
                (# s2#, w# #) -> 
                  case k# `eqWord#` w# of
                    1# -> (# s2#, Node a# #)
                    _  -> case k# `ltWord#` w# of
                            1# -> case readTArray# a# 1## s2# of
                                    (# s3#, a #) -> go (unsafeCoerce# a) s3#
                            _  -> case readTArray# a# 2## s2# of
                                    (# s3#, a #) -> go (unsafeCoerce# a) s3#


data Color = Red | Black
    deriving (Eq, Show, Read)

newNode :: Int -> Int -> Node -> STM Node
newNode (I# ptrs#) (I# words#) (Node !a) = STM $ \s1# ->
    case newSTMArray# ptrs# words# (unsafeCoerce# a) s1# of
          (# s2#, marr# #) -> (# s2#, Node marr# #)
{-# INLINE newNode #-}

unsafeReadNode :: Node -> Word -> STM Node
unsafeReadNode marr (W# w#) = STM $ \s# ->
    case readTArray# (unNode marr) w# s# of
      (# s2#, a #) -> (# s2#, Node (unsafeCoerce# a) #)
{-# INLINE unsafeReadNode #-}

unsafeWriteNode :: Node -> Word -> Node -> STM ()
unsafeWriteNode marr (W# w#) (Node !a) = STM $ \s# ->
    case writeTArray# (unNode marr) w# (unsafeCoerce# a) s# of
      s2# -> (# s2#, () #)
{-# INLINE unsafeWriteNode #-}

unsafeWriteNodeP :: Node -> Word -> Node -> STM ()
unsafeWriteNodeP marr (W# w#) (Node !a) = STM $ \s# ->
    case writeSTMArray# (unNode marr) (word2Int# w#) (unsafeCoerce# a) s# of
      s2# -> (# s2#, () #)
{-# INLINE unsafeWriteNodeP #-}

unsafeReadNodeWord :: Node -> Word -> STM Word
unsafeReadNodeWord marr (W# wi#) = STM $ \s# ->
    case readTArrayWord# (unNode marr) wi# s# of
        (# s2#, w# #) -> (# s2#, W# w# #)
{-# INLINE unsafeReadNodeWord #-}

unsafeWriteNodeWord :: Node -> Word -> Word -> STM ()
unsafeWriteNodeWord marr (W# wi#) (W# w#) = STM $ \s# ->
    case writeTArrayWord# (unNode marr) wi# w# s# of
      s2# -> (# s2#, () #)
{-# INLINE unsafeWriteNodeWord #-}

unsafeWriteNodeWordP :: Node -> Word -> Word -> STM ()
unsafeWriteNodeWordP marr (W# wi#) (W# w#) = STM $ \s# ->
    case writeSTMArrayWord# (unNode marr) (word2Int# wi#) w# s# of
      s2# -> (# s2#, () #)
{-# INLINE unsafeWriteNodeWordP #-}

lengthNode :: Node -> Int
lengthNode marr = I# (sizeofSTMMutableArray# (unNode marr))
{-# INLINE lengthNode #-}

lengthNodeWords :: Node -> Int
lengthNodeWords marr = I# (sizeofSTMMutableArrayWords# (unNode marr))
{-# INLINE lengthNodeWords #-}

instance Eq Node where
  (Node t) == (Node t') =
    case sameSTMMutableArray# t t' of
      0# -> False
      _  -> True


writeKey :: Node -> Word -> STM ()
writeKey s x = unsafeWriteNodeWord s KEY x
{-# INLINE writeKey #-}

writeKeyP :: Node -> Word -> STM ()
writeKeyP s x = unsafeWriteNodeWordP s KEY x
{-# INLINE writeKeyP #-}

writeValue :: Node -> Word -> STM ()
writeValue s x = unsafeWriteNodeWord s VALUE x
{-# INLINE writeValue #-}

writeValueP :: Node -> Word -> STM ()
writeValueP s x = unsafeWriteNodeWordP s VALUE x
{-# INLINE writeValueP #-}

writeColor :: Node -> Color -> STM ()
writeColor s Black = unsafeWriteNodeWord s COLOR 0
writeColor s Red   = unsafeWriteNodeWord s COLOR 1
{-# INLINE writeColor #-}

writeColorP :: Node -> Color -> STM ()
writeColorP s Black = unsafeWriteNodeWordP s COLOR 0
writeColorP s Red   = unsafeWriteNodeWordP s COLOR 1
{-# INLINE writeColorP #-}

writeParent :: Node -> Node -> STM ()
writeParent s x = unsafeWriteNode s PARENT x
{-# INLINE writeParent #-}

writeLeft :: Node -> Node -> STM ()
writeLeft s x = unsafeWriteNode s LEFT x
{-# INLINE writeLeft #-}

writeRight :: Node -> Node -> STM ()
writeRight s x = unsafeWriteNode s RIGHT x
{-# INLINE writeRight #-}

writeParentP :: Node -> Node -> STM ()
writeParentP s x = unsafeWriteNodeP s PARENT x
{-# INLINE writeParentP #-}

writeLeftP :: Node -> Node -> STM ()
writeLeftP s x = unsafeWriteNodeP s LEFT x
{-# INLINE writeLeftP #-}

writeRightP :: Node -> Node -> STM ()
writeRightP s x = unsafeWriteNodeP s RIGHT x
{-# INLINE writeRightP #-}

mkNode :: Key -> Value -> Color -> STM Node
mkNode k v c = do
    s <- newNode PTRS WORDS nil
    -- We just made the node so it is private and we can access it 
    -- non-transactionally.
    writeKeyP s k
    writeValueP s v
    writeColorP s c
    return s
{-# INLINE mkNode #-}

key :: Node -> STM Key
key s = unsafeReadNodeWord s KEY
{- For non-monomorphic keys we need to do something else for nil (which
 - shouldn't be compaired anyway!  So this should just go away...
key s 
  | s == nil  = return 0
  | otherwise = unsafeReadNodeWord s KEY
-}
{-# INLINE key #-}

value :: Node -> STM Value
value s = unsafeReadNodeWord s VALUE
{- See key
value s
  | s == nil  = return 0
  | otherwise = unsafeReadNodeWord s VALUE
-}
{-# INLINE value #-}

color :: Node -> STM Color
color s = do
    w <- unsafeReadNodeWord s COLOR
    case w == (0 :: Word) of
      True -> return Black
      _    -> return Red
{-# INLINE color #-}

parent :: Node -> STM Node
parent s = unsafeReadNode s PARENT
{-# INLINE parent #-}

left :: Node -> STM Node
left s = unsafeReadNode s LEFT
{-# INLINE left #-}

right :: Node -> STM Node
right s = unsafeReadNode s RIGHT
{-# INLINE right #-}

