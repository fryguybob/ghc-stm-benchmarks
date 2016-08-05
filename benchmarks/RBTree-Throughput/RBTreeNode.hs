{-# LANGUAGE CPP #-}
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

-- data Node = Node {-# UNPACK #-} !(STMMutableArray# RealWorld Node) | Nil
data Node = Node { unNode :: !(STMMutableArray# RealWorld Any) }

{-# NOINLINE nil #-}
nil :: Node
nil = unsafePerformIO $ newNodeIO 0 0 (error "Nil followed!")

data Color = Red | Black
    deriving (Eq, Show, Read)

newNodeIO :: Int -> Int -> Node -> IO Node
newNodeIO (I# ptrs#) (I# words#) a = IO $ \s1# ->
    case newSTMArray# ptrs# words# (unsafeCoerce# (unNode a)) s1# of
          (# s2#, marr# #) -> (# s2#, Node marr# #)
{-# INLINE newNodeIO #-}

newNode :: Int -> Int -> Node -> STM Node
newNode (I# ptrs#) (I# words#) a = STM $ \s1# ->
    case newSTMArray# ptrs# words# (unsafeCoerce# (unNode a)) s1# of
          (# s2#, marr# #) -> (# s2#, Node marr# #)
{-# INLINE newNode #-}

unsafeReadNode :: Node -> Word -> STM Node
unsafeReadNode marr (W# w#) = STM $ \s# ->
    case readTArray# (unNode marr) w# s# of
      (# s2#, a #) -> (# s2#, Node (unsafeCoerce# a) #)
{-# INLINE unsafeReadNode #-}

unsafeWriteNode :: Node -> Word -> Node -> STM ()
unsafeWriteNode marr (W# w#) a = STM $ \s# ->
    case writeTArray# (unNode marr) w# (unsafeCoerce# (unNode a)) s# of
      s2# -> (# s2#, () #)
{-# INLINE unsafeWriteNode #-}

unsafeWriteNodeP :: Node -> Word -> Node -> STM ()
unsafeWriteNodeP marr (W# w#) a = STM $ \s# ->
    case writeSTMArray# (unNode marr) (word2Int# w#) (unsafeCoerce# (unNode a)) s# of
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

#define KEY   0
#define VALUE 1
#define COLOR 2

-- #define SEPARATE_POINTERS
#ifdef SEPARATE_POINTERS
#define PARENT 0
#define LEFT   8
#define RIGHT  16
#else
#define PARENT 0
#define LEFT   1
#define RIGHT  2
#endif

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
#ifdef SEPARATE_POINTERS
    s <- newNode 24 3 nil -- Minimal needed values
#else
    s <- newNode 3 3 nil -- Minimal needed values
--    s <- newNode 3 5 nil -- Padded end out to cacheline
--    s <- newNode 8 3 nil -- Ptrs and words on separate cachelines
--    s <- newNode 8 8 nil -- Ptrs and words on separate cachelines and padded
#endif
    -- We just made the node so it is private and we can access it 
    -- non-transactionally.
    writeKeyP s k
    writeValueP s v
    writeColorP s c
    return s
{-# INLINE mkNode #-}

key :: Node -> STM Word
key s 
  | s == nil  = return 0
  | otherwise = unsafeReadNodeWord s KEY
{-# INLINE key #-}

value :: Node -> STM Word
value s
  | s == nil  = return 0
  | otherwise = unsafeReadNodeWord s VALUE
{-# INLINE value #-}

color :: Node -> STM Color
color s
  | s == nil  = return Black
  | otherwise = do
    w <- unsafeReadNodeWord s COLOR
    case w == (0 :: Word) of
      True -> return Black
      _    -> return Red
{-# INLINE color #-}

parent :: Node -> STM Node
parent s
  | s == nil  = return nil
  | otherwise = unsafeReadNode s PARENT
{-# INLINE parent #-}

left :: Node -> STM Node
left s
  | s == nil  = return nil
  | otherwise = unsafeReadNode s LEFT
{-# INLINE left #-}

right :: Node -> STM Node
right s
  | s == nil  = return nil
  | otherwise = unsafeReadNode s RIGHT
{-# INLINE right #-}

