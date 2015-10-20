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
    , parent
    , left
    , right
    , key
    , value
    , color
    , mkNode
    ) where

import GHC.Num
import GHC.ST
import GHC.Base
import GHC.Conc
import GHC.Word

type Key = Word
type Value = Word

-- data Node = Node {-# UNPACK #-} !(STMMutableArray# RealWorld Node) | Nil
data Node = Node !(STMMutableArray# RealWorld Node) | Nil

data Color = Red | Black
    deriving (Eq, Show, Read)

unNode :: Node -> STMMutableArray# RealWorld Node
unNode (Node a) = a
unNode Nil      = error "Nil dereference"

newNode :: Int -> Int -> Node -> STM Node
newNode (I# ptrs#) (I# words#) a = STM $ \s1# ->
    case newSTMArray# ptrs# words# a s1# of
          (# s2#, marr# #) -> (# s2#, Node marr# #)
{-# INLINE newNode #-}

unsafeReadNode :: Node -> Word -> STM Node
unsafeReadNode marr (W# w#) = STM $ \s# -> readTArray# (unNode marr) w# s#
{-# INLINE unsafeReadNode #-}

unsafeWriteNode :: Node -> Word -> Node -> STM ()
unsafeWriteNode marr (W# w#) a = STM $ \s# ->
    case writeTArray# (unNode marr) w# a s# of
      s2# -> (# s2#, () #)
{-# INLINE unsafeWriteNode #-}

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

lengthNode :: Node -> Int
lengthNode marr = I# (sizeofSTMMutableArray# (unNode marr))
{-# INLINE lengthNode #-}

lengthNodeWords :: Node -> Int
lengthNodeWords marr = I# (sizeofSTMMutableArrayWords# (unNode marr))
{-# INLINE lengthNodeWords #-}

instance Eq Node where
  Nil == Nil = True
  (Node t) == (Node t') =
    case sameSTMMutableArray# t t' of
      0# -> False
      _  -> True
  _ == _ = False

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

writeValue :: Node -> Word -> STM ()
writeValue s x = unsafeWriteNodeWord s VALUE x
{-# INLINE writeValue #-}

writeColor :: Node -> Color -> STM ()
writeColor s Black = unsafeWriteNodeWord s COLOR 0
writeColor s Red   = unsafeWriteNodeWord s COLOR 1
{-# INLINE writeColor #-}

writeParent :: Node -> Node -> STM ()
writeParent s x = unsafeWriteNode s PARENT x
{-# INLINE writeParent #-}

writeLeft :: Node -> Node -> STM ()
writeLeft s x = unsafeWriteNode s LEFT x
{-# INLINE writeLeft #-}

writeRight :: Node -> Node -> STM ()
writeRight s x = unsafeWriteNode s RIGHT x
{-# INLINE writeRight #-}

mkNode :: Key -> Value -> Color -> STM Node
mkNode k v c = do
#ifdef SEPARATE_POINTERS
    s <- newNode 24 3 Nil -- Minimal needed values
#else
    s <- newNode 3 3 Nil -- Minimal needed values
--    s <- newNode 3 5 Nil -- Padded end out to cacheline
--    s <- newNode 8 3 Nil -- Ptrs and words on separate cachelines
--    s <- newNode 8 8 Nil -- Ptrs and words on separate cachelines and padded
#endif
    writeKey s k
    writeValue s v
    writeColor s c
    return s
{-# INLINE mkNode #-}

key :: Node -> STM Word
key Nil = return 0
key s   = unsafeReadNodeWord s KEY
{-# INLINE key #-}

value :: Node -> STM Word
value Nil = return 0
value s   = unsafeReadNodeWord s VALUE
{-# INLINE value #-}

color :: Node -> STM Color
color Nil = return Black
color s   = do
    w <- unsafeReadNodeWord s COLOR
    case w == (0 :: Word) of
      True -> return Black
      _    -> return Red
{-# INLINE color #-}

parent :: Node -> STM Node
parent Nil = return Nil
parent s   = unsafeReadNode s PARENT
{-# INLINE parent #-}

left :: Node -> STM Node
left Nil = return Nil
left s   = unsafeReadNode s LEFT
{-# INLINE left #-}

right :: Node -> STM Node
right Nil = return Nil
right s   = unsafeReadNode s RIGHT
{-# INLINE right #-}

