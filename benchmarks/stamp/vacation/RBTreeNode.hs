{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module RBTreeNode
    ( Node(..)
    , Key
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

import Unsafe.Coerce

type Key = Word

data Node a = Node !(STMMutableArray# RealWorld (Node a)) | Nil

data Color = Red | Black
    deriving (Eq, Show, Read)

unNode :: Node a -> STMMutableArray# RealWorld (Node a)
unNode (Node a) = a
unNode Nil      = error "Nil dereference"

newNode :: Int -> Int -> Node a -> STM (Node a)
newNode (I# ptrs#) (I# words#) a = STM $ \s1# ->
    case newSTMArray# ptrs# words# a s1# of
          (# s2#, marr# #) -> (# s2#, Node marr# #)
{-# INLINE newNode #-}

unsafeReadNode :: Node a -> Word -> STM (Node a)
unsafeReadNode marr (W# w#) = STM $ \s# -> readTArray# (unNode marr) w# s#
{-# INLINE unsafeReadNode #-}

unsafeWriteNode :: Node a -> Word -> Node a -> STM ()
unsafeWriteNode marr (W# w#) a = STM $ \s# ->
    case writeTArray# (unNode marr) w# a s# of
      s2# -> (# s2#, () #)
{-# INLINE unsafeWriteNode #-}

unsafeReadNodeWord :: Node a -> Word -> STM Word
unsafeReadNodeWord marr (W# wi#) = STM $ \s# ->
    case readTArrayWord# (unNode marr) wi# s# of
        (# s2#, w# #) -> (# s2#, W# w# #)
{-# INLINE unsafeReadNodeWord #-}

unsafeWriteNodeWord :: Node a -> Word -> Word -> STM ()
unsafeWriteNodeWord marr (W# wi#) (W# w#) = STM $ \s# ->
    case writeTArrayWord# (unNode marr) wi# w# s# of
      s2# -> (# s2#, () #)
{-# INLINE unsafeWriteNodeWord #-}

lengthNode :: Node a -> Int
lengthNode marr = I# (sizeofSTMMutableArray# (unNode marr))
{-# INLINE lengthNode #-}

lengthNodeWords :: Node a -> Int
lengthNodeWords marr = I# (sizeofSTMMutableArrayWords# (unNode marr))
{-# INLINE lengthNodeWords #-}

instance Eq (Node a) where
  Nil == Nil = True
  (Node t) == (Node t') =
    case sameSTMMutableArray# t t' of
      0# -> False
      _  -> True
  _ == _ = False

#define KEY   0
#define COLOR 1

-- #define SEPARATE_POINTERS
#ifdef SEPARATE_POINTERS
#define VALUE  0
#define PARENT 8
#define LEFT   16
#define RIGHT  24
#else
#define VALUE  0
#define PARENT 1
#define LEFT   2
#define RIGHT  3
#endif

writeKey :: Node a -> Word -> STM ()
writeKey s x = unsafeWriteNodeWord s KEY x
{-# INLINE writeKey #-}

writeValue :: Node a -> a -> STM ()
writeValue s x = unsafeWriteNode s VALUE (unsafeCoerce x)
{-# INLINE writeValue #-}

writeColor :: Node a -> Color -> STM ()
writeColor s Black = unsafeWriteNodeWord s COLOR 0
writeColor s Red   = unsafeWriteNodeWord s COLOR 1
{-# INLINE writeColor #-}

writeParent :: Node a -> Node a -> STM ()
writeParent s x = unsafeWriteNode s PARENT x
{-# INLINE writeParent #-}

writeLeft :: Node a -> Node a -> STM ()
writeLeft s x = unsafeWriteNode s LEFT x
{-# INLINE writeLeft #-}

writeRight :: Node a -> Node a -> STM ()
writeRight s x = unsafeWriteNode s RIGHT x
{-# INLINE writeRight #-}

mkNode :: Key -> a -> Color -> STM (Node a)
mkNode k v c = do
#ifdef SEPARATE_POINTERS
    s <- newNode 32 2 Nil -- Minimal needed values
#else
    s <- newNode 4 2 Nil -- Minimal needed values
--    s <- newNode 3 5 Nil -- Padded end out to cacheline
--    s <- newNode 8 3 Nil -- Ptrs and words on separate cachelines
--    s <- newNode 8 8 Nil -- Ptrs and words on separate cachelines and padded
#endif
    writeKey s k
    writeValue s v
    writeColor s c
    return s
{-# INLINE mkNode #-}

key :: Node a -> STM Word
key Nil = return 0
key s   = unsafeReadNodeWord s KEY
{-# INLINE key #-}

value :: Node a -> STM a
value Nil = error "Nil dereference for value"
value s   = unsafeCoerce $ unsafeReadNode s VALUE
{-# INLINE value #-}

color :: Node a -> STM Color
color Nil = return Black
color s   = do
    w <- unsafeReadNodeWord s COLOR
    case w == (0 :: Word) of
      True -> return Black
      _    -> return Red
{-# INLINE color #-}

parent :: Node a -> STM (Node a)
parent Nil = return Nil
parent s   = unsafeReadNode s PARENT
{-# INLINE parent #-}

left :: Node a -> STM (Node a)
left Nil = return Nil
left s   = unsafeReadNode s LEFT
{-# INLINE left #-}

right :: Node a -> STM (Node a)
right Nil = return Nil
right s   = unsafeReadNode s RIGHT
{-# INLINE right #-}

