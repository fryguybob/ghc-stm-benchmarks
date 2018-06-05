{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MutableFields             #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE Strict #-}
module Main where

import System.Random.PCG.Fast.Pure
import GHC.Types
import GHC.Prim
import GHC.IO
import GHC.Int
import GHC.Conc

type Mutable a = Ref# RealWorld a

readRef :: Mutable a -> IO a
readRef r = IO (\s# -> readRef# r s#)

writeRef :: Mutable a -> a -> IO ()
writeRef r l = IO (\s1# -> case writeRef# r l s1# of
                             s2# -> (# s2#, () #))

data Node where
    Node :: !Int -- Key
         -> !Int -- Priority
         -> mutable (Node) -- Left
         -> mutable (Node) -- Right
         -> IO (Node)
    Nil :: Node

key    :: Node -> Int
key    (Node k _ _ _) = k

priority :: Node -> Int
priority  (Node _ p _ _) = p


left   :: Node -> Mutable (Node)
left   (Node _ _ l _) = l

right  :: Node -> Mutable (Node)
right  (Node _ _ _ r) = r

instance Show (Node) where
  show Nil = "Nil"
  show (Node k v _ _) = show ("Node " ++ show k ++ " " ++ show v)

instance Eq Node where
  Nil == Nil = True
  Nil == _   = False
  _   == Nil = False

--  x   == y   = case reallyUnsafePtrEquality# x y of
--                 0# -> False
--                 _  -> True
  (Node k _ _ _) == (Node k' _ _ _) = k == k'

isNil :: Node -> Bool
isNil Nil = True
isNil _   = False

isNode = not . isNil

-- Implementation follows: 
--  https://github.com/frol/completely-unscientific-benchmarks/blob/master/c%23/Program.cs
mkNode :: GenIO -> Int -> IO (Node)
mkNode g k = do
    p <- uniform g
    Node k p Nil Nil

merge :: Node -> Node -> IO (Node)
merge Nil g   = return g
merge l   Nil = return l
merge l@(Node _ lp _ lr) g@(Node _ gp gl _)
  | lp < gp = do
    lrn <- readRef lr
    merge lrn g >>= writeRef lr
    return l
  | otherwise = do
    gln <- readRef gl
    merge l gln >>= writeRef gl
    return g

splitL :: Int -> Node -> IO (Node, Node)
splitL _   Nil = return (Nil, Nil)
splitL key n@(Node k _ l r)
  | k < key = do
    (f, s) <- readRef r >>= splitL key
    writeRef r f
    return (n, s)
  | otherwise = do
    (f, s) <- readRef l >>= splitL key
    writeRef l s
    return (f, n)

splitLEq :: Int -> Node -> IO (Node, Node)
splitLEq _   Nil = return (Nil, Nil)
splitLEq key n@(Node k _ l r)
  | k <= key = do
    (f, s) <- readRef r >>= splitLEq key
    writeRef r f
    return (n, s)
  | otherwise = do
    (f, s) <- readRef l >>= splitLEq key
    writeRef l s
    return (f, n)

merge3 :: Node -> Node -> Node -> IO (Node)
merge3 l e g = do
    l' <- merge l e
    merge l' g

split :: Int -> Node -> IO (Node, Node, Node)
split k n = do
    (lof, los) <- splitL   k n
    (egf, egs) <- splitLEq k los
    return (lof, egf, egs)

hasValue :: Int -> Node -> IO Bool
hasValue k n = do
    let loop Nil = return False
        loop (Node k' _ l r)
          | k < k'    = readRef l >>= loop
          | k > k'    = readRef r >>= loop
          | otherwise = return True
    loop n

insert :: GenIO -> Int -> Node -> IO Node
insert gen k n = do
    b <- hasValue k n
    if b
      then return n
      else do
        (l,e,g) <- split k n
        e' <- mkNode gen k
        merge3 l e' g

erase :: Int -> Node -> IO Node
erase k n = do
    b <- hasValue k n
    if not b
      then return n
      else do
        (l,e,g) <- split k n
        merge l g

main = do
    g <- create
    let loop i cur res t
          | i >= 1000000 = return res
          | otherwise    = do
            let cur' = (cur * 57 + 43) `mod` 10007
                a = i `mod` 3
            case a of
                0 -> insert g cur' t >>= loop (i+1) cur' res
                1 -> erase    cur' t >>= loop (i+1) cur' res
                2 -> do
                    b <- hasValue cur' t
                    if b
                      then loop (i+1) cur' (res + 1) t
                      else loop (i+1) cur' res       t
    loop 0 5 0 Nil >>= print
