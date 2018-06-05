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

data Node k where
    Node :: !k   -- Key
         -> !Int -- Priority
         -> mutable (Node k) -- Left
         -> mutable (Node k) -- Right
         -> IO (Node k)
    Nil :: Node k

key    :: Node k -> k
key    (Node k _ _ _) = k

priority :: Node k -> Int
priority  (Node _ p _ _) = p


left   :: Node k -> Mutable (Node k)
left   (Node _ _ l _) = l

right  :: Node k -> Mutable (Node k)
right  (Node _ _ _ r) = r

instance Show k => Show (Node k) where
  show Nil = "Nil"
  show (Node k v _ _) = show ("Node " ++ show k ++ " " ++ show v)

instance Eq k => Eq (Node k) where
  Nil == Nil = True
  Nil == _   = False
  _   == Nil = False

--  x   == y   = case reallyUnsafePtrEquality# x y of
--                 0# -> False
--                 _  -> True
  (Node k _ _ _) == (Node k' _ _ _) = k == k'

isNil :: Node k -> Bool
isNil Nil = True
isNil _   = False

isNode = not . isNil

-- Implementation follows: 
--  https://github.com/frol/completely-unscientific-benchmarks/blob/master/c%23/Program.cs
mkNode :: GenIO -> k -> IO (Node k)
mkNode g k = do
    p <- uniform g
    Node k p Nil Nil

merge :: Node k -> Node k -> IO (Node k)
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

splitL :: Ord k => k -> Node k -> IO (Node k, Node k)
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

splitLEq :: Ord k => k -> Node k -> IO (Node k, Node k)
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

merge3 :: Node k -> Node k -> Node k -> IO (Node k)
merge3 l e g = do
    l' <- merge l e
    merge l' g

split :: Ord k => k -> Node k -> IO (Node k, Node k, Node k)
split k n = do
    (lof, los) <- splitL   k n
    (egf, egs) <- splitLEq k los
    return (lof, egf, egs)

hasValue :: Ord k => k -> Node k -> IO (Node k, Bool)
hasValue k n = do
    (l,e,g) <- split k n
    n' <- merge3 l e g
    return $ (n', isNode e)

insert :: Ord k => GenIO -> k -> Node k -> IO (Node k)
insert gen k n = do
    (l,e,g) <- split k n
    e' <- if isNode e then return e else mkNode gen k
    merge3 l e' g

erase :: Ord k => k -> Node k -> IO (Node k)
erase k n = do
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
                    (t', b) <- hasValue cur' t
                    if b
                      then loop (i+1) cur' (res + 1) t'
                      else loop (i+1) cur' res       t'
    loop 0 5 0 Nil >>= print
