{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import System.Random.PCG.Fast.Pure
import GHC.Types
import GHC.Prim
import GHC.IO
import GHC.Int
import GHC.Conc

import Data.IORef

data Node = Node 
          { key      :: !Int
          , priority :: !Int
          , left     :: IORef (Node)
          , right    :: IORef (Node)
          }
    | Nil

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
    l <- newIORef Nil
    r <- newIORef Nil
    return $ Node k p l r

merge :: Node -> Node -> IO (Node)
merge Nil g   = return g
merge l   Nil = return l
merge l@(Node _ lp _ lr) g@(Node _ gp gl _)
  | lp < gp = do
    lrn <- readIORef lr
    merge lrn g >>= writeIORef lr
    return l
  | otherwise = do
    gln <- readIORef gl
    merge l gln >>= writeIORef gl
    return g

splitL :: Int -> Node -> IO (Node, Node)
splitL _   Nil = return (Nil, Nil)
splitL key n@(Node k _ l r)
  | k < key = do
    (f, s) <- readIORef r >>= splitL key
    writeIORef r f
    return (n, s)
  | otherwise = do
    (f, s) <- readIORef l >>= splitL key
    writeIORef l s
    return (f, n)

splitLEq :: Int -> Node -> IO (Node, Node)
splitLEq _   Nil = return (Nil, Nil)
splitLEq key n@(Node k _ l r)
  | k <= key = do
    (f, s) <- readIORef r >>= splitLEq key
    writeIORef r f
    return (n, s)
  | otherwise = do
    (f, s) <- readIORef l >>= splitLEq key
    writeIORef l s
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

hasValue :: Int -> Node -> IO (Node, Bool)
hasValue k n = do
    (l,e,g) <- split k n
    n' <- merge3 l e g
    return $ (n', isNode e)

insert :: GenIO -> Int -> Node -> IO (Node)
insert gen k n = do
    (l,e,g) <- split k n
    e' <- if isNode e then return e else mkNode gen k
    merge3 l e' g

erase :: Int -> Node -> IO (Node)
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
