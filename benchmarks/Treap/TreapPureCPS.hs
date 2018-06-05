-- Strict makes a huge difference.
{- LANGUAGE Strict -}
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad.Primitive (PrimMonad, PrimState)
import System.Random.PCG.Fast.Pure (withSystemRandom, Gen, uniform)

data Node
  = Empty
  | Node {
    xValue :: !Int,
    yValue :: !Int,
    left :: Node,
    right :: Node
  }

isNode :: Node -> Bool
isNode Node{} = True
isNode Empty = False

newNode :: PrimMonad m => Gen (PrimState m) -> Int -> m Node
newNode gen !x = Node x <$> uniform gen <*> pure Empty <*> pure Empty

merge :: Node -> Node -> Node
merge Empty greater = greater
merge lower Empty = lower
merge lower greater
  | yValue lower < yValue greater = lower { right = merge (right lower) greater }
  | otherwise = greater { left = merge lower (left greater) }

splitBinary :: Node -> Int -> (Node -> Node -> a) -> a
splitBinary Empty _ c = c Empty Empty
splitBinary origNode !value c
  | xValue origNode < value =
     splitBinary (right origNode) value $ \l r -> c origNode { right = l } r
  | otherwise =
     splitBinary (left origNode)  value $ \l r -> c l origNode { left = r }

merge3 :: Node -> Node -> Node -> Node
merge3 lower equal greater = (lower `merge` equal) `merge` greater

split :: Node -> Int -> (Node -> Node -> Node -> a) -> a
split orig !value c =
    splitBinary orig value $ \lower equalGreater ->
        splitBinary equalGreater (value+1) $ \equal greater ->
            c lower equal greater

type Tree = Node

emptyTree :: Tree
emptyTree = Empty

hasValue :: Tree -> Int -> (Tree, Bool)
hasValue node !x = split node x $ \lower equal greater ->
      (merge3 lower equal greater, isNode equal)

insert :: PrimMonad m => Gen (PrimState m) -> Tree -> Int -> m Tree
insert gen tree !x = split tree x $ \lower equal greater ->
    if isNode equal
      then pure $ merge3 lower equal greater
      else merge3 <$> pure lower <*> newNode gen x <*> pure greater

erase :: Tree -> Int -> Tree
erase tree !x = split tree x $ \lower _ greater -> merge lower greater

run :: PrimMonad m => Gen (PrimState m) -> m Int
run gen = loop 0 emptyTree 5 0 where
  loop !i tree !cur !res
    | i >= (1000000 :: Int) = pure res
    | otherwise =
      let !a = i `mod` 3
          !newCur = (cur * 57 + 43) `mod` 10007
          !newI = i+1
      in case a of
           0 -> do
             newTree <- insert gen tree newCur
             loop newI newTree newCur res
           1 ->
             let newTree = erase tree newCur
             in loop newI newTree newCur res
           2 ->
             case hasValue tree newCur of
               (newTree, has) ->
                 let !newRes = if has then res+1 else res
                 in loop newI newTree newCur newRes
           _ ->
             loop newI tree newCur res

main :: IO ()
main = do
  result <- withSystemRandom run
  print result
