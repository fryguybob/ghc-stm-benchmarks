-- Strict makes a huge difference.
{-# LANGUAGE Strict #-}

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
newNode gen x = Node x <$> uniform gen <*> pure Empty <*> pure Empty

merge :: Node -> Node -> Node
merge Empty greater = greater
merge lower Empty = lower
merge lower greater
  | yValue lower < yValue greater = lower { right = merge (right lower) greater }
  | otherwise = greater { left = merge lower (left greater) }

splitBinary :: Node -> Int -> (Node, Node)
splitBinary Empty _ = (Empty, Empty)
splitBinary origNode value
  | xValue origNode < value =
    case splitBinary (right origNode) value of
      (l, r) -> (origNode { right = l }, r)
  | otherwise =
    case splitBinary (left origNode) value of
      (l, r) -> (l, origNode { left = r })

merge3 :: Node -> Node -> Node -> Node
merge3 lower equal greater = (lower `merge` equal) `merge` greater

type SplitResult = (Node, Node, Node)

split :: Node -> Int -> SplitResult
split orig value =
  case splitBinary orig value of
    (lower, equalGreater) ->
      case splitBinary equalGreater (value+1) of
        (equal, greater) -> (lower, equal, greater)

type Tree = Node

emptyTree :: Tree
emptyTree = Empty

hasValue :: Tree -> Int -> Bool
hasValue Empty _ = False
hasValue n  x
  | x < xValue n = hasValue (left  n) x
  | x > xValue n = hasValue (right n) x
  | otherwise    = True

insert :: PrimMonad m => Gen (PrimState m) -> Tree -> Int -> m Tree
insert gen tree x =
  case split tree x of
    (lower, equal, greater) ->
      if isNode equal
      then pure $ merge3 lower equal greater
      else merge3 <$> pure lower <*> newNode gen x <*> pure greater

erase :: Tree -> Int -> Tree
erase tree x =
  case split tree x of
    (lower, _, greater) -> merge lower greater

run :: PrimMonad m => Gen (PrimState m) -> m Int
run gen = loop 0 emptyTree 5 0 where
  loop i tree cur res
    | i >= (1000000 :: Int) = pure res
    | otherwise =
      let a = i `mod` 3
          newCur = (cur * 57 + 43) `mod` 10007
          newI = i+1
      in case a of
           0 -> do
             newTree <- insert gen tree newCur
             loop newI newTree newCur res
           1 ->
             let newTree = erase tree newCur
             in loop newI newTree newCur res
           2 ->
             let has = hasValue tree newCur
                 newRes = if has then res+1 else res
             in loop newI tree newCur newRes
           _ ->
             loop newI tree newCur res

main :: IO ()
main = do
  result <- withSystemRandom run
  print result
