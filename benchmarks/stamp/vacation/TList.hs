{-# LANGUAGE BangPatterns #-}
module TList
    ( TList
    , mkTList

    , isEmpty
    , getSize
    , find
    , insert
    , remove
    , clear

    , forEach
    , foldMapTList
    ) where

import Control.Monad
import Control.Applicative

import Control.Concurrent
import Control.Concurrent.STM

import Data.Monoid

data Node a
    = Node { _value :: !a, _next :: TVar (Node a) }
    | Nil

isNode Nil = False
isNode _   = True

isNil Nil = True
isNil _   = False

mkNode :: a -> STM (Node a)
mkNode !a = Node a <$> newTVar Nil

mkNodeTo :: a -> Node a -> STM (Node a)
mkNodeTo !a n = Node a <$> newTVar n

-- Is it fair to remove the size?  It seems like an 
-- obvious conflict.
data TList a = TList { _head :: TVar (Node a), _size :: TVar Int }

mkTList :: STM (TList a)
mkTList = TList <$> newTVar Nil <*> newTVar 0

isEmpty :: TList a -> STM Bool
isEmpty l = isNil <$> readTVar (_head l)

getSize :: TList a -> STM Int
getSize l = readTVar (_size l)

-- Find the TVar that leads to the node with the given value
-- or the next value if it exists.  This differs from the C
-- implementation because we don't result in the *Node* before,
-- but the reference coming out of that node.
--
-- If the list is empty, we return the reference from the
-- head of the list.
findPrevious :: Ord a => TList a -> a -> STM (TVar (Node a))
findPrevious (TList h _) s = do
    p <- readTVar h
    if isNil p
      then return h
      else loop h p
  where
    loop p Nil = return p
    loop p (Node v n)
      | v >= s    = return p
      | otherwise = readTVar n >>= loop n 

find :: Ord a => TList a -> a -> STM (Maybe a)
find l s = do
    p <- findPrevious l s
    -- p is the pointer from the previous node, following p leads
    -- to the node that would have the value.
    n <- readTVar p
    case n of
      Nil      -> return Nothing
      Node v _ 
        | v == s    -> return (Just v)
        | otherwise -> return Nothing

insert :: Ord a => TList a -> a -> STM Bool
insert l !a = do
    p <- findPrevious l a
    -- p points to the existing node with the same value, or
    -- the node that will be the next value, or the end of the list.
    n <- readTVar p

    let act = mkNodeTo a n >>= writeTVar p >> modifyTVar' (_size l) (+1) >> return True

    case n of
      Node v _
        | v == a    -> return False -- LIST_NO_DUPLICATES
        | otherwise -> act
      Nil           -> act

remove :: Ord a => TList a -> a -> STM Bool
remove l a = do
    p <- findPrevious l a
    -- p points to the node to remove, a node with
    -- a greater value, or Nil.
    na <- readTVar p
    case na of
      Node v n
        | v == a    -> readTVar n >>= writeTVar p >> modifyTVar' (_size l) pred >> return True
        | otherwise -> return False
      Nil           -> return False

clear :: TList a -> STM ()
clear l = do
    writeTVar (_head l) Nil
    writeTVar (_size l) 0
      
forEach :: TList a -> (a -> STM ()) -> STM ()
forEach l f = readTVar (_head l) >>= run
  where
    run Nil = return ()
    run (Node a n) = f a >> readTVar n >>= run

foldMapTList :: Monoid m => (a -> m) -> TList a -> STM m
foldMapTList f l = readTVar (_head l) >>= run mempty
  where
    run !m Nil = return m
    run !m (Node a n) = (readTVar n >>= run (m <> f a))
