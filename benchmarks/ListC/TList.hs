module TList
    ( TList
    , mkTList

    , isEmpty
    , getSize
    , find
    , find'
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
    = Node { _value :: a, _next :: TVar (Node a) }
    | Nil

isNode Nil = False
isNode _   = True

isNil Nil = True
isNil _   = False

mkNode :: a -> STM (Node a)
mkNode a = Node a <$> newTVar Nil

-- Is it fair to remove the size?  It seems like an 
-- obvious conflict.
data TList a = TList { _head :: TVar (Node a), _size :: TVar Int }

mkTList :: STM (TList a)
mkTList = TList <$> newTVar Nil <*> newTVar 0

isEmpty :: TList a -> STM Bool
isEmpty l = isNil <$> readTVar (_head l)

getSize :: TList a -> STM Int
getSize l = readTVar (_size l)

-- Find the TVar pointing to the node matching the predicate
findPrevious :: TList a -> (a -> Bool) -> STM (TVar (Node a))
findPrevious (TList h _) f = do
    p <- readTVar h
    if isNil p
      then return h
      else loop h p
  where
    loop p Nil = return p
    loop p (Node v n)
      | f v       = return p
      | otherwise = readTVar n >>= loop n 

find' :: TList a -> (a -> Bool) -> STM (Maybe a)
find' l f = do
    p <- findPrevious l f
    n <- readTVar p
    case n of
      Nil      -> return Nothing
      Node v _ -> return (Just v)

find :: Eq a => TList a -> a -> STM (Maybe a)
find l a = find' l (== a)

insert :: Eq a => TList a -> a -> STM Bool
insert l a = do
    p <- findPrevious l (== a)
    n <- readTVar p

    case n of
      Nil      -> mkNode a >>= writeTVar p >> modifyTVar (_size l) (+1) >> return True
      Node v n -> return False

remove :: Eq a => TList a -> a -> STM Bool
remove l a = do
    p <- findPrevious l (== a)
    n <- readTVar p
    case n of
      Nil      -> return False
      Node v n -> if v /= a
                    then return False
                    else do
                      readTVar n >>= writeTVar p
                      writeTVar n Nil
                      modifyTVar (_size l) pred
                      return True

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
    run m Nil = return m
    run m (Node a n) = readTVar n >>= run (m <> f a)
