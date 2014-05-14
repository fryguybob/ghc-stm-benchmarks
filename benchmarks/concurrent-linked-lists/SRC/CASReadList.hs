
module CASReadList where

import IO
import GHC.IOBase
import Monad
import Data.IORef
import Control.Concurrent
import Control.Concurrent.Chan
import System.Environment
import Data.Time


data List a = Node { val :: a
                   , next :: IORef (List a) }
            | DelNode { val :: a
                      , next :: IORef (List a) }
            | Null
            | Head { next :: IORef (List a) } deriving Eq

data ListHandle a = ListHandle { headList :: IORef (IORef (List a)), 
                             tailList :: IORef (IORef (List a)) }


-- we assume a static head pointer, pointing to the first node which must be Head
-- the deleted field of Head is always False, it's only there to make some of the code
-- more uniform
-- tail points to the last node which must be Null


type Iterator a = IORef (IORef (List a))


-------------------------------------------
-- auxilliary functions



while b cmd = if b then do {cmd; while b cmd}
              else return ()

repeatUntil cmd = do { b <- cmd; if b then return ()
                                  else repeatUntil cmd }

atomCAS :: Eq a => IORef a -> a -> a -> IO Bool
atomCAS ptr old new =
   atomicModifyIORef ptr (\ cur -> if cur == old
                                   then (new, True)
                                   else (cur, False))

atomicWrite :: IORef a -> a -> IO ()
atomicWrite ptr x =
   atomicModifyIORef ptr (\ _ -> (x,()))

atomicRead :: IORef a -> IO a
atomicRead ptr = do
      x <- atomicModifyIORef ptr (\cur -> (cur,cur))
      return x

----------------------------------------------
-- functions operating on lists


-- we create a new list
newList :: IO (ListHandle a)
newList = 
   do null <- newIORef Null
      hd <- newIORef (Head {next = null })
      hdPtr <- newIORef hd
      tailPtr <- newIORef null
      return (ListHandle {headList = hdPtr, tailList = tailPtr})


-- we add a new node, by overwriting the null tail node
-- we only need to adjust tailList but not headList because
-- of the static Head
-- we return the location of the newly added node
addToTail :: Eq a => ListHandle a -> a -> IO ()
addToTail (ListHandle {tailList = tailPtrPtr}) x =
   do null <- newIORef Null
      repeatUntil 
         (do tailPtr <- atomicRead tailPtrPtr
             b <- atomCAS tailPtr Null (Node {val = x, next = null})
             return b )
        -- we atomically update the tail
        -- (by spinning on the tailPtr)
      atomicWrite tailPtrPtr null


find :: Eq a => ListHandle a -> a -> IO Bool
find (ListHandle { headList = head }) x =
  let go prevPtr =
        do do prevNode <- atomicRead prevPtr
              let curPtr = next prevNode -- head/node/delnode have all next
              curNode <- atomicRead curPtr
              case curNode of
                Node {val = y, next = nextNode } ->
                   if (x == y)
                   then -- node found and alive 
                      return True
                   else go curPtr -- continue
                Null -> return False -- reached end of list
                DelNode {val = y, next = nextNode } -> 
                         -- atomically delete curNode by setting the next of prevNode to next of curNode
                         -- if this fails we simply move ahead
                        case prevNode of
                          Node {} -> do b <- atomCAS prevPtr prevNode (Node {val = val prevNode, 
                                                                             next = nextNode})
                                        if b then go prevPtr
                                         else go curPtr
                          Head {} -> do b <- atomCAS prevPtr prevNode (Head {next = nextNode})
                                        if b then go prevPtr 
                                         else go curPtr
                          DelNode {} -> go curPtr    -- if parent deleted simply move ahead
             {-
                correct as well, but a deleted parent deleting a child is (for certain cases) a useless operation
                                     do atomicModifyIORef prevPtr ( \ cur -> (cur{next = nextNode},True))
                                        go prevPtr
              -}

  in do startPtr <- atomicRead head
        go startPtr




delete :: Eq a => ListHandle a -> a -> IO Bool
delete (ListHandle { headList = head }) x =
  let go prevPtr =
        do do prevNode <- atomicRead prevPtr
              let curPtr = next prevNode -- head/node/delnode have all next
              curNode <- atomicRead curPtr
              case curNode of
                Node {val = y, next = nextNode } ->
                   if (x == y)
                   then -- node found and alive 
                      do b <- atomCAS curPtr curNode (DelNode {val = y, next = nextNode})
                         if b then return True
                          else go prevPtr -- spin
                   else go curPtr -- continue
                Null -> return False -- reached end of list
                DelNode {val = y, next = nextNode } -> 
                   if x == y then return True -- already deleted
                   else
                         -- atomically delete curNode by setting the next of prevNode to next of curNode
                         -- if this fails we simply move ahead
                        case prevNode of
                          Node {} -> do b <- atomCAS prevPtr prevNode (Node {val = val prevNode, 
                                                                             next = nextNode})
                                        if b then go prevPtr
                                         else go curPtr
                          Head {} -> do b <- atomCAS prevPtr prevNode (Head {next = nextNode})
                                        if b then go prevPtr 
                                         else go curPtr
                          DelNode {} -> go curPtr    -- if parent deleted simply move ahead

  in do startPtr <- atomicRead head
        go startPtr



-- the iterator always points to the PREVIOUS node,
-- recall that there's a static dummy new Head
-- Assumption: iterators are private, 
-- ie they won't be shared among threads
newIterator :: ListHandle a -> IO (Iterator a)
newIterator (ListHandle {headList = hd}) =
  do hdPtr <- atomicRead hd
     it <- newIORef hdPtr
     return it

-- we iterate through the list and return the first "not deleted" node
-- we delink deleted nodes
-- there's no need to adjust headList, tailList
-- cause headList has a static Head and
-- tailList points to Null
iterateList :: Eq a => Iterator a -> IO (Maybe (IORef (List a)))
iterateList itPtrPtr = 
  let go prevPtr =
        do do prevNode <- atomicRead prevPtr
              let curPtr = next prevNode -- head/node/delnode have all next
              curNode <- atomicRead curPtr
              case curNode of
                Node {} -> do writeIORef itPtrPtr curPtr 
                                 -- adjust iterator
                              return (Just curPtr)
                Null -> return Nothing -- reached end of list
                DelNode {next = nextNode} -> 
                         -- atomically delete curNode by setting the next of prevNode to next of curNode
                         -- if this fails we simply move ahead
                        case prevNode of
                          Node {} -> do b <- atomCAS prevPtr prevNode (Node {val = val prevNode, 
                                                                             next = nextNode})
                                        if b then go prevPtr
                                         else go curPtr
                          Head {} -> do b <- atomCAS prevPtr prevNode (Head {next = nextNode})
                                        if b then go prevPtr 
                                         else go curPtr
                          DelNode {} -> go curPtr    -- if parent deleted simply move ahead

  in do startPtr <- atomicRead itPtrPtr
        go startPtr


--printing and counting

printList :: Show a => ListHandle a -> IO ()
printList (ListHandle {headList = ptrPtr}) =
  do startptr <- (
          do ptr <- atomicRead ptrPtr
             Head {next = startptr} <- atomicRead ptr
             return startptr)
     printListHelp startptr


printListHelp :: Show a => IORef (List a) -> IO ()
printListHelp curNodePtr =
   do { curNode <- atomicRead curNodePtr
      ; case curNode of
          Null -> putStr "Nil"
          Node {val = curval, next = curnext} ->
             do { putStr (show curval  ++ " -> ")
                ;  printListHelp curnext }
          DelNode {val = curval, next = curnext} ->
             do { putStr (show curval  ++ "DEAD -> ")
                ;  printListHelp curnext }
      } 

cntList :: Show a => ListHandle a -> IO Int
cntList (ListHandle {headList = ptrPtr}) =
  do startptr <- (
          do ptr <- atomicRead ptrPtr
             Head {next = startptr} <- atomicRead ptr
             return startptr)
     cntListHelp startptr 0


cntListHelp :: Show a => IORef (List a) -> Int -> IO Int
cntListHelp curNodePtr i =
   do { curNode <- atomicRead curNodePtr
      ; case curNode of
          Null -> return i
          Node {val = curval, next = curnext} -> 
                cntListHelp curnext (i+1)
          DelNode {val = curval, next = curnext} ->
                cntListHelp curnext (i+1)
      } 