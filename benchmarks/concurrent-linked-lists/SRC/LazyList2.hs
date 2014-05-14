 
--------------------------------------------------------------------------------
--
-- Copyright (C) 2008 Martin Sulzmann, Edmund Lam. All rights reserved.

 
{-
Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

The names of the copyright holders may not be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


-}

module LazyList2 where

import Control.Concurrent
import Control.Concurrent.STM


---------------------------------------
-- API for a thread-safe singly-linked list


data List a = Node { val :: a
                   , next :: TVar (List a) }
            | DelNode { next :: TVar (List a) }
            | Null
            | Head { next :: TVar (List a) }

data ListHandle a = ListHandle { headList :: TVar (TVar (List a)), 
                             tailList :: TVar (TVar (List a)) }


-- we assume a static head pointer, pointing to the first node which must be Head
-- the deleted field of Head is always False, it's only there to make some of the code
-- more uniform
-- tail points to the last node which must be Null

type Iterator a = TVar (TVar (List a))


-- abbreviations

newAtomic x = atomically (newTVar x)

readAtomic x = atomically (readTVar x)

writeAtomic ptr x = atomically (writeTVar ptr x)

-- basic api

-- we create a new list
newList :: IO (ListHandle a)
newList = 
   do null <- newAtomic Null
      hd <- newAtomic (Head {next = null })
      hdPtr <- newAtomic hd
      tailPtr <- newAtomic null
      return (ListHandle {headList = hdPtr, tailList = tailPtr})


-- we add a new node, by overwriting the null tail node
-- we only need to adjust tailList but not headList because
-- of the static Head
-- we return the location of the newly added node
addToTail :: ListHandle a -> a -> IO (TVar (List a))
addToTail (ListHandle {tailList = tailPtrPtr}) x =
   do null <- atomically (newTVar Null)
      tPtr <- atomically (
                do --null <- newTVar Null -- move out
                   tailPtr <- readTVar tailPtrPtr
                   writeTVar tailPtr (Node {val = x, next = null})
                   writeTVar tailPtrPtr null
                   return tailPtr
              )
      return tPtr


{-
-- the iterator always points to the PREVIOUS node,
-- recall that there's a static dummy new Head
newIterator :: ListHandle a -> IO (Iterator a)
newIterator (ListHandle {headList = hd}) =
  do hdPtr <- readAtomic hd
     it <- newAtomic hdPtr
     return it


-- test if iterators point to same location
testEqIterator :: Iterator a -> Iterator a -> IO Bool
testEqIterator it1 it2 =
  do it1Ptr <- readAtomic it1
     it2Ptr <- readAtomic it2
     return (it1Ptr == it2Ptr)

-- assign the rhs iterator's current pointer to the lhs iterator
assignIterator :: Iterator a -> Iterator a -> IO ()
assignIterator lhs rhs =
 atomically (
  do rhsVal <- readTVar rhs
     writeTVar lhs rhsVal
 )
     

-- we iterate through the list and return the first "not deleted" node
-- we delink deleted nodes
-- there's no need to adjust headList, tailList
-- cause headList has a static Head and
-- tailList points to Null
iterateList :: Iterator a -> IO (Maybe (TVar (List a)))
iterateList itPtrPtr = 
  let go prevPtr =
        do action <- atomically (
                do prevNode <- readTVar prevPtr
                   let curPtr = next prevNode -- head/node have both next
                   curNode <- readTVar curPtr
                   case curNode of
                     Null -> return (return Nothing) -- reached end of list
                     Node {deleted = del, next = nextNode } ->
                       if not del
                       then -- node available
                            return (do writeAtomic itPtrPtr curPtr  -- move iterator forward
                                       return (Just curPtr))
                       else -- delete curNode by setting the next of prevNode to next of curNode
                         if deleted prevNode     -- first check if parent is deleted
                         then do writeTVar itPtrPtr curPtr
                                 return (go curPtr)           -- if parent deleted simply move ahead
                              -- NOTE: we could avoid adjusting the iterator, cause
                              -- either we hit a success case and the iterator will be adjusted
                              -- or we hit Null and the search will stop
                              -- NOTE2: it's better to adjust the iterator cause then we don't
                              -- have to scan through garbbage, ie deleted nodes
                              -- Consider the case of a barrier
                              -- NOTE3: iterators are never shared, so probably
                              -- we can adjust them outside the "atomic"
                              -- possibly have IORef (TVar (List a)) instead
                         else
                           do case prevNode of  -- prevNode still alive, so delink curNode
                                Head {} -> writeTVar prevPtr (Head {next = nextNode, deleted = False})
                                Node {} -> writeTVar prevPtr 
                                               (Node {val = val prevNode, 
                                                      deleted = deleted prevNode, 
                                                      next = nextNode})
                              return (go prevPtr)
               )
           action
  in do startPtr <- readAtomic itPtrPtr
        go startPtr
-}

-- more efficient version, avoiding the iterator step
findOld :: Eq a => ListHandle a -> a -> IO Bool
findOld (ListHandle { headList = head }) x =
  let go prevPtr =
        do action <- atomically (
                do prevNode <- readTVar prevPtr
                   let curPtr = next prevNode -- head/node/delnode have all next
                   curNode <- readTVar curPtr
                   case curNode of
                     Node {val = y, next = nextNode } ->
                       if (x == y)
                       then -- node found and alive 
                            return (return True)
                       else return (go curPtr) -- continue
                     Null -> return (return False) -- reached end of list
                     DelNode {next = nextNode } -> 
                         -- delete curNode by setting the next of prevNode to next of curNode
                        case prevNode of
                          Node {} -> do writeTVar prevPtr 
                                               (Node {val = val prevNode, 
                                                      next = nextNode})
                                        return (go prevPtr)
                          Head {} -> do writeTVar prevPtr (Head {next = nextNode})
                                        return (go prevPtr)
                          DelNode {} -> return (go curPtr)    -- if parent deleted simply move ahead
                              
                                           
               )
           action
  in do startPtr <- readAtomic head
        go startPtr


deleteOld :: Eq a => ListHandle a -> a -> IO Bool
deleteOld (ListHandle { headList = head }) x =
  let go prevPtr =
        do action <- atomically (
                do prevNode <- readTVar prevPtr
                   let curPtr = next prevNode -- head/node/delnode have all next
                   curNode <- readTVar curPtr
                   case curNode of
                     Null -> return (return False) -- reached end of list
                     Node {val = y, next = nextNode } ->
                       if (x == y)
                       then -- node found and alive, logical delete
                            do writeTVar curPtr (DelNode {next = nextNode})
                               return (return True)
                       else return (go curPtr) -- continue
                     DelNode {next = nextNode } -> 
                         -- delete curNode by setting the next of prevNode to next of curNode
                        case prevNode of
                          Node {} -> do writeTVar prevPtr 
                                               (Node {val = val prevNode, 
                                                      next = nextNode})
                                        return (go prevPtr)
                          Head {} -> do writeTVar prevPtr (Head {next = nextNode})
                                        return (go prevPtr)
                          DelNode {} -> return (go curPtr)    -- if parent deleted simply move ahead
                                           
               )
           action
  in do startPtr <- readAtomic head
        go startPtr

find lh x = searchAndExecute lh x (\ _ -> \ _ -> return (return True))
delete lh x = searchAndExecute lh x (\ curPtr -> \ curNode -> 
                                   do writeTVar curPtr (DelNode {next = next curNode})
                                      return (return True))

searchAndExecute :: Eq a => ListHandle a -> a -> (TVar (List a) -> List a -> STM (IO Bool)) -> IO Bool
searchAndExecute (ListHandle { headList = head }) x apply =
  let go prevPtr =
        do action <- atomically (
                do prevNode <- readTVar prevPtr
                   let curPtr = next prevNode -- head/node/delnode have all next
                   curNode <- readTVar curPtr
                   case curNode of
                     Node {val = y, next = nextNode } ->
                       if (x == y)
                       then -- node found and alive 
                            apply curPtr curNode 
                       else return (go curPtr) -- continue
                     Null -> return (return False) -- reached end of list
                     DelNode {next = nextNode } -> 
                         -- delete curNode by setting the next of prevNode to next of curNode
                        case prevNode of
                          Node {} -> do writeTVar prevPtr 
                                               (Node {val = val prevNode, 
                                                      next = nextNode})
                                        return (go prevPtr)
                          Head {} -> do writeTVar prevPtr (Head {next = nextNode})
                                        return (go prevPtr)
                          DelNode {} -> return (go curPtr)    -- if parent deleted simply move ahead
                              
                                           
               )
           action
  in do startPtr <- readAtomic head
        go startPtr


-- printing and counting

printList :: Show a => ListHandle a -> IO ()
printList (ListHandle {headList = ptrPtr}) =
  do startptr <- atomically (
          do ptr <- readTVar ptrPtr
             Head {next = startptr} <- readTVar ptr
             return startptr)
     printListHelp startptr


printListHelp :: Show a => TVar (List a) -> IO ()
printListHelp curNodePtr =
   do { curNode <- atomically (readTVar curNodePtr)
      ; case curNode of
          Null -> putStr "Nil"
          Node {val = curval, next = curnext} ->
             do { putStr (show curval  ++ " -> ")
                ;  printListHelp curnext }
          DelNode {next = curnext} ->
             do { putStr ("DEL -> ")
                ;  printListHelp curnext }
      } 

cntList :: Show a => ListHandle a -> IO Int
cntList (ListHandle {headList = ptrPtr}) =
  do startptr <- atomically (
          do ptr <- readTVar ptrPtr
             Head {next = startptr} <- readTVar ptr
             return startptr)
     cntListHelp startptr 0

cntListHelp :: Show a => TVar (List a) -> Int -> IO Int
cntListHelp curNodePtr i =
   do { curNode <- atomically (readTVar curNodePtr)
      ; case curNode of
          Null -> return i
          Node {val = curval, next = curnext} ->
             cntListHelp curnext (i+1)
          DelNode {next = curnext} ->
             cntListHelp curnext (i+1)
      }