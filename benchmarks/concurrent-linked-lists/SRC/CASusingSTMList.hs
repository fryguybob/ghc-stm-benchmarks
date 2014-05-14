
module CASusingSTMList where

import GHC.IO
import Data.IORef
import Control.Concurrent
import Control.Concurrent.STM
import System.Environment
import Data.Time


data List a = Node { val :: a
                   , next :: TVar (List a) }
            | DelNode { next :: TVar (List a) }
            | Null
            | Head { next :: TVar (List a) } deriving Eq

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

-------------------------------------------
-- auxilliary functions



while b cmd = if b then do {cmd; while b cmd}
              else return ()

repeatUntil cmd = do { b <- cmd; if b then return ()
                                  else repeatUntil cmd }

atomCAS :: Eq a => TVar a -> a -> a -> IO Bool
atomCAS ptr old new =
   atomically ( do
       cur <- readTVar ptr
       if cur == old 
        then do writeTVar ptr new
                return True
        else return False
   )


----------------------------------------------
-- functions operating on lists


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
addToTail :: Eq a => ListHandle a -> a -> IO (TVar (List a))
addToTail (ListHandle {tailList = tailPtrPtr}) x =
   do null <- atomically (newTVar Null)
      tPtr <- atomically (
                do tailPtr <- readTVar tailPtrPtr
                   writeTVar tailPtr (Node {val = x, next = null})
                   writeTVar tailPtrPtr null
                   return tailPtr
              )
               -- in essence, mimics a single atomic write (CAS)
      return tPtr


find :: Eq a => ListHandle a -> a -> IO Bool
find (ListHandle { headList = head }) x =
  let go prevPtr =
        do do prevNode <- atomically (readTVar prevPtr)
              let curPtr = next prevNode -- head/node/delnode have all next
              curNode <- atomically (readTVar curPtr)
              case curNode of
                Node {val = y, next = nextNode } ->
                   if (x == y)
                   then -- node found and alive 
                      return True
                   else go curPtr -- continue
                Null -> return False -- reached end of list
                DelNode {next = nextNode } -> 
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

  in do startPtr <- atomically (readTVar head)
        go startPtr




delete :: Eq a => ListHandle a -> a -> IO Bool
delete (ListHandle { headList = head }) x =
  let go prevPtr =
        do do prevNode <- atomically (readTVar prevPtr)
              let curPtr = next prevNode -- head/node/delnode have all next
              curNode <- atomically (readTVar curPtr)
              case curNode of
                Node {val = y, next = nextNode } ->
                   if (x == y)
                   then -- node found and alive 
                      do b <- atomCAS curPtr curNode (DelNode {next = nextNode})
                         if b then return True
                          else go prevPtr -- spin
                   else go curPtr -- continue
                Null -> return False -- reached end of list
                DelNode {next = nextNode } -> 
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

  in do startPtr <- atomically (readTVar head)
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