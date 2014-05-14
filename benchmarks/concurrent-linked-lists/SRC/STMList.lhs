
> module STMList where

> import Data.IORef
> import Control.Concurrent
> import Control.Concurrent.STM


> data List a = Node { val :: a, next :: TVar (List a)}   
>           | Null
>           | Head {next :: TVar (List a) }


> data ListHandle a = ListHandle { headList :: TVar (TVar (List a)),
>                            tailList :: TVar (TVar (List a)) }


  abbreviations

> newAtomic x = atomically (newTVar x)

> readAtomic x = atomically (readTVar x)

> writeAtomic ptr x = atomically (writeTVar ptr x)

> -- we create a new list
> newList :: IO (ListHandle a)
> newList = 
>   do null <- newAtomic Null
>      hd <- newAtomic (Head {next = null })
>      hdPtr <- newAtomic hd
>      tailPtr <- newAtomic null
>      return (ListHandle {headList = hdPtr, tailList = tailPtr})

  
> find ::  Eq a => ListHandle a -> a -> IO Bool
> find (ListHandle {headList = ptrPtr})  i = 
>  atomically (
>          do ptr <- readTVar ptrPtr
>             Head {next = startptr} <- readTVar ptr
>             find2 startptr i)
>   where
>    find2 :: Eq a => TVar (List a) -> a -> STM Bool
>    find2 curNodePtr i = do
>    { curNode <- readTVar curNodePtr
>    ; case curNode of
>        Null -> return False  -- we've reached the end of the list
>                              -- element not found
>        Node {val = curval, next = curnext} ->
>          if (curval == i) then return True -- element found
>          else find2 curnext i              -- keep searching
>    }


> -- we add a new node, by overwriting the null tail node
> -- we only need to adjust tailList but not headList because
> -- of the static Head
> -- we return the location of the newly added node
> addToTail :: ListHandle a -> a -> IO (TVar (List a))
> addToTail (ListHandle {tailList = tailPtrPtr}) x =
>   do tPtr <- atomically (
>                do null <- newTVar Null
>                   tailPtr <- readTVar tailPtrPtr
>                   writeTVar tailPtr (Node {val = x, next = null})
>                   writeTVar tailPtrPtr null
>                   return tailPtr
>              )
>      return tPtr


> delete :: Eq a => ListHandle a -> a -> IO Bool
> delete (ListHandle {headList = ptrPtr})  i = 
>  atomically (
>          do startptr <- readTVar ptrPtr
>             delete2 startptr i)
>   where
>    delete2 :: Eq a => TVar (List a) -> a -> STM Bool
>    delete2 prevPtr i =
>     do
>    { prevNode <- readTVar prevPtr
>    ; let curNodePtr = next prevNode --  head/node have both next 
>    ; curNode <- readTVar curNodePtr
>    ; case curNode of
>        Null -> return False  -- we've reached the end of the list
>                              -- element not found
>        Node {val = curval, next = nextNode} ->
>          if (curval /= i) 
>           then delete2 curNodePtr i         -- keep searching
>           else 
>               -- delete element (ie delink node)
>               do { case prevNode of
>                      Head {} -> do writeTVar prevPtr (Head {next = nextNode})
>                                    return True
>                      Node {} -> do writeTVar prevPtr
>                                       (Node {val = val prevNode, next = nextNode})
>                                    return True
>                  }
>     }


printing and counting

> printList :: Show a => ListHandle a -> IO ()
> printList (ListHandle {headList = ptrPtr}) =
>  do startptr <- atomically (
>          do ptr <- readTVar ptrPtr
>             Head {next = startptr} <- readTVar ptr
>             return startptr)
>     printListHelp startptr


> printListHelp :: Show a => TVar (List a) -> IO ()
> printListHelp curNodePtr =
>   do { curNode <- atomically (readTVar curNodePtr)
>      ; case curNode of
>          Null -> putStr "Nil"
>          Node {val = curval, next = curnext} ->
>             do { putStr (show curval  ++ " -> ")
>                ;  printListHelp curnext }
>      } 

> cntList :: Show a => ListHandle a -> IO Int
> cntList (ListHandle {headList = ptrPtr}) =
>  do startptr <- atomically (
>          do ptr <- readTVar ptrPtr
>             Head {next = startptr} <- readTVar ptr
>             return startptr)
>     cntListHelp startptr 0

> cntListHelp :: Show a => TVar (List a) -> Int -> IO Int
> cntListHelp curNodePtr i =
>   do { curNode <- atomically (readTVar curNodePtr)
>      ; case curNode of
>          Null -> return i
>          Node {val = curval, next = curnext} ->
>             cntListHelp curnext (i+1)
>      }