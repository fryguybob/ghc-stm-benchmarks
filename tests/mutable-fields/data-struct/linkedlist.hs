{-# LANGUAGE MutableFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import GHC.Types
import GHC.Prim
import GHC.IO
import GHC.Int
import GHC.Conc

import Control.Monad (forM_)

data Node a where
    MkNode :: a -> mutable (Node a) -> IO (Node a)
    Nil :: Node a

data LinkedList a where
    MkList :: mutable (Node a) -> IO (LinkedList a)

{-# NOINLINE wQQQ #-}
wQQQ :: Node a -> IO (LinkedList a)
wQQQ a = MkList a

type RefIONode a = Ref# RealWorld (Node a)

readRefIONode :: RefIONode a -> IO (Node a)
readRefIONode r = IO $ \s1# -> readRef# r s1#

writeRefIONode :: RefIONode a -> Node a -> IO ()
writeRefIONode r l = IO $ \s1# -> case writeRef# r l s1# of
                                        s2# -> (# s2#, () #)

push :: a -> LinkedList a -> IO ()
push a (MkList r) = do
    n <- readRefIONode r
    h <- MkNode a n
    writeRefIONode r h

append :: a -> LinkedList a -> IO ()
append a (MkList r) = appendNode a r

appendNode :: a -> RefIONode a -> IO ()
appendNode a r = do
   n <- readRefIONode r
   case n of
      Nil -> do
        t <- MkNode a Nil
        writeRefIONode r t
      MkNode _ r' -> appendNode a r'

forEach :: LinkedList a -> (a -> IO ()) -> IO ()
forEach (MkList r) act = readRefIONode r >>= go
  where
    go Nil = return ()
    go (MkNode a r) = do
        act a
        readRefIONode r >>= go
        
main = do
    l <- wQQQ Nil -- MkList Nil

    forM_ [1..100000] $ \a -> do
        push a l

    forM_ [1..100] $ \a -> do
        append a l

    forEach l (putStr . show)
    putStrLn ""
