{-# LANGUAGE MutableFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import GHC.Types
import GHC.Prim
import GHC.IO
import GHC.Int
import GHC.Conc

data M where
  MkM :: mutable [Int] -> Int -> IO M

type Mutable a = Ref# RealWorld a

readRef :: Mutable a -> IO a
readRef r = IO $ \s1# -> readRef# r s1#

writeRef :: Mutable a -> a -> IO ()
writeRef r l = IO $ \s1# -> case writeRef# r l s1# of
                              s2# -> (# s2#, () #)


_mList :: M -> Mutable [Int]
_mList (MkM l _) = l

{-# NOINLINE testA #-}
testA :: M -> IO ()
testA m = case m of
            MkM r b -> do
              print b
              writeRef r [4,5,6]
              readRef r >>= print

{-# NOINLINE testB #-}
testB :: M -> IO ()
testB m = do
  readRef (_mList m) >>= print
  writeRef (_mList m) [7,8,9]
  readRef (_mList m) >>= print
 

main = do
    m <- MkM [1,2,3] 0x7654321

    testA m
    testB m

