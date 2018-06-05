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
  MkM :: mutable [Int] -> Int -> STM M

type TRef a = Ref# RealWorld a

readTRef :: TRef a -> STM a
readTRef r = STM $ \s1# -> readTRef# r s1#

writeTRef :: TRef a -> a -> STM ()
writeTRef r l = STM $ \s1# -> case writeTRef# r l s1# of
                                    s2# -> (# s2#, () #)

main = do
    m <- atomically $ MkM [0,1,2,3,4] 0x7654321
    x <- atomically
          $ case m of
              MkM r b -> do
                writeTRef r [4,5,6]
                l <- readTRef r
                return (l,b)
    print x

