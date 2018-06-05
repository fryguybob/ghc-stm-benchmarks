{-# LANGUAGE MutableFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import GHC.Types
import GHC.Prim
import GHC.IO
import GHC.Int
import GHC.Conc

initMutConSTM# :: a -> State# RealWorld -> (# State# RealWorld, a #)
initMutConSTM# a = \s# -> (# s#, a #)
{-# NOINLINE initMutConSTM# #-}

data M = MkM [Int] Int

type TRef a = Ref# RealWorld a

readTRef :: TRef a -> STM a
readTRef r = STM $ \s1# -> readTRef# r s1#

writeTRef :: TRef a -> a -> STM ()
writeTRef r l = STM $ \s1# -> case writeTRef# r l s1# of
                                    s2# -> (# s2#, () #)

{-# NOINLINE mWithInit #-}
mWithInit a b = STM $ \s1# -> initMutConSTM# (MkM a b) s1#

main = do
    m <- atomically $ mWithInit [0,1,2,3,4] 0x7654321
    x <- atomically
          $ case m of
              MkM r b -> do
                -- writeTRef r [4,5,6]
                -- l <- readTRef r
                return (r,b)
    print x

