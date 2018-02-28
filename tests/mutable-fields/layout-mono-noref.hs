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

type RefIOString# = Ref# RealWorld [Int]

readRefIOString :: RefIOString# -> IO [Int]
readRefIOString r = IO $ \s1# -> readRef# r s1#

main = do
    v <- newTVarIO 0 :: IO (TVar Int)
    m <- MkM [1,2,3] 0x7654321
    case m of
        MkM r b -> do
            print b
            atomically (writeTVar v 1)
            -- readRefIOString r >>= print
            atomically (writeTVar v 2)
