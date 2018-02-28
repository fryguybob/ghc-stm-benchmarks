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
  MkM :: { _ms :: mutable [Int]
         , _m  :: Int
         } :: IO M

type RefIOListInt# = Ref# RealWorld [Int]

readRefIOListInt :: RefIOListInt# -> IO [Int]
readRefIOListInt r = IO $ \s1# -> readRef# r s1#

writeRefIOListInt :: RefIOListInt# -> [Int] -> IO ()
writeRefIOListInt r l = IO $ \s1# -> case writeRef# r l s1# of
                                        s2# -> (# s2#, () #)

main = do
    v <- newTVarIO 0 :: IO (TVar Int)
    m <- MkM [1,2,3] 0x7654321
    case m of
        MkM r b -> do
            print b
            writeRefIOListInt r [4,5,6]
            atomically (writeTVar v 1)
            readRefIOListInt r >>= print
            atomically (writeTVar v 2)

