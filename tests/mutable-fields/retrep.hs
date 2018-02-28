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

type RefIOListInt# = Ref# RealWorld [Int]

readRefIOListInt :: RefIOListInt# -> IO [Int]
readRefIOListInt r = IO $ \s1# -> readRef# r s1#

writeRefIOListInt :: RefIOListInt# -> [Int] -> IO ()
writeRefIOListInt r l = IO $ \s1# -> case writeRef# r l s1# of
                                        s2# -> (# s2#, () #)

{-# NOINLINE getList #-}
getList :: M -> RefIOListInt#
getList (MkM l _) = l

data X where
  MkX :: Int -> (# Int, Int #) -> X

{-# NOINLINE testUB #-}
testUB :: X -> (# Int, Int #)
testUB (MkX _ is) = is

main = do
    m <- MkM [1,2,3] 0x7654321
    let r = getList m
    writeRefIOListInt r [4,5,6]
    readRefIOListInt r >>= print

    let x = MkX 1 (# 2, 3 #)
        y = testUB x

    case y of
        (# a, b #) -> print (a,b)

