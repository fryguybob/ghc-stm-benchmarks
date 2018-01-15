{-# LANGUAGE MutableFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import GHC.Types
import GHC.Prim
import GHC.IO
import GHC.Int

data M where
  MkM :: [Int] -> Int -> M

wrapMkM :: [Int] -> Int -> IO M
wrapMkM a b = IO (\s -> (# s, MkM a b #))
{-# NOINLINE wrapMkM #-}

main = do
    m <- wrapMkM [1,2,3] 0x7654321
    case m of
        MkM r b -> do
            print b
            print r
