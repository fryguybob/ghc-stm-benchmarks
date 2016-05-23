{-# LANGUAGE MagicHash #-}

import Data.Bits
import GHC.Prim
import GHC.Base


roundUp64 :: Int -> Int
roundUp64 i = (i + 63) .&. (-64)

roundUp64# :: Int# -> Int#
roundUp64# i# = (i# +# 63#) `andI#` (-64#)

roundUp64' :: Int -> Int
roundUp64' (I# i#) = I# (roundUp64# i#)
