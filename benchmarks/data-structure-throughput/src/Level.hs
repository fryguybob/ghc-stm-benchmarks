module Level where

import Prelude hiding (mask)

import Data.Bits


-- |
-- A depth level of a node.
-- Must be a multiple of the 'step' value.
type Level = Int

{-# INLINE hashIndex #-}
hashIndex :: Level -> (Int -> Int)
hashIndex l i = mask .&. unsafeShiftR i l

{-# INLINE mask #-}
mask :: Int
mask = bit step - 1

{-# INLINE step #-}
step :: Int
step = 5

{-# INLINE limit #-}
limit :: Int
limit = finiteBitSize (undefined :: Int)

{-# INLINE succ #-}
succ :: Level -> Level
succ = (+ step)
