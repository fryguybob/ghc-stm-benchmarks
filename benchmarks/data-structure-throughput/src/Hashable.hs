module Hashable
    ( Hashable(..)
    ) where

import Data.Word
import Data.Bits

infixl 0 `hashWithSalt`

defaultSalt :: Int
defaultSalt = -2578643520546668380  -- 0xdc36d1615b7400a4

class Hashable a where
    hash :: a -> Int
    hashWithSalt :: Int -> a -> Int

    hash = hashWithSalt defaultSalt

defaultHashWithSalt :: Hashable a => Int -> a -> Int
defaultHashWithSalt salt x = salt `combine` hash x

combine :: Int -> Int -> Int
combine h1 h2 = (h1 * 16777619) `xor` h2

instance Hashable Word where
    hash = fromIntegral
    hashWithSalt = defaultHashWithSalt
