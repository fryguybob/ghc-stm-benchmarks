{-# LANGUAGE FlexibleContexts #-}
module RandomMWC
    ( Random
    , initRandom
    , getRandom
    ) where

import Control.Applicative
import Control.Monad

import Data.Word

import System.Random.MWC
import qualified Data.Vector as V

type T = Word32

type Random = GenIO

initRandom :: T ->  IO Random
initRandom s = initialize (V.singleton . fromIntegral $ s)

getRandom :: Random -> IO T
getRandom g = uniform g
