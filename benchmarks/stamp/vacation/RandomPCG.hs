{-# LANGUAGE FlexibleContexts #-}
module RandomPCG
    ( Random
    , initRandom
    , getRandom
    ) where

import Control.Applicative
import Control.Monad

import Data.Word

import System.Random.PCG.Fast.Pure

type T = Word32

type Random = GenIO

initRandom :: T -> IO Random
initRandom s = initialize (fromIntegral s)

getRandom :: Random -> IO T
getRandom g = uniform g
