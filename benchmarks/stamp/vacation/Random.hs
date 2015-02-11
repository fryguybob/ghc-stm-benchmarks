{-# LANGUAGE FlexibleContexts #-}
module Random
    ( Random
    , initRandom
    , getRandom
    ) where

import Data.Bits
import Data.Word
import Data.Array
import Data.Array.IO
import Data.IORef

import Control.Applicative
import Control.Monad

periodN, periodM, matrixA, upperMask, lowerMask :: Word32
periodN   = 624
periodM   = 397
matrixA   = 0x9908b0df
upperMask = 0x80000000
lowerMask = 0x7fffffff

data Random = Random
    { _mt  :: IOUArray Word32 Word32
    , _mti :: IORef Word32
    }

initRandom :: Word32 ->  IO Random
initRandom s = do
    mt <- newArray (0,periodN-1) 0
    mti <- newIORef periodN
    let r = Random mt mti
    initGenRand r s -- init_genrand
    return r

initGenRand :: Random -> Word32 -> IO ()
initGenRand (Random mt mti) s = do
    writeArray mt 0 (s .&. 0xffffffff)
    forM_ [1..periodN-1] $ \i -> do
        p <- readArray mt (i - 1)
        writeArray mt i ((1812433253 * (p `xor` (p `shiftR` 30)) + i) .&. 0xffffffff) 
    writeIORef mti periodN

getRandom :: Random -> IO Word32
getRandom r@(Random mt mti) = do
    i <- readIORef mti

    when (i >= periodN) (reinit i)

    i <- readIORef mti
    writeIORef mti (i+1)
    y <- readArray mt i

    return . f 18 . g 15 0xefc60000 . g 7 0x9d2c5680 . f 11 $ y

  where
    f n   y = y `xor` (y `shiftR` n)
    g n m y = y `xor` ((y `shiftL` n) .&. m)

    reinit i = do
        when (i == periodN + 1) $ initGenRand r 5489
        forM_ [0..periodN-periodM-1]       $ step (+1) (+periodM)
        forM_ [periodN-periodM..periodN-2] $ step (+1) (+(periodM-periodN))
        step (const 0) (const (periodM-1)) (periodN-1)

        writeIORef mti 0
      where
        step fb fc k = do
          a <- readArray mt k
          b <- readArray mt (fb k)
          let y = (a .&. upperMask) .|. (b .&. lowerMask)
          c <- readArray mt (fc k)
          writeArray mt k (c `xor` (y `shiftR` 1) 
                             `xor` mag (y .&. 1))
    
    mag 0 = 0
    mag 1 = matrixA
