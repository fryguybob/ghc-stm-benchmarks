{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction #-}
import Data.Word
import Numeric
import Data.Bits

import System.Environment

(.<<.), (.>>.) :: Word64 -> Word64 -> Word64
a .<<. (fromIntegral -> b) = shiftL a b
a .>>. (fromIntegral -> b) = shiftR a b

hash1,hash2,hash3,hash :: Word64 -> Word64

-- hash1 (fromIntegral -> w) = 1 .<<. ((w .&. 0x00003f0) .>>.  4)
-- hash2 (fromIntegral -> w) = 1 .<<. ((w .&. 0x003f000) .>>. 12)
-- hash3 (fromIntegral -> w) = 1 .<<. ((w .&. 0x3f00000) .>>. 20)

hash1 w = 1 .<<. ((w .&. 0x00003f0) .>>.  4)
hash2 w = 1 .<<. ((w .&. 0x003f000) .>>. 12)
hash3 w = 1 .<<. ((w .&. 0x3f00000) .>>. 20)

hash  w = hash1 w .|. hash2 w .|. hash3 w

printBin x = putStrLn . padLeft '.' 64 $ showIntAtBase 2 (".1"!!) x ""

padLeft c n s = go (n - length s) s
 where
   go n s
    | n <= 0    = s
    | otherwise = go (n-1) (c:s)

readHex' x = case readHex x of
               [(v,"")] -> v
               _        -> error $ "Failed to parse \"" ++ x ++ "\""

main = do
    [read -> v] <- getArgs

    printBin (hash1 v)
    printBin (hash2 v)
    printBin (hash3 v)
    putStrLn "" -- (replicate 64 '-')
    printBin (hash  v)
