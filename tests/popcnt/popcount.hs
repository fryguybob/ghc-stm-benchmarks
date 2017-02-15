import Data.Bits
import Control.Monad

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- 00010100101
-- 00000011111
-- ----------- &
-- 00000000101

position i b = popCount (b .&. (bit i - 1) :: Int)

main = print $ flip map [i | i <- [1..10], j <- [1..10], k <- [1..10]] $ \n -> position (fib n :: Int) 6

-- main = print (popCount (fib 20 :: Int))

-- main = print (popCount (1010 :: Int))