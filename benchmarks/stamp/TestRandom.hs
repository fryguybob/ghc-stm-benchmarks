import Random
import Control.Monad

main = do
    r <- initRandom 0

    forM_ [0..1000-1] $ \_ -> do
        i <- getRandom r
        print i
