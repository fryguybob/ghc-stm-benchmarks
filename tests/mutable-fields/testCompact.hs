import Control.Monad

data T = MkT 
    String
    String
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int

main = do
    let ts = [MkT (show x) (show y) x y (x+y) (x*y) (x-y) (x*x) | x <- [1..1000], y <- [1..1000]]
    forM_ ts $ \(MkT a b c d e f g h) -> do
        print (a,b,c+d+e+f+g+h)
