import Prelude hiding (lookup)

import Control.Monad
import Control.Applicative
import HAMTTStruct
import GHC.Conc

import System.Random
import Data.Word

import Data.Hashable

newtype Collision = Collision Int
    deriving (Show, Eq)

instance Hashable Collision where
  hash (Collision x) = x `mod` 10
  hashWithSalt _ = hash

main = do

    let g = mkStdGen 42
        rs = take 1000 (randomRs (0,1000000) g) :: [Word]

    a <- atomically new
    forM_ rs $ \i -> do
        atomically (insert i (show i) a)
        putStrLn ""
        atomically (showSTM a) >>= putStrLn

    forM_ rs $ \i -> do
        atomically (delete i a)
        putStrLn ""
        atomically (showSTM a) >>= putStrLn

main' = do

    let g = mkStdGen 42
        rs = map Collision $ take 1000 (randomRs (0,1000000) g) :: [Collision]

    a <- atomically new
    forM_ rs $ \i -> do
        atomically (insert i (show i) a)
        putStrLn ""
        atomically (showSTM a) >>= putStrLn

    forM_ rs $ \i -> do
        atomically (delete i a)
        putStrLn ""
        atomically (showSTM a) >>= putStrLn
 
{-
    vs <- atomically (showSTM a)
    print rs
    putStrLn vs
    s <- atomically (lookup 617539 a)
    print s

    atomically (insert 617539 "new" a)

    vs' <- atomically (showSTM a)

    putStrLn vs'
    putStrLn "Done."
-}
