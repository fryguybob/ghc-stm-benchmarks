{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}
module CuckooHash
    ( Tree
    , mkTree
    
    , benchCode

    , insert
    , delete
    , update
    , get
    , contains

    , insertTest
    , deleteTest
    ) where

#ifdef TSTRUCT
-- import qualified CuckooHashTStruct as M
import qualified CuckooHashTStructInt as M
#define TSTRUCT_INT
#elif defined(TVAR)
-- import qualified CuckooHashTVar as M
import qualified CuckooHashTVarSimple as M
#else
#error Unsupported CuckooHash code variation
#endif
import GHC.Conc.Sync
import Control.Applicative
import Control.Monad (forM_)
import Data.Maybe
import Data.Word
import Data.List (inits,tails)


#if defined(TSTRUCT) && defined(TSTRUCT_INT)
type Tree = M.Table Word
#else
type Tree = M.Table Word Word
#endif
----------------------------------
-- Public API
--
mkTree :: STM Tree
mkTree = M.mkTable (floor $ 100000/2) 4 2

insert :: Tree -> Word -> Word -> STM Bool
insert t k v = M.insert t k v

delete :: Tree -> Word -> STM Bool
delete t k = M.remove t k

update :: Tree -> Word -> Word -> STM Bool
update t k v = M.insert t k v

get :: Tree -> Word -> STM (Maybe Word)
get t k = M.find t k

contains :: Tree -> Word -> STM Bool
contains t k = M.contains t k

benchCode :: String
benchCode = M.benchCode

insertTest :: [Word] -> IO ()
insertTest as = do
    r <- atomically $ mkTree
    forM_ (zip3 as (tail (inits as)) (tail (tails as))) $ \(a,is,ts) -> do
        atomically $ insert r a a

        forM_ is $ \i -> do
            m <- atomically $ get r i
            case m of
                Nothing -> print (i, "not found.")
                Just i' -> if i == i'
                             then return ()
                             else print (i, i', "Unmatched!")

        forM_ ts $ \i -> do
            m <- atomically $ get r i
            case m of
                Nothing -> return ()
                Just i' -> print (i, i', "Unexpected!")



deleteTest :: [Word] -> IO ()
deleteTest as = do
    r <- atomically $ mkTree
    forM_ as $ \a -> do
        atomically $ insert r a a

    forM_ (zip3 as (tail (inits as)) (tail (tails as))) $ \(a,is,ts) -> do
        atomically $ delete r a
        putStrLn $ "Deleting " ++ show a
        -- putStrLn $ "  out " ++ show is ++ " in " ++ show ts

        forM_ is $ \i -> do
            m <- atomically $ get r i
            case m of
                Nothing -> return ()
                Just i' -> print (i, i', "Unexpected!")

        forM_ ts $ \i -> do
            m <- atomically $ get r i
            case m of
                Nothing -> print (i, "not found.")
                Just i' -> if i == i'
                             then return ()
                             else print (i, i', "Unmatched!")
