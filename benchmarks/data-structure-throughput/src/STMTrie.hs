{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}
module STMTrie
    ( BenchTree
    , mkBenchTree
    
    , insert
    , delete
    , update
    , get
    , contains
#ifdef TESTCODE
    , insertTest
    , deleteTest
#endif
    ) where

#ifdef TSTRUCT
import qualified HAMTTStruct as M
#else
import qualified STMContainers.Map as M
#endif
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad (forM_)
import Data.Maybe
import Data.Word
import Data.List (inits,tails)

type BenchTree = M.Map Word Word
----------------------------------
-- Public API
--
mkBenchTree :: STM BenchTree
mkBenchTree = M.new

insert :: BenchTree -> Word -> Word -> STM Bool
insert t k v = M.insert v k t >> return False

delete :: BenchTree -> Word -> STM Bool
delete t k = M.delete k t >> return False

update :: BenchTree -> Word -> Word -> STM Bool
update t k v = insert t k v

get :: BenchTree -> Word -> STM (Maybe Word)
get t k = M.lookup k t

contains :: BenchTree -> Word -> STM Bool
contains t k = isJust <$> get t k


#ifdef TESTCODE
insertTest :: [Word] -> IO ()
insertTest as = do
    r <- atomically $ mkBenchTree
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
    r <- atomically $ mkBenchTree
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

#endif
