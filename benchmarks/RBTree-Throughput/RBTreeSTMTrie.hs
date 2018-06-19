{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}
module RBTreeSTMTrie
    ( RBTree
    , mkRBTree
    
    , benchCode

    , insert
    , delete
    , update
    , get
    , contains

    , insertTest
    , deleteTest
    ) where

#ifdef STMTRIE_TSTRUCT
import qualified HAMTTStruct as M
#elif defined(STMTRIE_MUT)
import qualified HAMTTRef as M
#else
import qualified STMContainers.Map as M
#endif
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad (forM_)
import Data.Maybe
import Data.Word
import Data.List (inits,tails)

#ifdef STMTRIE_TSTRUCT
benchCode :: String
benchCode = "HAMTTStruct"
#elif defined(STMTRIE_MUT)
benchCode :: String
benchCode = "HAMTTRef"
#else
benchCode :: String
benchCode = "STMTrieTVar"
#endif


type RBTree = M.Map Word Word
----------------------------------
-- Public API
--
mkRBTree :: STM RBTree
mkRBTree = M.new

insert :: RBTree -> Word -> Word -> STM Bool
#if defined(STMTRIE_TSTRUCT) || defined(STMTRIE_MUT)
insert t k v = M.insert k v t >> return False
#else
insert t k v = M.insert v k t >> return False
#endif

delete :: RBTree -> Word -> STM Bool
delete t k = M.delete k t >> return False

update :: RBTree -> Word -> Word -> STM Bool
update t k v = insert t k v

get :: RBTree -> Word -> STM (Maybe Word)
get t k = M.lookup k t

contains :: RBTree -> Word -> STM Bool
contains t k = isJust <$> get t k

insertTest :: [Word] -> IO ()
insertTest as = do
    r <- atomically $ mkRBTree
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
    r <- atomically $ mkRBTree
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


