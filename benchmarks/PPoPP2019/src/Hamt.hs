{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}
module Hamt
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
import qualified HamtTStruct as M
#elif defined(MUT)
import qualified HamtTRef as M
#elif defined(TVAR)
import qualified STMContainers.Map as M
#else
#error Unsupported HAMT code variation
#endif
import GHC.Conc.Sync
import Control.Applicative
import Control.Monad (forM_)
import Data.Maybe
import Data.Word
import Data.List (inits,tails)

#ifdef TSTRUCT
benchCode :: String
benchCode = "HAMTTStruct"
#elif defined(MUT)
benchCode :: String
benchCode = "HAMTTRef"
#else
benchCode :: String
benchCode = "STMTrieTVar"
#endif

type Tree = M.Map Word Word
----------------------------------
-- Public API
--
mkTree :: STM Tree
mkTree = M.new

insert :: Tree -> Word -> Word -> STM Bool
#if defined(TSTRUCT) || defined(MUT)
insert t k v = M.insert k v t >> return False
#else
insert t k v = M.insert v k t >> return False
#endif

delete :: Tree -> Word -> STM Bool
delete t k = M.delete k t >> return False

update :: Tree -> Word -> Word -> STM Bool
update t k v = insert t k v

get :: Tree -> Word -> STM (Maybe Word)
get t k = M.lookup k t

contains :: Tree -> Word -> STM Bool
contains t k = isJust <$> get t k

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
