{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}
module RBTreeCTrie
    ( RBTree
    , mkRBTree
    
    , insert
    , delete
    , update
    , get
    , contains
    ) where

import qualified Control.Concurrent.Map as M
import Control.Applicative
import Data.Maybe
import Data.Word

type RBTree = M.Map Word Word
----------------------------------
-- Public API
--
mkRBTree :: IO RBTree
mkRBTree = M.empty

insert :: RBTree -> Word -> Word -> IO Bool
insert t k v = M.insert k v t >> return False

delete :: RBTree -> Word -> IO Bool
delete t k = M.delete k t >> return False

update :: RBTree -> Word -> Word -> IO Bool
update t k v = insert t k v

get :: RBTree -> Word -> IO (Maybe Word)
get t k = M.lookup k t

contains :: RBTree -> Word -> IO Bool
contains t k = isJust <$> get t k


