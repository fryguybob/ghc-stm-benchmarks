{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}
module RBTreeSTMTrie
    ( RBTree
    , mkRBTree
    
    , insert
    , delete
    , update
    , get
    , contains
    ) where

import qualified STMContainers.Map as M
import Control.Concurrent.STM
import Control.Applicative
import Data.Maybe
import Data.Word

type RBTree = M.Map Word Word
----------------------------------
-- Public API
--
mkRBTree :: STM RBTree
mkRBTree = M.new

insert :: RBTree -> Word -> Word -> STM Bool
insert t k v = M.insert v k t >> return False

delete :: RBTree -> Word -> STM Bool
delete t k = M.delete k t >> return False

update :: RBTree -> Word -> Word -> STM Bool
update t k v = insert t k v

get :: RBTree -> Word -> STM (Maybe Word)
get t k = M.lookup k t

contains :: RBTree -> Word -> STM Bool
contains t k = isJust <$> get t k


