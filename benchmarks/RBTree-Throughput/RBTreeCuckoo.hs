{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}
module RBTreeCuckoo
    ( RBTree 
    , mkRBTree
    , Key
    
    , insert
    , delete
    , get
    ) where

-- import qualified CuckooHash as M
import qualified CuckooHashInt as M
import Control.Applicative
import Data.IORef
import Data.Word
import Data.Maybe

import Control.Concurrent.STM

type Key = Word

type RBTree = M.Table Word
-- type RBTree = M.Table Word Word
----------------------------------
-- Public API
--
mkRBTree :: STM RBTree
mkRBTree = M.mkTable 16 4 2

insert :: RBTree -> Word -> Word -> STM Bool
insert t k v = M.insert t k v

delete :: RBTree -> Word -> STM Bool
delete t k = M.remove t k

update :: RBTree -> Word -> Word -> STM Bool
update t k v = M.insert t k v

get :: RBTree -> Word -> STM (Maybe Word)
get t k = M.find t k

contains :: RBTree -> Word -> STM Bool
contains t k = isJust <$> get t k

