{-# LANGUAGE CPP #-}
module RBTreeSkipList
    ( RBTree
    , mkRBTree
    
    , insert
    , delete
    , update
    , get
    , contains
    ) where

import Control.Concurrent.STM
import Control.Applicative
import Data.Word
import Data.Maybe (isJust)

#ifdef SKIPLIST_TSTRUCT
import qualified SkipListTStruct as S
#else
import qualified SkipList as S
#endif

type RBTree = S.SkipList

----------------------------------
-- Public API
--
mkRBTree :: STM RBTree
mkRBTree = S.new 32

insert :: RBTree -> Word -> Word -> STM Bool
insert t k v = S.insert t k v >> return False

delete :: RBTree -> Word -> STM Bool
delete = S.delete

get :: RBTree -> Word -> STM (Maybe Word)
get = S.get

update :: RBTree -> Word -> Word -> STM Bool
update t k v = S.insert t k v >> return False

contains :: RBTree -> Word -> STM Bool
contains t k = isJust <$> get t k
