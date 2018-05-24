{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}
module RBTreeCuckoo
    ( RBTree 
    , mkRBTree
    , Key
    
    , insert
    , delete
    , get

    , benchCode
    ) where

#ifdef CUCKOOTSTRUCTINT
import qualified CuckooHashInt as M
#elif CUCKOOTVAR
import qualified CuckooHashTVar as M
#elif CUCKOOTVARSIMPLE
import qualified CuckooHashTVarSimple as M
#else
import qualified CuckooHash as M
#endif
import Control.Applicative
import Data.IORef
import Data.Word
import Data.Maybe

import Control.Concurrent.STM


#ifdef CUCKOOTSTRUCTINT
type Key = Word
type RBTree = M.Table Word
#else
type Key = Word
type RBTree = M.Table Word Word
#endif
----------------------------------
-- Public API
--
mkRBTree :: STM RBTree
-- mkRBTree = M.mkTable 16 4 2
mkRBTree = M.mkTable (floor $ 100000/2) 4 2

insert :: RBTree -> Word -> Word -> STM Bool
insert t k v = M.insert t k v

delete :: RBTree -> Word -> STM Bool
delete t k = M.remove t k

update :: RBTree -> Word -> Word -> STM Bool
update t k v = M.insert t k v

get :: RBTree -> Word -> STM (Maybe Word)
get t k = M.find t k

contains :: RBTree -> Word -> STM Bool
contains t k = M.contains t k

benchCode :: String
benchCode = M.benchCode
