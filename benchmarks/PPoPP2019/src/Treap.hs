{-# LANGUAGE CPP #-}
module Treap
    ( Tree
    , mkTree
    
    , insert
    , delete
    , update
    , get
    , contains

    , benchCode
    ) where

import Control.Applicative
import Data.Word
import Data.Maybe (isJust)

import GHC.Conc

#if defined(MUT)
import qualified TreapMutSTM as T
#define M STM
#elif defined(TVAR)
import qualified TreapTVar as T
#define M STM
#else
#error Unknown Treap Variant
#endif

benchCode :: String
benchCode = T.benchCode

type Tree = T.Treap

----------------------------------
-- Public API
--
mkTree :: M Tree
mkTree = T.mkTreap

-- #define TESTCODE
#ifdef TESTCODE
insert :: Tree -> Word -> Word -> M Bool
insert = T.insertV

delete :: Tree -> Word -> M Bool
delete = T.deleteV
#else
insert :: Tree -> Word -> Word -> M Bool
insert = T.insert

delete :: Tree -> Word -> M Bool
delete = T.delete
#endif

get :: Tree -> Word -> M (Maybe Word)
get = T.get

update :: Tree -> Word -> Word -> M Bool
update t k v = T.insert t k v >> return False

contains :: Tree -> Word -> M Bool
contains t k = T.contains t k
