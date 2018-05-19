{-# LANGUAGE CPP #-}
module RBTreeTreap
    ( RBTree
    , mkRBTree
    
    , insert
    , delete
    , update
    , get
    , contains
    ) where

import Control.Applicative
import Data.Word
import Data.Maybe (isJust)

import GHC.Conc

#ifdef TREAP_MUT_SINGLE
import qualified TreapMut as T
#define M IO
#elif defined(TREAP_IOREF)
import qualified TreapIORef as T
#define M IO
#elif defined(TREAP_MUT_STM_REF)
import qualified TreapMutSTMRef as T
#define M STM
#elif defined(TREAP_MUT_STM_CPS)
import qualified TreapMutSTMCPS as T
#define M STM
#elif defined(TREAP_MUT_STM)
import qualified TreapMutSTM as T
#define M STM
#elif defined(TREAP_TVAR)
import qualified TreapTVar as T
#define M STM
#elif defined(TREAP_TSTRUCT)
import qualified TreapTStruct as T
#define M STM
#else
#error Unknown Treap Variant
#endif

type RBTree = T.Treap

----------------------------------
-- Public API
--
mkRBTree :: M RBTree
mkRBTree = T.mkTreap

-- #define TESTCODE
#ifdef TESTCODE
insert :: RBTree -> Word -> Word -> M Bool
insert = T.insertV

delete :: RBTree -> Word -> M Bool
delete = T.deleteV
#else
insert :: RBTree -> Word -> Word -> M Bool
insert = T.insert

delete :: RBTree -> Word -> M Bool
delete = T.delete
#endif

get :: RBTree -> Word -> M (Maybe Word)
get = T.get

update :: RBTree -> Word -> Word -> M Bool
update t k v = T.insert t k v >> return False

contains :: RBTree -> Word -> M Bool
contains t k = T.contains t k
