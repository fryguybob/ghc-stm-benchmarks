{-# LANGUAGE CPP #-}
module RBTree
    ( Tree
    , mkTree

    , insert
    , delete
    , update
    , get
    , contains

    , benchCode
    ) where

import GHC.Conc.Sync
import Data.Word

#if defined(TSTRUCT)
import RBTreeTStruct
type Tree = RBTree
#elif defined(MUT)
import RBTreeMutUSTM
type Tree = RBTree Word Word
#elif defined(TVAR)
import RBTreeTVar
type Tree = RBTree Word Word
#else
#error Unknown RBTree Variant
#endif


mkTree :: STM Tree
mkTree = mkRBTree
