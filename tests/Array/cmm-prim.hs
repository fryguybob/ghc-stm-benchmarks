import StgCmmPrim
import StgCmmMonad

import Cmm
import CmmInfo
import CmmExpr
import CmmType

import Unique

import PrimOp
import StgSyn

import PprCmm
import Outputable
import DynFlags

import GHC
import Literal
import SysTools
import MkGraph
import BlockId


main = do
    putStrLn "Hello"

    (f,cmm) <- buildCmm

    putStrLn (showSDoc f (ppr cmm))

buildCmm = do
    mySettings <- initSysTools 
        (Just "/localdisk/ryates/ghc-7.10/ghc-htm-mut-build/lib/ghc-7.9.20150220")
                -- ^^^ ghc --print-libdir
    f' <- initDynFlags (defaultDynFlags mySettings) -- Opt_D_dump_cmm
    let f = dopt_set (dopt_set f' Opt_D_dump_cmm) Opt_D_dump_cmm_raw

    s <- initC
    let ((cmm, u),_) = runC f m s $ do
            c <- code 1000 10000
            u <- newUnique
            return (c, u)

    return (f, labelAGraph (mkBlockId u) cmm)



  where
    m = undefined

code a b = do
    u <- newUnique
    let r = LocalReg u b64
--    let m = mkLocalId "m" b64
--        i = mkLocalId "i" b64
--    getCmm (cgPrimOp [r] ReadSTMArrayWordOp [StgVarArg m, StgVarArg i])
--    getCmm (cgPrimOp [r] ReadSTMArrayWordOp [StgLitArg (MachWord 42), StgLitArg (MachWord 13)])
--    getCmm (cgPrimOp [r] SizeofSmallArrayOp [StgLitArg (MachWord 42)])

    getCode (cgPrimOp [r] ReadSTMArrayWordOp [StgLitArg (MachWord a), StgLitArg (MachWord b)])
