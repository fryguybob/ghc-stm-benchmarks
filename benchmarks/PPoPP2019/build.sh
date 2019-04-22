#!/bin/bash

set -e

if [[ -z $GHC_COMPILERS ]]; then
    GHC_COMPILERS=$HOME/ghc-8
fi

# Thesis:
# ------------------
# tvar-hybrid,         cuckoo,   hybrid, tvar,    (2,t)
# tsturct-hybrid,      cuckoo,   hybrid, tstruct, (2,t)
# tvar-fine-hybrid,    cuckoo,   fine,   tvar,    (t,t)
# tsturct-fine-hybrid, cuckoo,   fine,   tstruct, (t,t)
# tvar-fine,           cuckoo,   fine,   tvar,    (0,0)
# tsturct-fine,        cuckoo,   fine,   tstruct, (0,0)

# tvar-hybrid,         skiplist, hybrid, tvar,    (2,t)
# tsturct-hybrid,      skiplist, hybrid, tstruct, (2,t)
# tvar-fine-hybrid,    skiplist, fine,   tvar,    (t,t)
# tsturct-fine-hybrid, skiplist, fine,   tstruct, (t,t)
# tvar-fine,           skiplist, fine,   tvar,    (0,0)
# tsturct-fine,        skiplist, fine,   tstruct, (0,0)

# Figure, Label               , benchmark, compiler     , code    , htm options
# -------------------------------------------------------------------------------
#  1(a) , mut-hybrid          , rbtree   , hybrid       , mut     , (2,t)
#  1(a) , tvar-hybrid         , rbtree   , hybrid       , tvar    , (2,t)
#  1(a) , tstruct-hybrid      , rbtree   , hybrid       , tstruct , (2,t)
#  1(a) , mut-fine-hybrid     , rbtree   , fine         , mut     , (t,t)
#  1(a) , tvar-fine-hybrid    , rbtree   , fine         , tvar    , (t,t)
#  1(a) , tstruct-fine-hybrid , rbtree   , fine         , tstruct , (t,t)
#  1(a) , mut-fine            , rbtree   , fine         , mut     , (0,0)
#  1(a) , tvar-fine           , rbtree   , fine         , tvar    , (0,0)
#  1(a) , tstruct-fine        , rbtree   , fine         , tstruct , (0,0)

#  1(b) , mut-hybrid          , hamt     , hybrid       , mut     , (2,t)
#  1(b) , tvar-hybrid         , hamt     , hybrid       , tvar    , (2,t)
#  1(b) , tstruct-hybrid      , hamt     , hybrid       , tstruct , (2,t)
#  1(b) , mut-fine-hybrid     , hamt     , fine         , mut     , (t,t)
#  1(b) , tvar-fine           , hamt     , fine         , tvar    , (t,t)
#  1(b) , tstruct-fine-hybrid , hamt     , fine         , tstruct , (t,t)
#  1(b) , mut-fine            , hamt     , fine         , mut     , (0,0)
#  1(b) , tvar-fine           , hamt     , fine         , tvar    , (0,0)
#  1(b) , tstruct-fine        , hamt     , fine         , tstruct , (0,0)

#  1(c) , mut-hybrid          , treap    , hybrid       , mut     , (2,t)
#  1(c) , tvar-hybrid         , treap    , hybrid       , tvar    , (2,t)
#  1(c) , mut-fine-hybrid     , treap    , fine         , mut     , (t,t)
#  1(c) , tvar-fine           , treap    , fine         , tvar    , (t,t)
#  1(c) , mut-fine            , treap    , fine         , mut     , (0,0)
#  1(c) , tvar-fine           , treap    , fine         , tvar    , (0,0)

#  2(a) , mut-hard            , treap    , hybrid       , mut     , (2,t)
#  2(a) , tvar-hard           , treap    , hybrid       , tvar    , (2,t)
#  2(a) , mut-fall            , treap    , hybrid       , mut     , (2,t)
#  2(a) , tvar-fall           , treap    , hybrid       , tvar    , (2,t)
#  2(a) , mut-lock            , treap    , hybrid       , mut     , (2,t)
#  2(a) , tvar-lock           , treap    , hybrid       , tvar    , (2,t)

#  2(b) , hamt-late           , hamt     , hybrid       , mut     , (2,t)
#  2(b) , hamt-early          , hamt     , hybrid-early , mut     , (2,t)
#  2(b) , rbtree-late         , rbtree   , hybrid       , mut     , (2,t)
#  2(b) , rbtree-early        , rbtree   , hybrid-early , mut     , (2,t)
#  2(b) , treap-late          , treap    , hybrid       , mut     , (2,t)
#  2(b) , treap-early         , treap    , hybrid-early , mut     , (2,t)

#  2(c) , (2,t)               , treap    , hybrid       , mut     , (2,t)
#  2(c) , (t,t)               , treap    , hybrid       , mut     , (t,t)
#  2(c) , (2,2)               , treap    , hybrid       , mut     , (2,2)
#  2(c) , (2,0)               , treap    , hybrid       , mut     , (2,0)

# Compiler versions:
#  hybrid -- supports:
#       - full hybrid (2,t), (t,t), (2,2)
#       - lock-elision on fallback lock (0,t), (0,2)
#       - coarse-grain STM (0,0)
#       - late lock subscription
#  hybrid-lock -- Same as hybrid, but with early lock subscription.
#  fine   -- supports:
#       - fine hybrid (t,t) (htm used for the commit of the fine-grain TM)
#       - fine-grain STM (0,0) (equivalant to main branch of GHC when used with TVar code)
#
#  All versions support mutable-fields extensions.

# Code variations:
#  mut     -- Mutable fields.
#  tvar    -- TVar-based  (equivalant to main branch of GHC).
#  tstruct -- TStruct-based code.

deps="-isrc/focus-0.1.5.2/library"
deps="$deps -isrc/hashable-1.2.6.1"
deps="$deps -isrc/base-prelude-1.2.0.1/library"
deps="$deps -isrc/loch-th-0.2.1"
deps="$deps -isrc/primitive-0.6.3.0"
deps="$deps -isrc/transformers-base-0.4.4/src"
deps="$deps -isrc/transformers-compat-0.5.1.4/src"
deps="$deps -isrc/entropy-0.4"

flags=""
flags="$flags -XBangPatterns"
flags="$flags -XConstraintKinds"
flags="$flags -XDataKinds"
flags="$flags -XDefaultSignatures"
flags="$flags -XDeriveDataTypeable"
flags="$flags -XDeriveFunctor"
flags="$flags -XDeriveGeneric"
flags="$flags -XEmptyDataDecls"
flags="$flags -XFlexibleContexts"
flags="$flags -XFlexibleInstances"
flags="$flags -XFunctionalDependencies"
flags="$flags -XGADTs"
flags="$flags -XGeneralizedNewtypeDeriving"
flags="$flags -XLambdaCase"
flags="$flags -XMultiParamTypeClasses"
flags="$flags -XMultiWayIf"
flags="$flags -XNoMonomorphismRestriction"
flags="$flags -XPatternGuards"
flags="$flags -XPolyKinds"
flags="$flags -XQuasiQuotes"
flags="$flags -XRankNTypes"
flags="$flags -XStandaloneDeriving"
flags="$flags -XTemplateHaskell"
flags="$flags -XTupleSections"
flags="$flags -XTypeFamilies"
flags="$flags -XTypeOperators"

cbits="src/cbits/gettime.c"
cbits="$cbits src/hashable-1.2.6.1/cbits/fnv.c"
cbits="$cbits src/entropy-0.4/cbits/rdrand.c"
cbits="$cbits src/primitive-0.6.3.0/cbits/primitive-memops.c"

for compiler in hybrid hybrid-early fine; do
  ghc=$GHC_COMPILERS/$compiler/bin/ghc
  for bench in RBTREE HAMT TREAP CUCKOO SKIPLIST; do
    for code in MUT TVAR TSTRUCT; do
      if [ "$bench" == "RBTREE" ]; then
        a=""
      elif [ "$bench" == "HAMT"   ]; then
        a=""
      elif [ "$bench" == "TREAP"  ]; then
        if [ "$code" == "TSTRUCT" ]; then
            continue
        fi
      elif [ "$bench" == "CUCKOO" ]; then
        if [ "$code" == "MUT" ]; then
            continue
        fi
      elif [ "$bench" == "SKIPLIST" ]; then
        if [ "$code" == "MUT" ]; then
            continue
        fi
      fi
      
      n=$bench-$code-$compiler
      opts="-O2 -rtsopts -threaded -fno-omit-yields -msse4.2 $flags"
      buildopts="-no-user-package-db -outputdir .build-$n -isrc $deps"
      cppflags="-DBYTECOUNTER -D$bench -D$code"
      ccflags="--optc='-O2'"

      echo $ghc src/Main.hs -o bin/$n $cppflags
      $ghc src/Main.hs $cbits $opts $buildopts -o bin/$n $cppflags
    done
  done
done
