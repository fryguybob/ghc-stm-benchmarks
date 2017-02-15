#!/bin/bash

bc=-f-byteCounter
#bc=-fbyteCounter
other=

for n in stmtrie-tstruct-fine; do # HAMT

# ICFP 2017
# This is TStruct without HTM for IFL
# for n in stmtrie-fine stmtrie-tstruct-fine; do # HAMT
# for n in rbtree rbtree-tstruct-fine; do # RBTree
# for n in cuckoo-tvar-fine cuckoo-tstruct-fine cuckoo-tstruct-int-fine; do # Cuckoo
# for n in cuckoo-tvar-fine cuckoo-tvar-fine-simple cuckoo-tstruct-int-fine; do # Cuckoo
# for n in skiplist skiplist-tstruct-fine; do # SkipList

    echo "------------- $n -------------------"
    sb=.cabal-sandbox-$n-8
    d=-f$n

    if [ $n == "cuckoo-tstruct-fine" ] ; then
       flavor=mutable-fields
       d=-fcuckootstruct
    elif [ $n == "cuckoo-tstruct-int-fine" ] ; then
       flavor=mutable-fields
       d=-fcuckootstructint
    elif [ $n == "cuckoo-tvar-fine" ] ; then
       flavor=mutable-fields
       d=-fcuckootvar
    elif [ $n == "cuckoo-tvar-fine-simple" ] ; then
       flavor=mutable-fields
       d=-fcuckootvarsimple
    elif [ $n == "skiplist" ] ; then
       flavor=mutable-fields
    elif [ $n == "skiplist-tstruct" ] ; then
       flavor=htm-mut
       d=-fskiplisttstruct
    elif [ $n == "skiplist-tstruct-fine" ] ; then
       flavor=mutable-fields
       d=-fskiplisttstruct
    elif [ $n == "stmtrie-tstruct" ] ; then
       flavor=htm-mut
       d=-fstmtrietstruct
    elif [ $n == "stmtrie-fine" ] ; then
       flavor=mutable-fields
       d=-fstmtrie
    elif [ $n == "stmtrie-tstruct-fine" ] ; then
       flavor=mutable-fields
       d=-fstmtrietstruct
    elif [ $n == "tstruct-fine" ] ; then
       flavor=mutable-fields
       d=-ftstruct
    elif [ $n == "stmtrie-tstruct-fine-htm" ] ; then
       flavor=mutable-fields
       d=-fstmtrietstruct
    elif [ $n == "stmtrie-tstruct-allocs" ] ; then
       flavor=heap-allocs
       d=-fstmtrietstruct
    elif [ $n == "stmtrie-allocs" ] ; then
       flavor=heap-allocs
       d=-fstmtrie
    elif [ $n == "heap-allocs-notstruct" ] ; then
       flavor=heap-allocs
       d=
    elif [ $n == "stmtrie-htm" ] ; then
       flavor=htm-mut
       d=-fstmtrie
    elif [ $n == "ctrie" ] ; then
       flavor=no-invariants
       ghc=/localdisk/ryates/ghc-8/ghc-no-invariants-build/bin/ghc
    elif [ $n = "hashmapcas" ] || [ $n = "hashmaptvar" ] || [ $n = "hashmaptmvar" ] || [ $n = "hashmapmvar" ] || [ $n = "hashmap" ] ; then
       flavor=no-invariants
       ghc=/localdisk/ryates/ghc-8/ghc-no-invariants-build/bin/ghc
    elif [ $n = "rbtree" ] ; then
       flavor=mutable-fields
       d=-frbtree
    elif [ $n = "rbtree-tstruct-fine" ] ; then
       flavor=mutable-fields
       d=-frbtreetstruct
    else
       echo "Unknown option $n"
       exit 1
    fi

    ghc=/localdisk/ryates/ghc-8/build-$flavor/bin/ghc
    dump="-ddump-simpl -ddump-cmm -ddump-to-file -dumpdir dump/$n"

    # cabal install optparse-applicative stm MonadRandom ../throughput/ --with-ghc $ghc
    cabal sandbox init --sandbox=$sb

    if [ $n == "head" ] ; then
        # head builds need their local stm
        cabal install /localdisk/ryates/ghc-8/ghc-head/libraries/stm \
              $bc ../throughput/ ./ $d \
              --disable-executable-stripping --with-ghc $ghc
    elif [ $n == "stmtrie" ] || [ $n == "stmtrie-fine" ] || [ $n == "stmtrie-allocs" ] ; then
        cabal install $bc ../throughput/ ./ $d \
            stm-containers-0.2.9/ \
            --disable-executable-stripping --with-ghc $ghc --ghc-options="-O2 -msse4.2"
    else
        cabal install $bc ../throughput/ ./ $d \
            --disable-executable-stripping --with-ghc $ghc --ghc-options="-O2 -msse4.2"
    fi

    cp $sb/bin/data-structure-throughput bin/Main-$n-8

done
