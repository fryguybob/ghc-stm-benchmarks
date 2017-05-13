#!/bin/bash

bc=-f-byteCounter
#bc=-fbyteCounter
other=

# HASKELL 2017
# This is TStruct without HTM originally for IFL, now for HASKELL 2017
# for n in stmtrie-fine stmtrie-tstruct-fine; do # HAMT
for n in no-invariants tstruct-fine; do # RBTree
# for n in cuckoo-tvar-fine cuckoo-tstruct-fine cuckoo-tstruct-int-fine; do # Cuckoo
# for n in skiplist skiplist-tstruct-fine; do # SkipList

# for n in skiplist skiplist-tstruct skiplist-tstruct-fine; do
# for n in stmtrie-tstruct stmtrie-fine stmtrie-tstruct-fine stmtrie-tstruct-fine-htm; do # for HASKELL
# for n in no-invariants tstruct-fine; do # for HASKELL2016
# for n in tstruct-fine; do # for HASKELL2016
# for n in stmtrie-tstruct stmtrie-fine stmtrie-htm; do
# for n in  stmtrie-fine; do
# for n in stmtrie-tstruct stmtrie-fine stmtrie-htm stmtrie-tstruct-allocs stmtrie-allocs; do
# for n in stmtrie-tstruct stmtrie-tstruct-fine; do
# for n in stmtrie-tstruct-allocs stmtrie-allocs; do
# for n in pastm-tl2 pastm-fine head; do
# for n in heap-allocs heap-allocs-notstruct stmtrie-allocs; do
# for n in stmtrie-fine stmtrie-htm; do
# for n in htm-mut no-invariants; do
# for n in htm-mut; do
# for n in head 7.10.1 pastm-tl2 pastm-fine; do
# for n in cuckoo stmtrie-fine stmtrie-htm no-invariants coarse htm-bloom fine-hle htm-mut htm-mut-fine ctrie pastm-tl2; do
# for n in hashmap hashmapcas hashmaptvar hashmaptmvar hashmapmvar; do
# for n in cuckoo stmtrie-fine stmtrie-htm; do
# for n in no-invariants coarse htm-bloom fine-hle htm-mut; do
# for n in htm-mut; do
# for n in htm-bloom; do
# for n in fine-hle; do
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
       d=-fskiplist
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
#       flavor=no-invariants
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
    elif [ $n == "pastm-tl2" ] ; then
       ghc=/localdisk/ryates/ghc-8/ghc-pastm-build/bin/ghc
       d=-fpastmtl2
       other="cabal install --with-ghc $ghc /localdisk/ryates/ghc-8/ghc-pastm/libraries/pastm/ \
                            --extra-include-dirs /localdisk/ryates/ghc-8/ghc-pastm/rts/"
    elif [ $n == "pastm-fine" ] ; then
       ghc=/localdisk/ryates/ghc-8/ghc-pastm-build/bin/ghc
       d=-fpastmfine
       other="cabal install --with-ghc $ghc /localdisk/ryates/ghc-8/ghc-pastm/libraries/pastm/ \
                            --extra-include-dirs /localdisk/ryates/ghc-8/ghc-pastm/rts/"
    elif [ $n == "ctrie" ] ; then
       flavor=no-invariants
       ghc=/localdisk/ryates/ghc-8/ghc-no-invariants-build/bin/ghc
    elif [ $n = "hashmapcas" ] || [ $n = "hashmaptvar" ] || [ $n = "hashmaptmvar" ] || [ $n = "hashmapmvar" ] || [ $n = "hashmap" ]; then
       flavor=no-invariants
       ghc=/localdisk/ryates/ghc-8/ghc-no-invariants-build/bin/ghc
    else
       flavor=mutable-fields
       d=
    fi

    ghc=/localdisk/ryates/ghc-8/build-$flavor/bin/ghc
    dump="-ddump-simpl -ddump-cmm -ddump-to-file -dumpdir dump/$n"

    # cabal install optparse-applicative stm MonadRandom ../throughput/ --with-ghc $ghc
    cabal sandbox init --sandbox=$sb

    if [ $n == "pastm-tl2" ] ; then
        $other
        cabal install optparse-applicative /localdisk/ryates/ghc-8/ghc-pastm/libraries/stm \
              $bc ../throughput/ ../random/pcg-random/ ./ $d \
              --disable-executable-stripping --with-ghc $ghc
    elif [ $n == "pastm-fine" ] ; then
        $other
        cabal install optparse-applicative /localdisk/ryates/ghc-8/ghc-pastm/libraries/stm \
              $bc ../throughput/ ../random/pcg-random/ ./ $d \
              --disable-executable-stripping --with-ghc $ghc
    elif [ $n == "head" ] ; then
        # head builds need their local stm
        cabal install optparse-applicative /localdisk/ryates/ghc-8/ghc-head/libraries/stm \
              $bc ../throughput/ ../random/pcg-random/ ./ $d \
              --disable-executable-stripping --with-ghc $ghc
    elif [ $n == "stmtrie-tstruct" ] || [ $n == "stmtrie-tstruct-allocs" ] || [ $n == "stmtrie-tstruct-fine" ] || [ $n == "stmtrie-tstruct-fine-htm" ] ; then
        cabal install optparse-applicative $bc ../throughput/ ../random/pcg-random/ ./ $d \
            stm-containers-TStruct/ \
            --disable-executable-stripping --with-ghc $ghc --ghc-options="-O2 -msse4.2"
    elif [ $n == "stmtrie" ] || [ $n == "stmtrie-fine" ] || [ $n == "stmtrie-allocs" ] ; then
        cabal install optparse-applicative $bc ../throughput/ ../random/pcg-random/ ./ $d \
            stm-containers-0.2.9/ \
            --disable-executable-stripping --with-ghc $ghc --ghc-options="-O2 -msse4.2"
    else
        cabal install optparse-applicative $bc ../throughput/ ../random/pcg-random/ ./ $d \
            --disable-executable-stripping --with-ghc $ghc --ghc-options="-O2 -msse4.2"
    fi

    cp $sb/bin/rbtree-throughput bin/Main-$n-8
#    cabal install optparse-applicative stm mwc-random ../throughput/ --with-ghc $ghc
    
#    cabal exec -- $ghc -O2 -threaded -rtsopts -eventlog Main.hs -outputdir .build-$n -fno-omit-yields -o Main-$n
#    cabal exec -- $ghc -O2 -threaded -rtsopts -eventlog Main-stm-containers.hs -outputdir .build-sc-$n -fno-omit-yields -o Main-sc-$n
#    $ghc -O2 -threaded -rtsopts -eventlog Main-32.hs -outputdir .build-$n -fno-omit-yields -o Main-32-$n
#   $ghc -O2 -threaded -rtsopts -eventlog Main.hs -outputdir .build -o Main-event

done
