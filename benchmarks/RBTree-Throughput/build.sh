#!/bin/bash

bc=-f-byteCounter
#bc=-fbyteCounter
other=

# for n in pastm-tl2 pastm-fine head; do
for n in htm-mut; do
# for n in head 7.10.1 pastm-tl2 pastm-fine; do
# for n in cuckoo stmtrie-fine stmtrie-htm no-invariants coarse htm-bloom fine-hle htm-mut htm-mut-fine ctrie pastm-tl2; do
# for n in hashmap hashmapcas hashmaptvar hashmaptmvar hashmapmvar; do
# for n in cuckoo stmtrie-fine stmtrie-htm; do
# for n in no-invariants coarse htm-bloom fine-hle htm-mut; do
# for n in htm-mut; do
# for n in htm-bloom; do
# for n in fine-hle; do
    if [ $n == "cuckoo" ] ; then
       ghc=/localdisk/ryates/ghc-7.10/ghc-htm-mut-build/bin/ghc
       sb=.cabal-sandbox-$n
       echo "------------- $n -------------------"
       d=-fcuckoo
    elif [ $n == "stmtrie-fine" ] ; then
       ghc=/localdisk/ryates/ghc-7.10/ghc-no-invariants-build/bin/ghc
       sb=.cabal-sandbox-$n
       echo "------------- $n -------------------"
       d=-fstmtrie
    elif [ $n == "stmtrie-htm" ] ; then
       ghc=/localdisk/ryates/ghc-7.10/ghc-htm-mut-build/bin/ghc
       sb=.cabal-sandbox-$n
       echo "------------- $n -------------------"
       d=-fstmtrie
    elif [ $n == "pastm-tl2" ] ; then
       ghc=/localdisk/ryates/ghc-7.10/ghc-pastm-build/bin/ghc
       sb=.cabal-sandbox-$n
       echo "------------- $n -------------------"
       d=-fpastmtl2
       other="cabal install --with-ghc $ghc /localdisk/ryates/ghc-7.10/ghc-pastm/libraries/pastm/ \
                            --extra-include-dirs /localdisk/ryates/ghc-7.10/ghc-pastm/rts/"
    elif [ $n == "pastm-fine" ] ; then
       ghc=/localdisk/ryates/ghc-7.10/ghc-pastm-build/bin/ghc
       sb=.cabal-sandbox-$n
       echo "------------- $n -------------------"
       d=-fpastmfine
       other="cabal install --with-ghc $ghc /localdisk/ryates/ghc-7.10/ghc-pastm/libraries/pastm/ \
                            --extra-include-dirs /localdisk/ryates/ghc-7.10/ghc-pastm/rts/"
    elif [ $n == "ctrie" ] ; then
       ghc=/localdisk/ryates/ghc-7.10/ghc-no-invariants-build/bin/ghc
       sb=.cabal-sandbox-$n
       echo "------------- $n -------------------"
       d=-fctrie
    elif [ $n = "hashmapcas" ] || [ $n = "hashmaptvar" ] || [ $n = "hashmaptmvar" ] || [ $n = "hashmapmvar" ] || [ $n = "hashmap" ]; then
        ghc=/localdisk/ryates/ghc-7.10/ghc-no-invariants-build/bin/ghc
        sb=.cabal-sandbox-$n
        echo "------------- $n -------------------"
        d=-f$n
    else
       ghc=/localdisk/ryates/ghc-7.10/ghc-$n-build/bin/ghc
       sb=.cabal-sandbox-$n
       echo "------------- $n -------------------"
       if [ $n == "htm-mut" ] || [ $n == "htm-mut-fine" ] ; then
           echo Building with tstruct support.
           d=-ftstruct
       elif [ $n = "head" ] ; then
           d=-fhead
       else
           d=
       fi
    fi

    # cabal install optparse-applicative stm MonadRandom ../throughput/ --with-ghc $ghc
    cabal sandbox init --sandbox=$sb

    if [ $n == "pastm-tl2" ] ; then
        $other
        cabal install optparse-applicative /localdisk/ryates/ghc-7.10/ghc-pastm/libraries/stm \
              $bc ../throughput/ ../random/pcg-random/ ./ $d \
              --disable-executable-stripping --with-ghc $ghc
    elif [ $n == "pastm-fine" ] ; then
        $other
        cabal install optparse-applicative /localdisk/ryates/ghc-7.10/ghc-pastm/libraries/stm \
              $bc ../throughput/ ../random/pcg-random/ ./ $d \
              --disable-executable-stripping --with-ghc $ghc
    elif [ $n == "head" ] ; then
        # head builds need their local stm
        cabal install optparse-applicative /localdisk/ryates/ghc-7.10/ghc-head/libraries/stm \
              $bc ../throughput/ ../random/pcg-random/ ./ $d \
              --disable-executable-stripping --with-ghc $ghc
    else
        cabal install optparse-applicative stm-2.4.3 $bc ../throughput/ ../random/pcg-random/ ./ $d \
            --disable-executable-stripping --with-ghc $ghc
    fi

    cp $sb/bin/rbtree-throughput Main-$n
#    cabal install optparse-applicative stm mwc-random ../throughput/ --with-ghc $ghc
    
#    cabal exec -- $ghc -O2 -threaded -rtsopts -eventlog Main.hs -outputdir .build-$n -fno-omit-yields -o Main-$n
#    cabal exec -- $ghc -O2 -threaded -rtsopts -eventlog Main-stm-containers.hs -outputdir .build-sc-$n -fno-omit-yields -o Main-sc-$n
#    $ghc -O2 -threaded -rtsopts -eventlog Main-32.hs -outputdir .build-$n -fno-omit-yields -o Main-32-$n
#   $ghc -O2 -threaded -rtsopts -eventlog Main.hs -outputdir .build -o Main-event

done
