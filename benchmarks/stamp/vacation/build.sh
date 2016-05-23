#!/bin/bash

set -e

bc=-f-byteCounter
#bc=-fbyteCounter

# for n in no-invariants coarse htm-bloom fine-hle htm-mut; do
for n in heap-allocs; do
# for n in no-invariants htm-bloom; do

    ghc=/localdisk/ryates/ghc-7.10/ghc-$n-build/bin/ghc
    sb=.cabal-sandbox-$n
    dist=dist-$n

    echo "------------- $n -------------------"

    if [ $n == "htm-mut" ] || [ $n == "htm-mut-fine" ] || [ $n == "heap-allocs" ] ; then
        echo Building with tstruct support.
        d=-ftstruct
    else
        d=
    fi


    cabal sandbox init --sandbox=$sb
    cabal sandbox add-source ../../throughput/
    cabal sandbox add-source ../../random/pcg-random/
    cabal install stm-2.4.3 $bc ../../throughput --with-ghc=$ghc
    cabal install vacation.cabal $d --with-ghc=$ghc --builddir=$dist --disable-executable-stripping
    
    cp ./.cabal-sandbox-$n/bin/vacation ./.cabal-sandbox-$n/bin/vacation-$n

#    $ghc -O2 -threaded -rtsopts -eventlog Main.hs -outputdir .build-$n -fno-omit-yields -o Main-$n
#   $ghc -O2 -threaded -rtsopts -eventlog Main.hs -outputdir .build -o Main-event

done























