#!/bin/bash

set -e

for n in no-invariants coarse htm-bloom; do

    ghc=/localdisk/ryates/ghc-7.10/ghc-$n-build/bin/ghc
    sb=.cabal-sandbox-$n
    dist=dist-$n

    cabal sandbox init --sandbox=$sb
    cabal install acid-state-bench.cabal ./acid-state-0.12.3/ ../throughput --with-ghc $ghc

#    $ghc -O2 -threaded -rtsopts -eventlog Main.hs -outputdir .build-$n -fno-omit-yields -o Main-$n
#   $ghc -O2 -threaded -rtsopts -eventlog Main.hs -outputdir .build -o Main-event

done
