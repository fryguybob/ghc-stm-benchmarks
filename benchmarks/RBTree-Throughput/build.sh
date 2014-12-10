#!/bin/bash

for n in 7.10 no-invariants coarse htm-bloom; do

    ghc=/localdisk/ryates/ghc-7.10/ghc-$n-build/bin/ghc

    cabal install random-shuffle cmdargs stm async split ../throughput/ --with-ghc $ghc

    $ghc -O2 -threaded -rtsopts Main.hs -outputdir .build-$n -o Main-$n
#   $ghc -O2 -threaded -rtsopts -eventlog Main.hs -outputdir .build -o Main-event

done
