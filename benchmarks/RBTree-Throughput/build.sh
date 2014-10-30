#!/bin/bash



flavor=htm-bloom

for n in 7.10 htm-active-retry htm-bloom; do

    ghc=/localdisk/ryates/ghc-7.10/ghc-$n-build/bin/ghc

    cabal install random-shuffle cmdargs stm async split --with-ghc $ghc
    cabal install ../throughput/ --with-ghc $ghc

    $ghc -O2 -threaded -rtsopts Main.hs -outputdir .build-$n -o Main-$n
#   $ghc -O2 -threaded -rtsopts -eventlog Main.hs -outputdir .build -o Main-event

done
