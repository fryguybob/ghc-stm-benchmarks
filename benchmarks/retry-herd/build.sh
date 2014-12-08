#!/bin/bash



for n in 7.10 htm-bloom no-invariants coarse; do
# for n in 7.10 htm-active-retry htm-bloom no-invariants coarse; do

    ghc=/localdisk/ryates/ghc-7.10/ghc-$n-build/bin/ghc

    cabal install random-shuffle cmdargs stm async split --with-ghc $ghc
    # cabal configure ../throughput/ --with-ghc $ghc
    cabal install ../throughput/ --with-ghc $ghc

#    $ghc -O2 -threaded -rtsopts Main.hs -outputdir .build-$n -o Main-$n
    $ghc -O2 -threaded -rtsopts Herd.hs -outputdir .build-$n -o Herd-$n -main-is Herd # -debug
#    $ghc -O2 -threaded -rtsopts Simple.hs -outputdir .build-$n -o Simple-$n -main-is Simple # -debug
#    $ghc -O2 -threaded -rtsopts -eventlog Main.hs -outputdir .build -o Main-event

done
