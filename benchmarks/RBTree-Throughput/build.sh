#!/bin/bash

for n in no-invariants coarse htm-bloom fine-hle; do
# for n in htm-bloom; do
# for n in fine-hle; do

    ghc=/localdisk/ryates/ghc-7.10/ghc-$n-build/bin/ghc
    sb=.cabal-sandbox-$n

    # cabal install optparse-applicative stm MonadRandom ../throughput/ --with-ghc $ghc
    cabal sandbox init --sandbox=$db
    cabal install optparse-applicative stm MonadRandom ../throughput/ --with-ghc $ghc
    
    cabal exec -- $ghc -O2 -threaded -rtsopts -eventlog Main.hs -outputdir .build-$n -fno-omit-yields -o Main-$n
#    cabal exec -- $ghc -O2 -threaded -rtsopts -eventlog Main-stm-containers.hs -outputdir .build-sc-$n -fno-omit-yields -o Main-sc-$n
#    $ghc -O2 -threaded -rtsopts -eventlog Main-32.hs -outputdir .build-$n -fno-omit-yields -o Main-32-$n
#   $ghc -O2 -threaded -rtsopts -eventlog Main.hs -outputdir .build -o Main-event

done
