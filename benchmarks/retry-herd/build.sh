#!/bin/bash

set -e

for n in htm-bloom no-invariants coarse fine-hle; do
    ghc=/localdisk/ryates/ghc-7.10/ghc-$n-build/bin/ghc

    cabal install optparse-applicative stm split ../throughput/ --with-ghc $ghc

    $ghc -O2 -threaded -fno-omit-yields -rtsopts -eventlog Herd.hs -outputdir .build-$n -o Herd-$n -main-is Herd
done
