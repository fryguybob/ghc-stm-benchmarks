#!/bin/bash

set -e

for n in no-invariants; do

    ghc=/localdisk/ryates/ghc-7.10/ghc-$n-build/bin/ghc

    $ghc -O2 -threaded -rtsopts -eventlog Main-IORef.hs -outputdir .build-IORef-$n -fno-omit-yields -o Main-IORef-$n
    $ghc -O2 -threaded -rtsopts -eventlog Main-HashMap.hs -outputdir .build-HashMap-$n -fno-omit-yields -o Main-HashMap-$n
done
