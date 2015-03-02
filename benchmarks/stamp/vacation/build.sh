#!/bin/bash

set -e

for n in no-invariants coarse htm-bloom fine-hle; do
# for n in htm-bloom; do

    ghc=/localdisk/ryates/ghc-7.10/ghc-$n-build/bin/ghc
    sb=.cabal-sandbox-$n
    dist=dist-$n

    cabal sandbox init --sandbox=$sb
    cabal sandbox add-source ../../throughput/
    cabal install vacation.cabal --with-ghc=$ghc --builddir=$dist --disable-executable-stripping

#    $ghc -O2 -threaded -rtsopts -eventlog Main.hs -outputdir .build-$n -fno-omit-yields -o Main-$n
#   $ghc -O2 -threaded -rtsopts -eventlog Main.hs -outputdir .build -o Main-event

done























