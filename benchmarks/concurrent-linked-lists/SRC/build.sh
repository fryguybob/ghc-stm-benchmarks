#!/bin/bash

rm -rf ./build-7.6
rm -rf ./build

rm Main-7.6
rm Main

ghc                         -O2 -threaded -rtsopts Main.lhs -outputdir .build-7.6 -o Main-7.6

~/ghc/ghc-7.6-build/bin/ghc -O2 -threaded -rtsopts Main.lhs -outputdir .build     -o Main
