#!/bin/sh

cabal install random-shuffle cmdargs stm async split --with-ghc /localdisk/ryates/ghc/ghc-7.6-build/bin/ghc

~/ghc/ghc-7.6-build/bin/ghc -O2 -threaded -rtsopts Main.hs -outputdir .build -o Main

# ~/ghc/ghc-7.6-build/bin/ghc -O2 -threaded -rtsopts -eventlog Main.hs -outputdir .build -o Main-event

ghc -O2 -threaded -rtsopts Main.hs -outputdir .build-7.6.3 -o Main-7.6.3
