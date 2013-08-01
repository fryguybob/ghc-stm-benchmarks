#!/bin/sh

cabal install async --with-ghc=/localdisk/ryates/ghc/ghc-7.6-build/bin/ghc

rm -f simple.o simple.hi simple

/localdisk/ryates/ghc/ghc-7.6-build/bin/ghc -rtsopts -threaded -O2 simple.hs
