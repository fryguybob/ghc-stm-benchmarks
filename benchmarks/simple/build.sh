#!/bin/sh

cabal install async --with-ghc=/localdisk/ryates/ghc/ghc-7.6-build/bin/ghc --reinstall

rm -f simple.o simple.hi simple

/localdisk/ryates/ghc/ghc-7.6-build/bin/ghc -rtsopts -threaded -eventlog -O2 simple.hs
