#!/bin/bash

set -e

common="-threaded -rtsopts Main.hs -package semigroups-0.9.2"

~/ghc/ghc-7.6-build/bin/ghc $common -outputdir .build -o Main         -main-is Main.main
~/ghc/ghc-7.6-build/bin/ghc $common -outputdir .build -o Main-init    -main-is Main.mainInit
~/ghc/ghc-7.6-build/bin/ghc $common -outputdir .build -o Main-nocheck -main-is Main.mainNoCheck

ghc $common -outputdir .build-7.6.3 -o Main-7.6.3         -main-is Main.main
ghc $common -outputdir .build-7.6.3 -o Main-7.6.3-init    -main-is Main.mainInit
ghc $common -outputdir .build-7.6.3 -o Main-7.6.3-nocheck -main-is Main.mainNoCheck
