#!/bin/bash

for n in `cat variants`; do
    echo Treap$n.hs
    mkdir -p .build$n
    ~/ghc-8/build-mutable-fields/bin/ghc -O2 Treap$n.hs -rtsopts -outputdir=.build$n -o bin/Treap$n >& .build-logs/Treap$n.log
done
