#!/bin/bash

n=htm-mut
ghc=/localdisk/ryates/ghc-7.10/ghc-$n-build/bin/ghc
d=-ftstruct

rm -rf mut-dump

for bc in -f-byteCounter -fbyteCounter; do
    echo "------------- $n $bc -------------------"

    mkdir mut-dump

    sb=.cabal-sandbox-$n$bc

    cabal sandbox init --sandbox=$sb
    cabal install optparse-applicative stm $bc ../throughput/ ../random/pcg-random/ ./ $d \
        --disable-executable-stripping --with-ghc $ghc

    cp $sb/bin/rbtree-throughput Main-$n-$bc
    mv mut-dump/ mut-dump$bc/
    objdump -D Main-$n-$bc &> dump$bc{}.s
done
