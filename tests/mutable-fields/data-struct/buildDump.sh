#!/bin/bash

rm -rf .build

# ghc=/localdisk/ryates/ghc-8/build-mutable-fields/bin/ghc
ghc=/home/ryates/ghc-8/build-mutable-fields/bin/ghc

# opt=-debug
# opt=-O0
# opt=-O1
# opt=-O2
# opt="-O0 -debug"
# opt="-O1 -debug"
opt="-O2 -debug"
# opt="-O2 -debug -fno-worker-wrapper"

lint="-dcmm-lint -dstg-lint -dcore-lint"
# lint=""

dump="-ddump-simpl -ddump-stg -ddump-cmm -ddump-asm"

$ghc $1 -rtsopts $opt -with-rtsopts="-V0" $lint $dump -outputdir .build &> dump-$1.cmm
objdump -Ds `basename $1 .hs` > `basename $1 .hs`.s
