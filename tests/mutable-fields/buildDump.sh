#!/bin/bash

rm -rf .build

# ghc=/localdisk/ryates/ghc-8/build-mutable-fields/bin/ghc
ghc=/home/ryates/ghc-8/build-mutable-fields/bin/ghc
# ghc=/home/ryates/ghc-8/build-mutable-fields-hybrid/bin/ghc

# opt=-debug
# opt=-O0
# opt=-O1
# opt=-O2
# opt="-O0 -debug"
# opt="-O1 -debug"
# opt="-O2 -debug"
opt="-O2 -debug -fno-worker-wrapper"

lint="-dcmm-lint -dstg-lint -dcore-lint"
# lint=""

# dump="-ddump-simpl -dsuppress-all -ddump-stg -ddump-cmm -ddump-asm"
dump="-ddump-simpl -ddump-stg -ddump-cmm -ddump-asm -ddump-simpl-trace"
# dump="-ddump-worker-wrapper"

n=`basename $1 .hs`

$ghc $1 -rtsopts $opt -with-rtsopts="-V0" $lint $dump -outputdir .build -o bin/$n &> dump/dump-$1.cmm
ret=$?
if [ $ret -ne 0 ]; then
    echo "log: dump/dump-$1.cmm"
    echo "-----------------------------"
    tail dump/dump-$1.cmm
    echo "-----------------------------"
    exit 1
fi
objdump -Ds bin/$n > dump/$n.s
