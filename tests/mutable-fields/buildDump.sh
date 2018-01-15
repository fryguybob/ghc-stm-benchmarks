#!/bin/bash

rm -rf .build

ghc=/localdisk/ryates/ghc-8/build-mutable-fields/bin/ghc

# $ghc -debug $1 -dcmm-lint -ddump-simpl -ddump-stg -ddump-cmm -ddump-asm -outputdir .build &> dump-$1.cmm
# $ghc $1 -dcmm-lint -ddump-simpl -ddump-stg -ddump-cmm -ddump-asm -outputdir .build &> dump-$1.cmm
$ghc $1 -rtsopts -dcmm-lint -ddump-simpl -dsuppress-all -ddump-stg -ddump-cmm -ddump-asm -outputdir .build &> dump-$1.cmm
objdump -Ds `basename $1 .hs` > `basename $1 .hs`.s
