#!/bin/bash

set -e

rm -rf .build
rm -rf .build-s
rm -rf .build-m
rm -rf .build-struct
rm -rf .build-rbtree
rm -rf .build-minimal
rm -rf .build-small
rm -rf .build-readWord

rm -rf size main structTest sizeDebug rbtree minimal readWord


ghc=/localdisk/ryates/ghc-7.10/ghc-htm-mut-build/bin/ghc

$ghc sizeDebug.hs cbits/size.cmm -O0 -rtsopts -outputdir .build -ddump-cmm &> cmm.dump
$ghc size.hs -O0 -rtsopts -outputdir .build-s
$ghc main.hs -O0 -rtsopts -outputdir .build-m -ddump-cmm &> main-cmm.dump
$ghc structTest.hs -O0 -rtsopts -outputdir .build-struct

$ghc --make RBTree.hs -O0 -rtsopts -outputdir .build-rbtree -DTESTCODE -main-is RBTree.testMain \
     -o rbtree -debug -dcore-lint -ddump-cmm &> rbtree.simpl.dump

$ghc readWord.hs -O0 -ddump-cmm -outputdir .build-readWord -ddump-simpl &> readWord-cmm.dump

$ghc minimal.hs -O0 -rtsopts -debug -outputdir .build-minimal -dcore-lint -ddump-cmm &> minimal-cmm.dump
# $ghc smallArray.hs -O0 -rtsopts -debug -outputdir .build-small -dcore-lint

# objdump -D sizeDebug &> sizeDebug.s
# objdump -D rbtree &> rbtree.s

