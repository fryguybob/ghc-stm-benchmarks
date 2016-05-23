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

flavor=htm-mut-fine

ghc=/localdisk/ryates/ghc-7.10/ghc-$flavor-build/bin/ghc

$ghc sizeDebug.hs cbits/size.cmm -O0 -rtsopts -outputdir .build -ddump-cmm &> cmm.dump
$ghc size.hs -O0 -rtsopts -outputdir .build-s
$ghc main.hs -O0 -rtsopts -outputdir .build-m -ddump-cmm &> main-cmm.dump
$ghc structTest.hs -O0 -debug -rtsopts -outputdir .build-struct

$ghc --make RBTree.hs -O0 -rtsopts -outputdir .build-rbtree -DTESTCODE -main-is RBTree.testMain \
     -o rbtree -threaded -debug -dcore-lint -ddump-cmm &> rbtree.simpl.dump

$ghc --make RBTreeUnpack.hs -O0 -rtsopts -outputdir .build-rbtree -DTESTCODE \
     -main-is RBTreeUnpack.testMain \
     -o rbtree-unpack -threaded -debug -dcore-lint -ddump-simpl -ddump-cmm &> rbtree-unpack.simpl.dump

$ghc readWord.hs -O0 -ddump-cmm -outputdir .build-readWord -ddump-simpl &> readWord-cmm.dump

$ghc minimal.hs -O0 -rtsopts -debug -outputdir .build-minimal -dcore-lint -ddump-cmm &> minimal-cmm.dump
# $ghc smallArray.hs -O0 -rtsopts -debug -outputdir .build-small -dcore-lint

# objdump -D sizeDebug &> sizeDebug.s
# objdump -D rbtree &> rbtree.s

