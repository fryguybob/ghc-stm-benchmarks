#!/bin/bash

ghc=/localdisk/ryates/ghc-8/build-mutable-fields/bin/ghc

rm -rf .build-skip

$ghc SkipTest.hs -debug -threaded -outputdir .build-skip -ddump-cmm &> skip-test.dump
