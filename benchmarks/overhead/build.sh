ghc small.hs -O2 -threaded -rtsopts -DRUN_IO  -outputdir .buildSIO  -o .bin/smallIO
ghc small.hs -O2 -threaded -rtsopts -DRUN_STM -outputdir .buildSSTM -o .bin/smallSTM
ghc small.hs -O2 -threaded -rtsopts           -outputdir .buildS    -o .bin/small

ghc largeRead.hs -O2 -threaded -rtsopts -DRUN_IO  -outputdir .buildLRIO  -o .bin/largeReadIO
ghc largeRead.hs -O2 -threaded -rtsopts -DRUN_STM -outputdir .buildLRSTM -o .bin/largeReadSTM
ghc largeRead.hs -O2 -threaded -rtsopts           -outputdir .buildLR    -o .bin/largeRead


