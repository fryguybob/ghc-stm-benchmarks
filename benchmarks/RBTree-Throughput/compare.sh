#!/bin/bash

set -e

if [ $# -eq 0 ] ; then
  echo "use:"
  echo "  ./compare.sh -e 100000 -t 36 -m 90 -s 1000 +RTS -qatopo-cores-sockets-threads -N36 -ki4k -kc64k -kb4k -A2m --htm-retry=0 --hle-retry=0"
  exit 1
fi

# HASKELL 2017 
for exe in stmtrie-fine-8 stmtrie-tstruct-fine-8 no-invariants-8 tstruct-fine-8 cuckoo-tvar-fine-8 cuckoo-tstruct-fine-8 cuckoo-tstruct-int-fine-8 skiplist-8 skiplist-tstruct-fine-8; do

    main=./bin/Main-$exe
    $main $@

done

