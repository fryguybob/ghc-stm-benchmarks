#!/bin/bash

set -e

for e in stmtrie-tstruct-fine stmtrie-fine; do
   extra=
   if [ $e == "stmtrie-tstruct-fine" ] ; then
     extra="--htm-retry=0 --hle-retry=0"
   fi

   for t in `seq 1 72` ; do
      cmd="./bin/Main-$e -e 100000 -t $t -m 90 -s 1000 +RTS -qatopo-cores-sockets-threads \
          -N$t -ki4k -kc64k -kb4k -A8m -s $extra"
      echo $cmd
      $cmd &>> $1
   done
done
