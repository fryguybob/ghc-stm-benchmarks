#!/bin/bash

# ./emu.sh ./bin/Main-rbtreemutstm-TVar-coarse-hybrid-8 -e 1000 -t 4 -m 90 -s 1000 +RTS --stm-stats -qatopo-cores-sockets-threads -N4 -ki4k -kc64k -kb4k -A1m --htm-retry=10 --hle-retry=10 --stm-accum=0
# ./emu.sh ./bin/Main-treapmutstmref-TVar-coarse-hybrid-8 -e 1000 -t 4 -m 90 -s 1000 +RTS --stm-stats -qatopo-cores-sockets-threads -N4 -ki4k -kc64k -kb4k -A1m --htm-retry=10 --hle-retry=10 --stm-accum=0
./emu.sh ./bin/Main-rbtreemutustm-TVar-coarse-hybrid-8 -e 1000 -t 4 -m 90 -s 1000 +RTS --stm-stats -qatopo-cores-sockets-threads -N4 -ki4k -kc64k -kb4k -A1m --htm-retry=10 --hle-retry=10 --stm-accum=0

echo look at sde-tsx-stats.txt 
