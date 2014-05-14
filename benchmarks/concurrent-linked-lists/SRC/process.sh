#/bin/bash

grep Perf log.txt     | cut -d' ' -f6-11 > perf.txt
grep Time log.txt     | cut -d' ' -f2 > time.txt
grep start log.txt    | cut -d' ' > start.txt
grep conflict log.txt > conflict.txt
grep capacity log.txt > capacity.txt


