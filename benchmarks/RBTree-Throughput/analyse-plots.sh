#!/bin/bash

b=$1

xl=Threads
xf=threads
#xl=Readonly-percent
#xf=cmd-m

# throughput max
./plotMulti.sh $xl $xf Throughput transactions max $b-a-max.pdf logs/$b-*.log

# Conflict min
./plotMulti.sh $xl $xf Conflict perf-tx-conflict min $b-a-conflict-min.pdf logs/$b-*.log

# Capacity min
./plotMulti.sh $xl $xf Capacity perf-tx-capacity min $b-a-capacity-min.pdf logs/$b-*.log

# Start max
./plotMulti.sh $xl $xf Starts perf-tx-start max $b-a-start-max.pdf logs/$b-*.log


