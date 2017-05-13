#!/bin/bash

# Example:
# ./plotMulti.sh Threads threads max results-max.pdf results-*.log

lx=$1
fx=$2
ly=$3
fy=$4
m=$5
p=$6

shift
shift
shift
shift
shift
shift

./parseHeap.hs -x $lx -X $fx -y $ly -Y $fy -m $m -T "$@"

if [ "$?" -ne "0" ]; then
    exit 1
fi

pushd figures &> /dev/null

pdflatex -interaction=nonstopmode fig.tex &> plot.log

if [ "$?" -ne "0" ]; then
    echo "Failed:"
    tail plot.log
    exit 1
fi

mv fig.pdf $p
~/uploadT.sh $p

popd &> /dev/null
