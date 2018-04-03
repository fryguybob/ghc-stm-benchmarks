#!/bin/bash

# Example:
# ./plot.sh Threads threads max results-max.pdf results-1.log

lx=$1
fx=$2
ly=$3
fy=$4
p=$5

shift
shift
shift
shift
shift

./parse.hs -x $lx -X $fx -y $ly -Y $fy -T "$@"
# ./parse.hs $1 -T -x $3 -X $4 -y $5 -Y $6

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
