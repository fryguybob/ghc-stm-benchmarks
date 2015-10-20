#!/bin/bash

# Example:
# ./plotMulti.sh max results-max.pdf results-*.log

m=$1
p=$2

shift
shift

./parse.hs -m $m -T "$@"

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
