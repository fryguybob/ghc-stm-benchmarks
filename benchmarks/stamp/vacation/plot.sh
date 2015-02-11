#!/bin/bash

# ./parseThroughput.hs $1 -T
# ./parseThreadMix.hs $1 -T
./parsePerfTime.hs $1 $2 -T

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

mv fig.pdf $3
~/uploadT.sh $3

popd &> /dev/null
