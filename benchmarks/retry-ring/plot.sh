#!/bin/bash

# ./parseThroughput.hs $1 -T
# ./parseThreadMix.hs $1 -T
# ./parseRSTM.hs $1 -T -x $3 -f $4 -s 2819257
# ./parseSize.hs $1 -T -x $3 -f $4
./parse.hs $1 -T -x $3 -f $4

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

mv fig.pdf $2
~/uploadT.sh $2

popd &> /dev/null
