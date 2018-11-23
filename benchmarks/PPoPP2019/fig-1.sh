#!/bin/bash

set -e

if [[ -z $GHC_COMPILERS ]]; then
    GHC_COMPILERS=$HOME/ghc-8
fi

run=$GHC_COMPILERS/hybrid/bin/runhaskell

if [[ ! -x $run ]]; then
    echo "Did not find ghc here: $run"
    echo "GHC_COMPILERS=$GHC_COMPILERS"
    echo "Check that GHC_COMPILERS points to the right place."
    exit 1
fi

N=`nproc`

n=100000
m=90
q=-qatopo-cores-sockets-threads-$N
s=1000

function threads()
{
  if [[ $1 == "t" ]]; then
    if [[ $2 -le 10 ]]; then
      count=10
    else
      count=$2
    fi
  else
    count=$1
  fi
}

# mut-hybrid            RBTREE     HYBRID    MUT       2 t
function doBenchmark()
{
  label=$1
  bench=$2
  compiler=$3
  code=$4
  htm=$5
  hle=$6

  name=$bench-$code-$compiler
  exe=./bin/$name 
  log="logs/$bench-$label.log"
  logs="$logs $log"
  echo "Benchmarking $name."
  rm -f "$log" &> /dev/null

  for t in `seq 1 $N`; do
    threads $htm $t
    thtm=$count
    threads $hle $t
    thle=$count
    retry="--htm-retry=$thtm --hle-retry=$thle"
    cmd="$exe -e $n -t $t -m $m -s $s +RTS --stm-stats $q -N$t -ki4k -kc64k -kb4k -A1m $retry"
    echo $cmd
    echo "command: $cmd" &>> $log
    $cmd &>> $log
  done
}

for p in a b c; do
  logs=""
  input="fig-1$p.txt"
  while IFS= read -r var
  do
    doBenchmark $var
  done < "$input"

  $run plot.hs "Figure 1($p)" "output/fig-1$p.html" $logs
done

