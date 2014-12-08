#!/bin/bash



for n in 7.10 htm-bloom no-invariants coarse; do
# for n in 7.10 htm-active-retry htm-bloom no-invariants coarse; do
    echo
    echo ---------- $n
    ./Herd-$n +RTS -N4 --stm-stats

done
