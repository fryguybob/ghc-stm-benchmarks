#!/bin/bash

function b {
    echo -e "\e[1m$1 $2\e[0m"
    ./build.hs build $1 $2 -c >& build-log/log-$1.cmm
    ret=$?
    if [ $ret -ne 0 ]; then
        echo "Log: build-log/log-$1.cmm"
        echo "-----------------------------"
        tail build-log/log-$1.cmm
        echo "-----------------------------"
    fi
}

b rbtreetvar      fine        
b rbtree          tstruct-fine
b rbtreemutstm    fine        
b rbtreemutsingle fine        
b rbtreeioref     fine        
b treapmutsingle  fine        
b treapioref      fine        
b treapmutstm     fine        
b treapmutstmcps  fine        
b treapmutstmref  fine        
b treaptvar       fine        
#./build.hs build rbtreemutsingle fine -c >& log.cmm
#./build.hs build rbtreemutsingle fine -c >& log.cmm

