#!/bin/bash

function b {
    echo -e "\e[1m$1 $2\e[0m"
#    ./build.hs build $1 $2 -c >& build-log/log-$1.cmm
    ./build.hs build $1 $2 >& build-log/log-$1.cmm
    ret=$?
    if [ $ret -ne 0 ]; then
        echo "Log: build-log/log-$1.cmm"
        echo "-----------------------------"
        tail build-log/log-$1.cmm
        echo "-----------------------------"
    fi
}

# RBTree STM
b rbtreetvar         fine
b rbtreetvar         hybrid
b rbtreemuttvarcolor fine
b rbtreemuttvarcolor hybrid
b rbtree             tstruct-fine
b rbtree             tstruct-hybrid
b rbtreemutstm       fine
b rbtreemutstm       hybrid

# RBTree single-threaded
# b rbtreemutsingle fine
# b rbtreeioref     fine

# Treap STM
b treapmutstm     fine
b treapmutstmcps  fine
b treapmutstmref  fine
b treaptvar       fine
b treapmutstm     hybrid
b treapmutstmcps  hybrid
b treapmutstmref  hybrid
b treaptvar       hybrid

# Treap single-threaded
# b treapmutsingle  fine
# b treapioref      fine


#./build.hs build rbtreemutsingle fine -c >& log.cmm
#./build.hs build rbtreemutsingle fine -c >& log.cmm

