#!/bin/bash

./build.hs build rbtree          fine         -c >& build-log/log-rbtree.cmm
./build.hs build rbtree          tstruct-fine -c >& build-log/log-rbtreetstruct.cmm
./build.hs build rbtreemutstm    fine         -c >& build-log/log-rbtreemutstm.cmm
./build.hs build rbtreemutsingle fine         -c >& build-log/log-rbtreemutsingle.cmm
./build.hs build rbtreeioref     fine         -c >& build-log/log-rbtreeioref.cmm
./build.hs build treapmutsingle  fine         -c >& build-log/log-treapmutsingle.cmm
./build.hs build treapioref      fine         -c >& build-log/log-treapioref.cmm
#./build.hs build rbtreemutsingle fine -c >& log.cmm
#./build.hs build rbtreemutsingle fine -c >& log.cmm

