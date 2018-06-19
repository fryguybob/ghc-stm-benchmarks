#!/bin/bash

# target='/localdisk/ryates/www-test'
target='/u/www/u/ryates/hybrid'

rsync -av --progress --chmod=a+rX html/ $target

echo "Updated: <http://cs.rochester.edu/u/ryates/hybrid/>"
