#!/bin/bash

echo "$@"

/localdisk/ryates/intel/sde-external-7.15.0-2015-01-11-lin/sde64 -tsx -tsx_stats -tsx_stats_call_stack -hsw -- "$@"
