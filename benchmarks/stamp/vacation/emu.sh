#!/bin/bash

echo "$@"

# old versions:
# /localdisk/ryates/intel/sde-external-7.15.0-2015-01-11-lin/sde64 -tsx -tsx_stats -tsx_stats_call_stack -hsw -- "$@"

# latest version TSX stats:
/localdisk/ryates/intel/sde-external-7.31.0-2015-09-25-lin/sde64 -tsx -tsx_stats -tsx_stats_call_stack -hsw -- "$@"

# Instruction histogram mix:
# /localdisk/ryates/intel/sde-external-7.31.0-2015-09-25-lin/sde64 -mix -tsx -hsw -- "$@"
