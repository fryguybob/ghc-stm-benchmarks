// Modified from criterion:  github.com/bos/criterion/
//                           Copyright (c) 2009, 2010 Bryan O'Sullivan

#include <time.h>

double throughput_gettime(void)
{
    struct timespec ts;

    clock_gettime(CLOCK_MONOTONIC, &ts);

    return ts.tv_sec + ts.tv_nsec * 1e-9;
}
