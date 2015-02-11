#!/bin/Rscript
library(ggplot2)

as <- commandArgs(trailingOnly=TRUE)

d <- read.csv(as[1])

pdf("throughput.pdf")

ggplot(data=d) + geom_line()
# matplot(d,type=c("b"),pch=1)


