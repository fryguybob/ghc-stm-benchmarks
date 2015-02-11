#!/bin/Rscript
as <- commandArgs(trailingOnly=TRUE)

d <- read.table(as[1])

p <- density(log(d$V1))
# p <- density(d$V1)

pdf("density.pdf")
# png("density.png")
plot(p)


