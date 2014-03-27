rm(list=ls())
source("~/Projects/R/Ranalysis/useful.R")
d <- read.csv("data/scales_adults.csv")

mss <- aggregate(correct ~ id + condition, data=d, mean)
ms <- aggregate(correct ~ condition, data=mss, mean)
ms$cih <- aggregate(correct ~ condition, data=mss, ci.high)$correct
ms$cil <- aggregate(correct ~ condition, data=mss, ci.low)$correct
ms$n <- aggregate(id ~ condition, data=mss, n.unique)$id