rm(list = ls())
library(Matrix)
library(data.table)
setwd("C:/Users/mabdelgadi/Downloads/Private/Role_event/003-day")
simDataLong_new <- read.csv("simDataLong_new.csv",sep = ';',header = TRUE)
simDataLong_newReshape <- dcast(simDataLong_new, PTID ~ TIMEPOINT,
                                value.var = "STATE")

simDataLong_newReshape <- unique(simDataLong_newReshape[,-1])
trans.matrix <- function(X, prob=T)
{
  tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
  if(prob) tt <- tt / rowSums(tt)
  as.matrix.data.frame(tt)
}

tranMat <-trans.matrix(as.matrix(simDataLong_newReshape[,-1]))
det(tranMat)
Id <- diag(1,6,6)
inv.matrix <- Id - tranMat
det(inv.matrix)
