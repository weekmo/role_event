# Clear workspace
rm(list = ls())

# Set Directory
setwd("C:/Users/mabdelgadi/Downloads/Private/Role_event/003-day")

# Load libraries
library(Matrix)
library(data.table)

# Read data
simDataLong_new <- read.csv("simDataLong_new.csv",sep = ';',header = TRUE)

# Reshape
simDataLong_newReshape <- dcast(simDataLong_new, PTID ~ TIMEPOINT,
                                value.var = "STATE")
# Function to generate transaction matrix
trans.matrix <- function(X, prob=T)
{
  tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
  if(prob) tt <- tt / rowSums(tt)
  tt
}

# Unique matrix
simDataLong_newReshape <- unique(simDataLong_newReshape[,-1])

# Generate transaction matrix
tranMat <-trans.matrix(as.matrix(simDataLong_newReshape[,-1]))
tranMat
tranMat <- tranMat[-1,-1]
tranMat
tranMat <- diag(1,5,5) - tranMat
inv <- solve(tranMat)
inv
one <- matrix(c(1,1,1,1,1),5,1,byrow = T)
(inv)%*%one

