#Question1 reurns the tranition matrix
library(data.table)
simDataLong_newReshape <- dcast(data3.original, PTID ~ TIMEPOINT,
                                value.var = "STATE")

simDataLong_newReshape <- unique(simDataLong_newReshape[,-1])
trans.matrix <- function(X, prob=T)
{
  tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
  if(prob) tt <- tt / rowSums(tt)
  tt
}

tranMat <- trans.matrix(as.matrix(simDataLong_newReshape[,-1]))
tranMat

#Question 2(a) picks the NL patients and compute the probabilities 

new_df <- simDataLong_newReshape[simDataLong_newReshape$`0`=="NL",]

new_df <- unique(new_df[,-1])

trans.matrix <- function(X, prob=T)
{
  tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
  if(prob) tt <- tt / rowSums(tt)
  tt
}

nl_tm <- trans.matrix(as.matrix(new_df[,-1]))
nl_tm

#FOR A SINGLE PATTERN 
#ab <- simDataLong_newReshape[1,]
#trans.matrix(as.matrix(ab[,-1]))


#computes the probability of the NL
prob1 <- tranMat[5,5]*tranMat[5,5]*tranMat[5,6]*tranMat[6,2]*tranMat[2,2]*
          tranMat[2,3]*tranMat[3,1]
prob1




#new_df1 <- simDataLong_newReshape[simDataLong_newReshape$`0`=="MCI",]
#trans.matrix(as.matrix(new_df1[,-1]))


