# Clear workspace
rm(list = ls())

# Set Directory
setwd("C:/Users/mabdelgadi/Downloads/Private/Role_event/001-day/")

# Libraries
library(mice)
library(DMwR)
library(compare)
library(corrplot)

# read data
data.first <- read.csv("simDataLongDx.csv",sep = ',',header = TRUE)
data.final <- read.csv("simDataLongFinal.csv",sep = ',',header = TRUE)
data.original <- read.csv("originalDataset.csv",sep = ',',header = TRUE)

# Ex1.1
data.first <-unique(data.first[,2:3])
table(c(data.frame(data.first$DiagStatus)))

# Ex1.2
summary(data.final)

# Ex1.4
data.normal <- data.first[data.first$DiagStatus=='NL',]
data.mci <- data.first[data.first$DiagStatus=='MCI',]
data.dementia <- data.first[data.first$DiagStatus=='Dementia',]


data.dementia.joined=merge(x=unique(data.dementia[2:3]),y=data.final[,-1], by="PTID", all.x = TRUE)
View(data.dementia.joined[c(1:2,7:ncol(data.dementia.joined))])

# Ex1.5
col.num <- function(strt) return(c(2:6,(5*(1:12))+2+strt))
col.names <-c("PTID","Age.in.years","Gender","Marital.Status","APOE4","CDRSB","ADAS11","FAQ","ABETA","TAU","PTAU","Hippocampus","Entorhinal","ICV","MMSE","ADAS13","Ventricles")

df1 <- data.final[,col.num(0)]
colnames(df1) <- col.names

df2 <- data.final[,col.num(1)]
colnames(df2) <- col.names

df3 <- data.final[,col.num(2)]
colnames(df3) <- col.names

df4 <- data.final[,col.num(3)]
colnames(df4) <- col.names

df5 <- data.final[,col.num(4)]
colnames(df5) <- col.names

data.combined <- rbind(df1,df2,df3,df4,df5)
summary(data.combined)

data.multiimput <- mice(data.combined)
data.multiimput <- as.data.frame(complete(data.multiimput))
data.knn <- knnImputation(data.combined)


# lib NRMSE for compare
colvec <- setdiff((colnames(data.original)), (colnames(data.combined)))
data.original[,colvec] <- NULL
comp <- compare(data.knn,data.original,allowAll = TRUE)
comp$detailedResult

# Ex1.3
data.cor <- cor(data.knn[,6:ncol(data.knn)])
corrplot(data.cor,type = "lower", tl.col = "black")

# Ex1.6
data.dementia.imputed=merge(x=unique(data.dementia[2:3]),y=data.knn, by="PTID", all.x = TRUE)
data.mci.imputed=merge(x=unique(data.mci[2:3]),y=data.knn, by="PTID", all.x = TRUE)
data.normal.imputed=merge(x=unique(data.normal[2:3]),y=data.knn, by="PTID", all.x = TRUE)

data.dementia.ttest <- t.test(data.dementia.imputed[data.dementia.imputed$APOE4==1,7:ncol(data.dementia.imputed)],data.dementia.imputed[data.dementia.imputed$APOE4==2,7:ncol(data.dementia.imputed)])
data.mci.ttest <- t.test(data.mci.imputed[data.mci.imputed$APOE4==1,7:ncol(data.mci.imputed)],data.mci.imputed[data.mci.imputed$APOE4==2,7:ncol(data.mci.imputed)])
data.normal.ttest <- t.test(data.normal.imputed[data.normal.imputed$APOE4==1,7:ncol(data.normal.imputed)],data.normal.imputed[data.normal.imputed$APOE4==2,7:ncol(data.normal.imputed)])

data.dementia.ttest
data.mci.ttest
data.normal.ttest
