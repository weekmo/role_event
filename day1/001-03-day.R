setwd("C:/Users/mabdelgadi/Downloads/Private/Role_event/001-day/")
library(mice)
col.num <- function(strt) return(c(2:6,(5*(1:12))+2+strt))
col.names <-c("PTID","Age.in.years","Gender","Marital.Status","APOE4","CDRSB","ADAS11","FAQ","ABETA","TAU","PTAU","Hippocampus","Entorhinal","ICV","MMSE","ADAS13","Ventricles")

df1 <- final_data[,col.num(0)]
colnames(df1) <- col.names

df2 <- final_data[,col.num(1)]
colnames(df2) <- col.names

df3 <- final_data[,col.num(2)]
colnames(df3) <- col.names

df4 <- final_data[,col.num(3)]
colnames(df4) <- col.names

df5 <- final_data[,col.num(4)]
colnames(df5) <- col.names

combined.data <- rbind(df1,df2,df3,df4,df5)
combined.data[c(1,137,273,409,545),1:8]

#Use reshape function
filled_data <- mice(combined.data)
filled_data$method
filled_data <- complete(filled_data)
filled_data <- as.data.frame(filled_data)
write.csv(filled_data,file = "imputed_data.csv")
colvec <- setdiff((colnames(originalDataset)), (colnames(imputed_data)))
originalDataset[,colvec] <- NULL

library(DMwR)
data.test <- knnImputation(combined.data)
