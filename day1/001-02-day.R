#install.packages("mice")
#airbarmsb
#regresionrmp
setwd("C:/Users/mabdelgadi/Downloads/Private/Role_event/001-day/")
library(mice)
final_data <- read.csv("simDataLongFinal.csv",sep = ',',header = TRUE)
joined.dementia=merge(x=unique(our_data.dementia[2:3]),y=final_data, by="PTID", all.x = TRUE)

joined.dementia.variables = joined.dementia[8:ncol(joined.dementia)]
# Find missing data pattern
md.pattern(joined.dementia.variables)
