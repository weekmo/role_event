# read data
setwd("C:/Users/mabdelgadi/Downloads/Private/Role_event/001-day/")
our_data <- read.csv("simDataLongDx.csv",sep = ',',header = TRUE)
final_data <- read.csv("simDataLongFinal.csv",sep = ',',header = TRUE)
# Ex1.1
table(c(data.frame(our_data$DiagStatus)))
# Ex1.2
summary(final_data)

# Normal
our_data.normal <- our_data[our_data$DiagStatus=='NL',]
our_data.normal.count=nrow(our_data.normal)

# MCI
our_data.mci <- our_data[our_data$DiagStatus=='MCI',]
our_data.mci.count = nrow(our_data.mci)

# Dementia
our_data.dementia <- our_data[our_data$DiagStatus=='Dementia',]
our_data.dementia.count = nrow(our_data.dementia)

table(c(data.frame(our_data$DiagStatus)))

our_data.unique=unique(our_data[2:3])
our_data.shared <- data.frame(table(unlist(our_data.unique$PTID)))
our_data.shared.count <- nrow(our_data.shared[our_data.shared$Freq>1,])

