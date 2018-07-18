setwd("C:/Users/mabdelgadi/Downloads/Private/Role_event/002-day")
#library(MASS)
#library(lme4)
library(ggplot2)

# Read data
data.original <- read.csv("clinicalDataAlz.csv")

# Ex2.1
data.dementia <- data.original[data.original$DiagStatus == "Dementia",]

data.dementia$PTID <- as.factor(data.dementia$PTID)
data.dementia$Gender <- as.factor(data.dementia$Gender)

ggplot(data=data.dementia, aes(x=Month, y=MMSE, group=PTID,colour=Gender)) +
  geom_line() +
  geom_point()

# Ex2.2
data.normal <- data.original[(data.original$DiagStatus== "NL") & (data.original$DiagStatusBl== "NL"),]

data.normal$APOE4 <- as.factor(data.normal$APOE4)

ggplot(data=data.normal, aes(x=Month, y=Hippocampus , group=PTID,colour=APOE4)) +
  geom_line() +
  geom_point()

# E2.3
reg.model3 <- lm(Month ~ MMSE, data = data.original)

ggplot(data = data.original, aes(x = Month, y = MMSE)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)
