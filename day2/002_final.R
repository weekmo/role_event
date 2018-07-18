# Clear workspace
rm(list = ls())

# Set Directory
setwd("C:/Users/mabdelgadi/Downloads/Private/Role_event/002-day")

# Load libraries
library(ggplot2)
library(lme4)
library(dplyr)
library(lattice)
library(lfe)

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
# 
data.normal <- data.original[(data.original$DiagStatus== "NL") & (data.original$DiagStatusBl== "NL"),]

data.normal$APOE4 <- as.factor(data.normal$APOE4)

ggplot(data=data.normal, aes(x=Month, y=Hippocampus , group=PTID,colour=APOE4)) +
  geom_line() +
  geom_point()

# E2.3
reg.model3 <- lm(MMSE ~ Month, data = data.original)

ggplot(data = data.original, aes(x = Month, y = MMSE)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

# Ex2.4
mixed <- lmer(MMSE ~ Month + (1 | PTID),data.original,REML = F)
pred_mmse = fitted(mixed)

print(paste("percentage of subject variance out of the total = ",(13.975/(13.975+2.422))*100,"%"))

data.original$PTID <- as.factor(data.original$PTID)

ggplot(data = data.original, aes(x=Month, y=pred_mmse, group=PTID, color=PTID)) + theme_classic() +
geom_line(size=1)
summary(mixed)
#plot(mixed)

# Ex2.7
ggplot(data = data.dementia, aes(x=Age.in.years,y=MMSE, group=PTID, color=PTID))+ theme_classic() +
  geom_line(size=1)
  #geom_point(color='blue')

plot(data.dementia$Age.in.years,data.dementia$MMSE)


# Ex2.8
dementia.mixed.mmse <- lmer(MMSE ~ Month+ (1 | PTID),data.dementia,REML = F)
dementia.mixed.mmse1 <- lmer(MMSE ~ Month+Gender+ (1 | PTID),data.dementia,REML = F)
dementia.mixed.mmse2 <- lmer(MMSE ~ (Month*Gender)+ (1 | PTID),data.dementia,REML = F)
anova(dementia.mixed.mmse,dementia.mixed.mmse1,dementia.mixed.mmse2)
# For other as well

# -----------
dementia.mixed.faq <- lmer(FAQ ~ (1 | PTID),data.dementia,REML = F)
normal.mixed.Hippocampus <- lmer(Hippocampus ~ (1 | PTID),data.normal,REML = F)

felm(data.original$Month ~ data.original$Month + data.original$Gender)
felm(data.original$Month ~ data.original$Month * data.original$Gender)
# ----------------------
dementia.mixed.mmse #343.0354
dementia.mixed.faq #364.0185
normal.mixed.Hippocampus #3326.528

