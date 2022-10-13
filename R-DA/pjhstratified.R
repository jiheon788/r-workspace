rm(list = ls())


data<-read.csv("pjhData.csv",header = T)

library(doBy)
trainingRate <- 0.5
validationRate <- 0.3
pSeed <- 1234
set.seed(pSeed)

Training<-sampleBy(formula = ~Z,frac = trainingRate,replace = F,systematic = F,data=data)
tra.row <-sort(Training$Record)
Training<-data[tra.row,]
temp <-data[-tra.row,]
Validation<-sampleBy(formula = ~Z,frac = validationRate/(1-trainingRate),replace = F,systematic = F,data=temp)
val.row <-sort(Validation$Record)
Validation<-data[val.row,]
Test<-data[-c(tra.row,val.row),]

Training
Validation
Test

