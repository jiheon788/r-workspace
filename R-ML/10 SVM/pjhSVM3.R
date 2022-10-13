setwd('C:/R DA-ML/10 SVM')
rm(list = ls())
library(e1071)
library(doBy)
pSeedList <-c(12345,23456,34567,45678,56789,98082,21686,99999,11111,88888)

D.data <-read.csv("bCancerWC.csv",header = T)

for (pSeed in pSeedList) {
  set.seed(pSeed)  
  
  trainingRate <- 0.5
  validationRate <- 0.3
  
  Tra<-sampleBy(formula = ~Z,frac = trainingRate,replace = F,systematic = F,data=D.data)
  tra.row <-sort(Tra$Record)
  Tra<-D.data[tra.row,]
  temp <-D.data[-tra.row,]
  Val<-sampleBy(formula = ~Z,frac = validationRate/(1-trainingRate),replace = F,systematic = F,data=temp)
  val.row <-sort(Val$Record)
  Val<-D.data[val.row,]
  Tes<-D.data[-c(tra.row,val.row),]
  
  D.tra <- Tra[,-c(1)]
  D.val <- Val[,-c(1)]
  D.tes <- Tes[,-c(1)]  
  
  linearSVM <- svm(Class~.,D.tra,type='C-classification',kernel='linear',cost=50,scale = F)
  
  Predict.tra <- predict(linearSVM,D.tra)
  cTab <- table(Actual=D.tra$Class,Predict.tra)
  Accu.tra <- sum(diag(cTab))/sum(cTab)*100
  #Tes적중률구할때 여기건들기
  Predict.val <- predict(linearSVM,D.val)
  cTab <- table(Actual=D.val$Class,Predict.val)
  Accu.val <- sum(diag(cTab))/sum(cTab)*100
  
  print(pSeed)
  print(Accu.tra)
  print(Accu.val)

}
