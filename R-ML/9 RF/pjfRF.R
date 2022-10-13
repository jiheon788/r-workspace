setwd('C:/R DA-ML/9 RF')
rm(list = ls())

Tra <- read.csv("Tra1.csv",header = T)
Val <- read.csv("Val1.csv",header = T)
Tes <- read.csv("Tes1.csv",header = T)
D.tra <- Tra[,-c(1)]
D.val <- Val[,-c(1)]
D.tes <- Tes[,-c(1)]
#decision tree
#Before Pruning
#Traning= 87.08; Validation=68.33 ; Variance=18.75
#After Pruning
#Traning= 82.58; Validation=71.67 ; Variance=10.91
  
#F의값 =mtry
#랜덤트리의 갯수= ntree

#install.packages('ramdomForest')
library(randomForest)

x <- subset(D.tra,select=-c(Class))
y <- factor(D.tra$Class)
D.tra$Class <- factor(D.tra$Class)
mtrySeed <- 1234
set.seed(mtrySeed)

bestmtry <- tuneRF(x,y,stepFactor=1.5,improve=0.001,ntreeTry=49)

nTree <- 49
mTry <- 4
rfSeed <- 123
set.seed(rfSeed)

rfModel<- randomForest(Class~.,D.tra,ntree=nTree,mtry=mTry)
Predict.tra <- predict(rfModel,D.tra)
cTab <- table(Acutal=D.tra$Class,Predict.tra)
cTab
Accu.tra <= sum(diag(cTab))/sum(cTab)*100
Accu.tra

levels(D.val$CP)<-levels(D.tra$CP)
levels(D.val$Restecg)<-levels(D.tra$Restecg)
levels(D.val$Slope)<-levels(D.tra$Slope)
levels(D.val$Thal)<-levels(D.tra$Thal)

#val바꾸기
Predict.val <- predict(rfModel,D.val)
cTab <- table(Acutal=D.val$Class,predict.val)
cTab
Accu.val <= sum(diag(cTab))/sum(cTab)*100
Accu.val

Variance <- abs(Accu.tra-Accu.val)
Variance#18.333333

#leaf노드를 제한하여 depth를 제한한다.

# Pruning
maxLeaf <- 16#여러숫자바꾸기
set.seed(rfSeed)
pruned.rfModel<- randomForest(Class~.,D.tra,ntree=nTree,mtry=mTry,maxcodes=maxLeaf)


Predict.tra <- predict(pruned.rfModel,D.tra)
cTab <- table(Acutal=D.tra$Class,predict.tra)
Accu.tra <= sum(diag(cTab))/sum(cTab)*100

Predict.val <- predict(pruned.rfModel,D.val)
cTab <- table(Acutal=D.val$Class,predict.val)
Accu.val <= sum(diag(cTab))/sum(cTab)*100

Variance<- abs(Accu.tra-Accu.val)

nTree; mTry; maxLeaf
Accu.tra
Accu.val
Variance