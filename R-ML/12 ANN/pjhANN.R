setwd('C:/R DA-ML/12 ANN')
rm(list = ls())

Tra <- read.csv("Tra1.csv",header = T)
Val <- read.csv("Val1.csv",header = T)
Tes <- read.csv("Tes1.csv",header = T)

D.tra <- Tra[,-c(1)]
D.val <- Val[,-c(1)]
D.tes <- Tes[,-c(1)]

#install.packages('neuralnet')
library(neuralnet)

D.tra$Class <- ifelse(D.tra$Class==2,0,1)
D.val$Class <- ifelse(D.val$Class==2,0,1)

wSeed<-13579
set.seed(wSeed)

H42 <- c(4,2); Thres<-0.1; Step <- 100000;Rate<-0.01
#히든과 트레스는 변화
BPnn <-neuralnet(Class~.,D.tra,hidden = H42,threshold = Thres,stepmax = Step,learningrate = Rate,algorithm = 'backprop',err.fct = 'ce',act.fct = 'logistic',linear.output = F)
plot(BPnn)

cutoff<-0.5
options(scipen = 100) #원래대로하고싶으면 -100
Output.tra <- predict(BPnn,D.tra)
Output.tra
Predict.tra <- ifelse(Output.tra>=cutoff,1,0)
cTab<-table(Actual=D.tra$Class,Predict.tra)
cTab
Accu.tra<-round(sum(diag(cTab))/sum(cTab)*100,digits = 2)
Accu.tra


Output.val <- predict(BPnn,D.val)
Output.val
Predict.val <- ifelse(Output.val>=cutoff,1,0)
cTab<-table(Actual=D.val$Class,Predict.val)
cTab
Accu.val<-round(sum(diag(cTab))/sum(cTab)*100,digits = 2)
Accu.val


