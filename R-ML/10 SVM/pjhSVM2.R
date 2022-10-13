rm(list = ls())
c(12345,11111,22222,33333,44444,55555,66666,77777,88888,99999)
D.data <-read.csv("bCancerWC.csv",header = T)
library(doBy)

pSeed <- 12345
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

ranges=list(cost=c(50,100,150),gamma=seq(0.1,1,by=0.1),coef0=c(0,1,2),degee=c(2,3))

#parameter 정하기 

#install.packages("e1071")
library(e1071)
x <- subset(D.tra,select=-c(Class))
y<- D.tra$Class

#tune
tSeed <- 12345
set.seed(tSeed)
linear_tune <- tune(svm,x,y,kernel='linear',ranges = list(cost=c(50,100,150)))
print(linear_tune)
#cost=50


linearSVM <- svm(Class~.,D.tra,type='C-classification',kernel='linear',cost=50,scale = F)

Predict.tra <- predict(linearSVM,D.tra)
cTab <- table(Actual=D.tra$Class,Predict.tra)
Accu.tra <- sum(diag(cTab))/sum(cTab)*100
Accu.tra

#val바꾸기
Predict.val <- predict(linearSVM,D.val)
cTab <- table(Actual=D.val$Class,Predict.val)
Accu.val <- sum(diag(cTab))/sum(cTab)*100
Accu.val

#poly
#오래걸린다
#set.seed(tSeed)
#poly_tune <- tune(svm,x,y,kernel='polynomial',ranges = list(cost=c(50,100,150),gamma=seq(0.1,1,by=0.1),coef0=c(1,2),degree=c(2,3)))
#print(poly_tune)

#radial
set.seed(tSeed)
radial_tune <- tune(svm,x,y,kernel='radial',ranges = list(cost=c(50,100,150),gamma=seq(0.1,1,by=0.1)))
print(radial_tune)
# cost=50 ,gamma=0.4

#Sigmold
set.seed(tSeed)
sigmoid_tune <- tune(svm,x,y,kernel='sigmoid',ranges = list(cost=c(50,100,150),gamma=seq(0.1,1,by=0.1),coef0=c(0,1,2)))
print(sigmoid_tune)
#cost=50 gamma=0.1 coef0=2
