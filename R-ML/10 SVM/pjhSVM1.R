Tra <- read.csv("Tra1(1).csv",header = T)
Val <- read.csv("Val1(1).csv",header = T)
Tes <- read.csv("Tes1(1).csv",header = T)

D.tra <- Tra[,-c(1)]
D.val <- Val[,-c(1)]
D.tes <- Tes[,-c(1)]
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
#cost=100
linearSVM <- svm(Class~.,D.tra,type='C-classification',kernel='linear',cost=100,scale = F)

Predict.tra <- predict(linearSVM,D.tra)
cTab <- table(Actual=D.tra$Class,Predict.tra)
Accu.tra <- sum(diag(cTab))/sum(cTab)*100
Accu.tra

#val바꾸기
Predict.tra <- predict(linearSVM,D.tra)
cTab <- table(Actual=D.tra$Class,Predict.tra)
Accu.tra <- sum(diag(cTab))/sum(cTab)*100
Accu.tra

#오래걸린다
set.seed(tSeed)
poly_tune <- tune(svm,x,y,kernel='polynomial',ranges = list(cost=c(50,100,150),gamma=seq(0.1,1,by=0.1),coef0=c(1,2),degree=c(2,3)))
print(poly_tune)

set.seed(tSeed)
radial_tune <- tune(svm,x,y,kernel='radial',ranges = list(cost=c(50,100,150),gamma=seq(0.1,1,by=0.1)))
print(radial_tune)
