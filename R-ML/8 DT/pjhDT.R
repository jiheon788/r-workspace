rm(list = ls())
getwd()

#UCI machine learning repository
D.data <- read.csv("HeartD.csv",header = T)
Tra <- read.csv("Tra1.csv",header = T)
Val <- read.csv("Val1.csv",header = T)
Tes <- read.csv("Tes1.csv",header = T)

table(Tra$Class)
table(Val$Class)
table(Tes$Class)

head(Tra)
head(Val)
D.data$Class[1:5]

prop.table(table(D.data$Class))
prop.table(table(Tra$Class))
prop.table(table(Val$Class))
prop.table(table(Tes$Class))

D.tra <- Tra[,-c(1)]
D.val <- Val[,-c(1)]

# Building Decision Tree
library(rpart)
#rpart는 반드시 seed지정 후 사용
tseed <-12345
set.seed(tseed)
 # Entropy
e_DT <- rpart(Class~.,D.tra,method = 'class',parms = list(split='information'))
plot(e_DT,margin = 0.2)
text(e_DT)
install.packages('ramdomForest')
table(D.tra$Thal) #t6,7 왼쪽 으로간다  bc
e_DT
version
Predict.tra <- predict(e_DT,D.tra,type="class")
Predict.tra[1:5]
Actual.tra <- D.tra$Class
head(Predict.tra)
head(Actual.tra)

#Confusion Matrix (예측,실제)
library(caret)
library(e1071)
CM1 <- confusionMatrix(Predict.tra,Actual.tra,positive = 'Disease')
#acc0.8708,sen 0.0924 spec 0.8438
cTab <- table(Actual.tra,Predict.tra)
cTab#이게우리가원하는거
CM.tra <- confusionMatrix(t(cTab),positive = 'Disease')
CM.tra
CM.tra$overall['Accuracy']
CM.tra$byClass['Sensitivity']
CM.tra$byClass['Specificity']

#Suming Up
Predict.tra <- predict(e_DT,D.tra,type="class")
Actual.tra <- D.tra$Class
cTab <- table(Actual.tra,Predict.tra)
CM.tra <- confusionMatrix(t(cTab),positive = 'Disease')
Accu.tra <- CM.tra$overall['Accuracy']
cTab
Accu.tra*100

#val로바꾸기
Predict.val <- predict(e_DT,D.val,type="class")
Actual.val <- D.val$Class
cTab <- table(Actual.val,Predict.val)
CM.val <- confusionMatrix(t(cTab),positive = 'Disease')
Accu.val <- CM.val$overall['Accuracy']
cTab
Accu.val*100

# Pruning
printcp(e_DT)#xerror 제일작은거찾기: 3
cpVal <- e_DT$cptable[which.min(e_DT$cptable[,'xerror']),'CP']
cpVal
pruned.e_DT <- prune(e_DT,cp=cpVal)
plot(pruned.e_DT,margin = 0.2)
text(pruned.e_DT)

Predict.tra <- predict(pruned.e_DT,D.tra,type="class")
Actual.tra <- D.tra$Class
cTab <- table(Actual.tra,Predict.tra)
CM.tra <- confusionMatrix(t(cTab),positive = 'Disease')
Accu.tra <- CM.tra$overall['Accuracy']
cTab
Accu.tra*100

#val로바꾸기
Predict.val <- predict(pruned.e_DT,D.val,type="class")
Actual.val <- D.val$Class
cTab <- table(Actual.val,Predict.val)
CM.val <- confusionMatrix(t(cTab),positive = 'Disease')
Accu.val <- CM.val$overall['Accuracy']
cTab
Accu.val*100

#Gini
g_DT<- rpart(cl)

g_DT <- rpart(Class~., D.tra, method='class',parms=list(split="gini"))
