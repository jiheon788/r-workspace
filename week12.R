# 2022.05.23
# 빅데이터개론 및 분석 실습 12주차
# e비즈니스학과 201823871 박지헌
getwd()
setwd('C:/Users/82104/my-workspace/R-workspace/')
rm(list = ls())
library(rpart)
library(rpart.plot) 
library(e1071)
library(nnet)

data <- iris 

set.seed(1234)
idxs <- sample(1:nrow(data), as.integer(0.7*nrow(data))) #
train <- data[idxs,] 
test <- data[-idxs,]

model <- naiveBayes(Species ~ ., train)

new <- data.frame(실젯값=test$Species) 
new$예측값 <- predict(model, test) 

predict_table <- table(new$예측값, new$실젯값) 
names(dimnames(predict_table)) <- c("predicted", "observed")
predict_table 

new$result <- ifelse(new$실젯값 == new$예측값, "Y", "N") 
predict_prob <- sum(new$result=="Y")/length(new$result) 
predict_prob


new_iris <- data.frame(5.1, 3.7, 1.5, 0.3)
names(new_iris) <- c("Sepal.Width", "Sepal.Length", "Petal.Width", "Petal.Length")
new_iris

p_iris <- predict(model, new_iris) 
p_iris


model <- svm(Species ~ . , 
             train, 
             type = "C-classification", 
             kernel = "radial",
             cost = 10, 
             gamma = 0.1)

new <- data.frame(실젯값= test$Species) 
new$예측값 <- predict(model, test, decision.values = TRUE) 
predict_table <- table(new$예측값, new$실젯값) 
names(dimnames(predict_table)) <- c("predicted", "observed")
predict_table
new$result <- ifelse(new$실젯값 == new$예측값, "Y", "N")
predict_prob <- sum(new$result=="Y")/length(new$result) 
predict_prob

cost_range <- 10^(-1:2) # cost 파라미터의 범위
gamma_range <- c(.1,5,1,2) # gamma 파라미터의 범위
svm_tune <- tune(svm, train.x = Species ~ ., data = train, kernel="radial", ranges=list(cost=cost_range, gamma=gamma_range))
svm_tune
model <- svm(Species ~ . , train, type = "C-classification", kernel = "radial", cost = 10, gamma = 0.1)
new <- data.frame(실젯값= test$ Species) 
new$예측값 <- predict(model, test, decision.values = TRUE) 
predict_table <- table(new$예측값, new$실젯값)  
names(dimnames(predict_table)) <- c("predicted", "observed")
predict_table
new$result <- ifelse(new$실젯값 == new$예측값, "Y", "N")
predict_prob <- sum(new$result=="Y")/length(new$result) 
predict_prob

model <- rpart(Species ~ ., data=train)
rpart.plot(model)
new <- data.frame(실젯값= test$Species) 
new$예측값 <- predict(model, newdata=test, type= "class") 

predict_table <- table(new$예측값, new$실젯값) 
names(dimnames(predict_table)) <- c("predicted", "observed")
predict_table

new$result <- ifelse(new$실젯값 == new$예측값, "Y", "N")
predict_prob <- sum(new$result=="Y")/length(new$result) 
predict_prob

model <- nnet(Species ~ ., data = train, size = 2)
library(downloader)

source_url( "https://gist.githubusercontent.com/fawda123/5086859/raw/17fd6d2adec4dbcf5ce750cbd1f3e0f4be9d8
b19/nnet_plot_fun.r" , prompt = FALSE)
plot.nnet(model)
new <- data.frame(실젯값= test$Species) 
new$예측값 <- predict(model, test, type = "class") 
predict_table <- table(new$예측값, new$실젯값)
names(dimnames(predict_table)) <- c("predicted", "observed")
predict_table 

new$result <- ifelse(new$실젯값 == new$예측값, "Y", "N")
predict_prob <- sum(new$result=="Y")/length(new$result) 
predict_prob


options("width"=500)
data <- read.csv("glass.csv", header = T, fileEncoding="EUC-KR")
data <- data[-1] 
org_type <- data[,10] 
normalize <- function(x) { 
  return((x-min(x)) / (max(x)-min(x)))
}
data <- as.data.frame(lapply(data, normalize)) 
data$Type <- factor(org_type) 
head(data) 
set.seed(1234)
idxs <- sample(1:nrow(data), as.integer(0.7*nrow(data))) 
train <- data[idxs,] 
test <- data[-idxs,] 
model <- naiveBayes(Type ~ ., train)
new <- data.frame(실젯값=test$Type)
new$예측값 <- predict(model, test)
predict_table <- table(new$예측값, new$실젯값) 
names(dimnames(predict_table)) <- c("predicted", "observed")
predict_table
new$result <- ifelse(new$실젯값 == new$예측값, "Y", "N") 
predict_prob <- sum(new$result=="Y")/length(new$result)
predict_prob

model <- nnet(Type ~ ., data = train, size = 6)
new <- data.frame(실젯값= test$Type)
new$예측값 <- predict(model, test, type = "class")
predict_table <- table(new$예측값, new$실젯값)
names(dimnames(predict_table)) <- c("predicted", "observed")
predict_table

new$result <- ifelse(new$실젯값 == new$예측값, "Y", "N")
predict_prob <- sum(new$result=="Y")/length(new$result) 
predict_prob
