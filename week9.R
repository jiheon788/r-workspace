# 2022.05.07 
# 빅데이터개론 및 분석 9주차 실습
# e비즈니스학과 201823871 박지헌
getwd()
setwd('C:/Users/82104/my-workspace/R-workspace/')
rm(list = ls())

library(rpart)
r = rpart(Species~., data = iris)
print(r)

par(mfrow = c(1,1), xpd = NA)
plot(r)
text(r, use.n = TRUE)

r_prior = rpart(Species~.,
                data = iris, 
                parms = list(prior=c(0.1, 0.1, 0.8)))
plot(r_prior)
text(r_prior, use.n = TRUE)

newd = data.frame(Sepal.Length = c(5.11, 7.01, 6.32), 
                  Sepal.Width = c(3.51, 3.2, 3.31), 
                  Petal.Length = c(1.4, 4.71, 6.02), 
                  Petal.Width = c(0.19, 1.4, 2.49))
newd

predict(r, newdata = newd)
predict(r_prior, newdata = newd)

summary(r)

#install.packages('rpart.plot')
library(rpart.plot)
rpart.plot(r, type = 4)


library(randomForest)
f = randomForest(Species~., data = iris)
f
summary(f)
plot(f)

varUsed(f)
varImpPlot(f)
treesize(f)

predict(f, newdata = newd, type = 'prob')
predict(f, newdata = newd, type = 'vote', norm.votes = FALSE)

library(e1071)
s = svm(Species~., data = iris)
print(s)
table(predict(s, iris), iris$Species)

s = svm(Species~., data = iris, kernel = 'polynomial')
table(predict(s, iris), iris$Species)

s = svm(Species~., data = iris, cost = 100)
table(predict(s, iris), iris$Species)


library(class)
train = iris
test = newd

k = knn(train[, 1:4], test, train$Species, k = 5)
k

library(caret)
r = train(Species~., data=iris, method='rpart')
f = train(Species~., data=iris, method='rf')
s = train(Species~., data=iris, method='svmRadial')
k = train(Species~., data=iris, method='knn')

#names(getModelInfo())

ucla = read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')
str(ucla)
ucla$admit = factor(ucla$admit)

#r = train(admit~., data=ucla, method='rpart')
#f = train(admit~., data=ucla, method='rf')
#s = train(admit~., data=ucla, method='svmRadial')
#k = train(admit~., data=ucla, method='knn')

r = rpart(admit~., data = ucla)
par(mfrow=c(1,1), xpd=NA)
plot(r)
text(r, use.n = TRUE)
table(predict(r, ucla, type = 'class'), ucla$admit)

f = randomForest(admit~., data = ucla)
f

r = rpart(Species~., data = iris)
f = randomForest(Species~., data = iris, ntree = 3)

r_pred = predict(r, iris, type = 'class')
confusionMatrix(r_pred, iris$Species)

f_pred = predict(f, iris)
confusionMatrix(f_pred, iris$Species)

train_list = createDataPartition(y=iris$Species, p=0.6, list=FALSE)
iris_train = iris[train_list,]
iris_test = iris[-train_list,]
f=randomForest(Species~.,data = iris_train)
p=predict(f, newdata = iris_test)

control=trainControl(method = 'cv', number=5)

r=train(Species~., 
        data=iris, 
        method='rpart',
        metrics='Accuracy',
        trControl=control)

f=train(Species~., 
        data=iris, 
        method='rf',
        metrics='Accuracy',
        trControl=control)

s=train(Species~., 
        data=iris, 
        method='svmRadial',
        metrics='Accuracy',
        trControl=control)
k=train(Species~., 
        data=iris, 
        method='knn',
        metrics='Accuracy',
        trControl=control)
#confusionMatrix(f)
resamp=resamples(list(결정트리=r, 랜덤포레스트=f,svm=s,knn=k))
summary(resamp)
