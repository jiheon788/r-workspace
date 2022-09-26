# 2022.06.05 
# 빅데이터개론 및 분석 프로젝트
# e비즈니스학과 201823871 박지헌
getwd()
setwd('C:/Users/82104/my-workspace/R-workspace/')
rm(list = ls())
library(moments)
library(psych)
library(caret)
library(doBy)
library(randomForest)
library(e1071)
library(rpart)

# EDA
df = read.csv('bike_seoul.csv')
str(df)
table(is.na(df))
summary(df)
df$id <- NULL

#더미
HT1=hist(df[,1], main = "hour")

df$morning <- ifelse(df$hour>=6 & df$hour <= 10, 1, 0)
df$evening <- ifelse(df$hour>=17 & df$hour <= 21, 1, 0)

df$hour <- NULL

str(df)



par(mfrow = c(3, 4))
HT1=hist(df[,1], main = "hour_bef_temperature")
HT2=hist(df[,2], main = "hour_bef_precipitation")
HT3=hist(df[,3], main = "hour_bef_windspeed")
HT4=hist(df[,4], main = "hour_bef_humidity")
HT5=hist(df[,5], main = "hour_bef_visibility")
HT6=hist(df[,6], main = "hour_bef_ozone")
HT7=hist(df[,7], main = "hour_bef_pm10")
HT8=hist(df[,8], main = "hour_bef_pm2.5")
HT9=hist(df[,9], main = "count")
HT10=hist(df[,10], main = "morning")
HT11=hist(df[,11], main = "evening")





#정규분포화
df$countLog <- log(df$count)
df$countSqrt <- sqrt(df$count)
skewness(df$count)
skewness(df$countLog)
skewness(df$countSqrt)

colnames(df)

par(mfrow = c(1, 3))
HT1=hist(df[,9], main = "count")
HT2=hist(df[,12], main = "countLog")
HT3=hist(df[,13], main = "countSqrt")

df$count <- NULL
df$countLog <- NULL

pairs.panels(df)
#모델링
## Split & Scale
colnames(df)
scale_model <- caret::preProcess(df[,-11], method = 'range')
df_scaled <- predict(scale_model, df)
head(df_scaled)

set.seed(123)
idx_train = sample(1:nrow(df_scaled), size = 800)
df_train = df_scaled[idx_train, ]
df_test = df_scaled[-idx_train, ]

formular = countSqrt~hour_bef_temperature+factor(hour_bef_precipitation)+hour_bef_windspeed+hour_bef_humidity+hour_bef_visibility+hour_bef_ozone+hour_bef_pm10+hour_bef_pm2.5+factor(morning)+factor(evening)

formular_noFactor = countSqrt~hour_bef_temperature+hour_bef_windspeed+hour_bef_humidity+hour_bef_visibility+hour_bef_ozone+hour_bef_pm10+hour_bef_pm2.5


#https://bioinformaticsandme.tistory.com/290
# 다중회귀분석
m1 = lm(formular, data = df_train)
summary(m1)
p1 = predict(m1, df_test);cor(df_test$countSqrt, p1)

m1 = update(m1, .~.-hour_bef_visibility-hour_bef_pm2.5-hour_bef_ozone)
summary(m1)
p1 = predict(m1, df_test);cor(df_test$countSqrt, p1)


# 의사결정나무
m2 = rpart(formular, data = df_train)
summary(m2)
p2 = predict(m2, df_test);cor(df_test$countSqrt, p2)
par(mfrow=c(1,1), xpd=NA)
plot(m2)
text(m2, use.n = TRUE)

# 랜덤 포레스트
    

  
m3 = randomForest(formular_noFactor, data = df_train)
summary(m3)
p3 = predict(m3, df_test);cor(df_test$countSqrt, p3)

varUsed(m3)
varImpPlot(m3)

# SVM
m4 = svm(formular, data = df_train)
summary(m4)
p4 = predict(m4, df_test);cor(df_test$countSqrt, p4)
svm

#cv
control = trainControl(method = 'cv', number=10)

s = train(formular, data=df_train, method='svmLinear', metric='RMSE',trControl=control)
sp = train(formular, data=df_train, method='svmPoly', metric='RMSE',trControl=control)
sr = train(formular, data=df_train, method='svmRadial', metric='RMSE',trControl=control)
rf = train(formular, data=df_train, method='rf', metric='RMSE',trControl=control)
r = train(formular, data=df_train, method='rpart', metric='RMSE',trControl=control)
lm = train(formular, data=df_train, method='lm', metric='RMSE',trControl=control)
k = train(formular, data=df_train, method='knn', metric='RMSE',trControl=control)

resamp = resamples(list(svm=s, svmPoly=sp, svmRadial=sr, RF=rf, decisionTree=r, regression=lm,kNN=k))
summary(resamp)
sort(resamp,decreasing = TRUE)
dotplot(resamp)
