# 2022.05.01 
# 빅데이터개론 및 분석 8주차 실습
# e비즈니스학과 201823871 박지헌
getwd()
setwd('C:/Users/82104/my-workspace/R-workspace/')
rm(list = ls())

plot(cars)
m = lm(dist ~ speed, data=cars)
coef(m)
abline(m)
n1 = data.frame(speed=21.5)
predict(m,n1)
n2 = data.frame(speed=c(25,26,27,28,29,39))
predict(m,n2)
plot(n2$speed, predict(m,n2))
abline(m)

str(women)
m=lm(weight~height,data=women)

coef(m)
plot(women)
abline(m)
summary(m)

m = lm(Volume~Girth+Height,data=trees)
m

s=scatterplot3d(trees$Girth,trees$Height,trees$Volume,pch=20,type='h',angle=55)

df = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/haberman/haberman.data",header = FALSE)
names(df)=c('age','ap_year','nnodes','suvival')
str(df)
df$suvival <- factor(df$suvival)
m=glm(suvival~age+ap_year+nnodes,data=df,family = binomial)
coef(m)
deviance(m)

df=read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')
str(df)
plot(df)
m=glm(admit~.,data=df,family = binomial)
coef(m)
deviance(m)
n=data.frame(gre=376,gpa=3.6,rank=3)
predict(m,newdata = n,type = 'response')

library(survival)
str(colon)
m=glm(status~.,data=colon,family = binomial)
coef(m)
deviance(m)

