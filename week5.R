# 2022.04.04 
# 빅데이터개론 및 분석 5주차 실습
# e비즈니스학과 201823871 박지헌
getwd()
setwd('C:/Users/82104/my-workspace/R-workspace/')
rm(list = ls())

dim(iris)
nrow(iris)
colnames(iris)
head(iris)
str(iris)
levels(iris[,5])
table(iris[,"Species"])
z<-matrix(1:20,nrow=4,ncol = 5);z
t(z)
head(subset(iris, Species=='setosa'))
z * 2
class(iris)
is.data.frame(iris)
is.matrix(state.x77)

class(trees)
str(trees)

girth.mean <- mean(trees$Girth);girth.mean
candidate <- subset(trees, Girth >= girth.mean &
                      Height > 80 &
                      Volume > 50) ;candidate
nrow(candidate)

#install.packages("reshape2")
library(reshape2)

str(tips)
colnames(tips)
table(tips$day)

dinner <- subset(tips, time == 'Dinner')
lunch <- subset(tips, time == 'Lunch')

table(dinner$day)
table(lunch$day)

colMeans(dinner[c('total_bill', 'tip', 'size')])
colMeans(lunch[c('total_bill', 'tip', 'size')])

tip.rate <- tips$tip / tips$total_bill
cat('평균 팁의 비율: ', mean(tip.rate) * 100, '%')

#install.packages("svDialogs", dependencies=TRUE, INSTALL_opts =c('--no-lock'))
#install.packages('svDialogs')
library(svDialogs)
user.input<-dlgInput('Input income')$res;user.input

income<-as.numeric(user.input)
income

tax<- income*0.05
cat('세금: ', tax)

height <- dlgInput('Input height')$res
weight <- dlgInput('Input weight')$res

height<-(as.numeric(height))/100
bmi<-(as.numeric(weight))/(height^2)

cat('입력한 키는 ', height*100, 'cm, 몸무게는 ', weight, 'kg입니다.\n', sep="")
cat('BMI는 ', bmi, '입니다.\n', sep="")

getwd()
air <- read.csv('airquality.csv', header = T, encoding = 'UTF-8')
head(air)
class(air)

my.iris<-subset(iris, Species=='setosa')
#write.csv(my.iris, 'myIris.csv', row.names = F)

library(ggplot2)
str(diamonds)

diamonds.new <- subset(diamonds,cut == 'Premium' &
                         carat >= 2)
write.csv(diamonds.new, 'sDiamonds.csv', row.names = F)
diamonds.load<-read.csv('sDiamonds.csv',header = T)

diamonds.new <- subset(diamonds.load, color != 'D')

print('begin work')
a <- 10; b<-20
sink('result.txt', append = T)
cat('a+b=',a+b,'\n')
cat('a*b=',a*b,'\n')
sink()

carprice.new <- read.csv('carprice.csv',header = T)
str(carprice.new)

input.type <- 'Sporty'
input.city <- 5

result <- subset(carprice.new, Type == input.type &
                   MPG.city >= input.city)

print(result)
sink('search.txt', append = T)
print(result)
sink()

write.csv(result, 'search.csv', row.names=F)


purchase <- 200000
type<-NULL
ratio<-NULL
if(purchase>=300000){
  type<-'p'  
  tario<-0.07
}else if(purchase>=200000){
  type<-'g'
  ratio<-0.05
}else{
  type<-'f'
  ratio<-'0.01'
}
cat('고객님은 ',type,'회원으로 구매액의',ratio*100,'%가 적립')


