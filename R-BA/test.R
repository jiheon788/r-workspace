# 20220321 빅데이터개론 및 분석 실습
# e비즈니스학과 201823871 박지헌
getwd()
setwd('C:/Users/82104/my-workspace/R-workspace/')
rm(list = ls())

# Test R
print('hello world')
sqrt(126*17)

# 산술연산
10 * 5 * (1 / 2)
10 * 10 * 3.14
5 * 60 *60 + 48 * 60 + 32
10000 - (1000*5 +500*3)
max(63, 95, 84, 36, 48) * 500

# 패키지 설치
#install.packages('ggplot2')
library(ggplot2)
ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width)) + geom_point()

install.packages('cowsay')
library(cowsay)
say('Hello', by = 'cat')
say('Hi', by = 'snowman')

ceiling(2.4)
ceiling(3.6)

Sys.time()

# 931 754 1029 1139 1442
# (((bmonth * 4) + 9) * 25) + bday

((931 / 25) - 9)/4
((754 / 25) - 9)/4
((1029 / 25) - 9)/4
((1139 / 25) - 9)/4
((1442 / 25) - 9)/4
# 결과: 월. 일

t <- 5000
t
print(t)
cat('합: ', t)

a <- 10 ; a
a <- 50 ; a

salt <- 50
water <- 100
result <- salt / (salt+water) * 100
cat("소금 =",salt,", 물 =",water," : 농도 = ",result,"%")

x <- c(1,2,3); x
y <- c('a','b','c'); y
z <- c(TRUE,TRUE,FALSE,TRUE); z
w <- c(1,2,3,'a','b','c'); w

d <- c(1,4,3,7,8)
d[c(1,3,5)]
d[1:3]
d[seq(1,5,2)]
d[-2]
d[-c(3:5)]

customer <- c('kim', 'lee', 'park', 'choi','seo')
deposit <- c(5000000, 4500000, 4000000, 5500000, 6000000)
rate <- c(3.5, 3, 4, 5, 4.5)
period <- c(2,2,5,7,4)

names(deposit) <- customer
names(rate) <- customer
names(period) <- customer

who <- 'kim'

sum <- deposit[who] * (1+rate[who]/100)^period[who]; sum

a <- 1:12
b <- '월'
c <- paste(a,b,sep=''); c

sales <- c(33,55,11,22,44,55,66,77,88,99,22,22)
names(sales) <- paste(1:12,'월',sep='')
sales
