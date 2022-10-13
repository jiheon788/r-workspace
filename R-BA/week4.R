# 2022.03.28 
# 빅데이터개론 및 분석 4주차 실습
# e비즈니스학과 201823871 박지헌
getwd()
setwd('C:/Users/82104/my-workspace/R-workspace/')
rm(list = ls())

v1 <- c(1,2,3,4)
v2 <- c('John', 'Jane', 'Tom')
v3 <- c(v1, v2); v3

v1 * 2

d<-1:9
d >=5
d[d>5]
sum(d>5)
sum(d[d>5])
d==5

condi = d>5 & d<8
d[condi]

espresso <- c(4,5,3,6,5,4,7)
americano <- c(63,68,64,68,72,89,94)
latte <- c(61,70,59,71,71,92,88)

sale.espresso <- 2*espresso
sale.americano <- 2.5 * americano
sale.latte <- 3.0 * latte

sale.day <- sale.espresso + sale.americano + sale.latte

names(sale.day) <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
sale.day

sum(sale.day)

sale.mean <- mean(sale.day)
names(sale.day[sale.day >= sale.mean])

bt <- c('a','b', 'b','o','ab','a')
bt.new <- factor(bt)
bt
bt.new
bt[5]
bt.new[5]
levels(bt.new)
as.integer(bt.new)
bt.new[7] <- 'b'
#bt.new[8] <- 'c'
bt.new

cafe <- list(
  espresso = c(4,5,3,6,5,4,7),
  americano = c(63,68,64,68,72,89,94),
  latte = c(61,70,59,71,71,92,88),
  price = c(2.0, 2.5, 3.0),
  menu = c('espresso', 'americano', 'latte')
)

cafe$menu <- factor(cafe$menu)
names(cafe$price) <- cafe$menu

sale.espresso <- cafe$espresso * cafe$price['espresso']
sale.americano <- cafe$americano * cafe$price['americano']
sale.latte <- cafe$latte * cafe$price['latte']

sale.day <- sale.espresso + sale.americano + sale.latte
names(sale.day)<-c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
sum(sale.day)
sale.mean <- mean(sale.day)
names(sale.day[sale.day>=sale.mean])

# 실전분석
data <- c(31,26,42,47,50,54,70,66,43,32,32,22)
names(data) <- c('m1','m2','m3','m4','m5','m6','m7','m8','m9','m10','m11','m12')
data

sum(data)

max(data)
min(data)

data - (data / 10)

data[data>50]

names(data[data>50])

length(names(data[data<50]))

data[data > data['m6']]

x <- 1:4
y <- 5:8
z <- matrix(1:20, nrow = 4, ncol = 5)

m1 <- cbind(x,y);m1
m2 <- rbind(x,y);m2
m3 <- rbind(m2,x);m3
m4 <- cbind(z,x);m4

score <- matrix(c(90,85,69,78,85,96,49,95,90,80,70,60),nrow = 4);score
rownames(score) <- c('John','Tom','Mark','Jane')
colnames(score) <- c('english','math','science')
score

burger <- matrix(c(
  514,917,11,
  533,853,13,
  566,888,10),
  nrow = 3,
  byrow = T
); burger

rownames(burger) <- c('M','L','B')
colnames(burger) <- c('kcal','na','fat')
burger

iris[,c(1:2)]
iris[,c(1,3,5)]

kcal <- c(514,533,566)
na <- c(917,853,888) 
fat <- c(11,13,10)
menu <- c('s','b','c')
burger <- data.frame(kcal,na,fat,menu)
rownames(burger) <- c('M','L','B');burger

burger['M',]
burger['M','na']
burger[,'kcal']
burger[c('M','B'),'menu']
