# 2022.04.11 
# 빅데이터개론 및 분석 6주차 실습
# e비즈니스학과 201823871 박지헌
getwd()
setwd('C:/Users/82104/my-workspace/R-workspace/')
rm(list = ls())

sub1 <- c(14,16,12,20,8,6,12,18,16,10)
sub2 <- c(18,14,14,16,10,12,10,20,14,14)
sub3 <- c(44,38,30,48,42,50,36,52,54,32)
score <- data.frame(sub1,sub2,sub3)
score

total<- apply(score, 1, sum);total
scoreSet <- cbind(score, total);scoreSet

result <- c()

for(i in 1:nrow(scoreSet)){
  if(scoreSet[i,1] < 20*0.4 |
     scoreSet[i,2] < 20*0.4 |
     scoreSet[i,3] < 60*0.4){
    result[i] <- 'fail'
  }else if(scoreSet[i,4] >= 60){
    result[i] <- 'pass'
  }else{
    result[i] <- 'fail'
  }
  cat(i, '번째 응시생은 ', result[i], '입니다.\n')
}


source('myfunc.R')

sub1 <- c(14,16,12,20,8,6,12,18,16,10)
sub2 <- c(18,14,14,16,10,12,10,20,14,14)
sub3 <- c(44,38,30,48,42,50,36,52,54,32)
score <- data.frame(sub1,sub2,sub3)
score

result <- determine(score);result

#install.packages('Stat2Data')
#library(Stat2Data)
#data(ChildSpeaks)

ChildSpeaks <- read.csv('dataset-28379.csv', header = T, encoding = 'UTF-8')

ChildSpeaks[which(ChildSpeaks$Age < 9), 'm1'] <- 5
ChildSpeaks[which(ChildSpeaks$Age >= 9 & ChildSpeaks$Age < 15), 'm1'] <- 4
ChildSpeaks[which(ChildSpeaks$Age >= 15 & ChildSpeaks$Age < 21), 'm1'] <- 3
ChildSpeaks[which(ChildSpeaks$Age >= 21 & ChildSpeaks$Age < 27), 'm1'] <- 2
ChildSpeaks[which(ChildSpeaks$Age >= 27), 'm1'] <- 1

ChildSpeaks[which(ChildSpeaks$Gesell >= 130), 'm2'] <- 5
ChildSpeaks[which(ChildSpeaks$Gesell >= 110 & ChildSpeaks$Gesell < 130), 'm2'] <- 4
ChildSpeaks[which(ChildSpeaks$Gesell >= 90 & ChildSpeaks$Gesell < 110), 'm2'] <- 3
ChildSpeaks[which(ChildSpeaks$Gesell >= 70 & ChildSpeaks$Gesell < 90), 'm2'] <- 2
ChildSpeaks[which(ChildSpeaks$Gesell < 70), 'm2'] <- 1

ChildSpeaks$total <- ChildSpeaks$m1 + ChildSpeaks$m2

ChildSpeaks[which(ChildSpeaks$total < 3), 'result'] <- '매우느림'
ChildSpeaks[which(ChildSpeaks$total >= 3 & ChildSpeaks$total < 5), 'result'] <- '느림'
ChildSpeaks[which(ChildSpeaks$total >= 5 & ChildSpeaks$total < 7), 'result'] <- '보통'
ChildSpeaks[which(ChildSpeaks$total >= 7 & ChildSpeaks$total < 9), 'result'] <- '빠름'
ChildSpeaks[which(ChildSpeaks$total >= 9), 'result'] <- '매우빠름'

ChildSpeaks
ChildSpeaks[which.min(ChildSpeaks$total),]

library(reshape2)
data("tips")
unique(tips$sex)

source('myfunc.R')
meanbycol.tip('sex')
meanbycol.tip('smoker')

source('myfunc.R')
tips.new <- categorize.tip(tips)
head(tips.new)

favorite <- c('WT', 'SM', 'SP', 'FA','SP', 'FA','SM', 'FA')
ds <- table(favorite)

barplot(ds, main = 'fs')
barplot(ds, main = 'fs', col='blue')
barplot(ds, main = 'fs', col='blue', horiz = TRUE)

age.a <- c(13707,10974,7979,5000,4250)
age.b <- c(17540,29701,36209,33947,24487)
age.c <- c(991,2195,5366,12980,19007)
ds <- rbind(age.a,age.b,age.c)
colnames(ds) <- c('1970','1990','2010','2030','2050')

barplot(ds,
        col=c('green','blue','yellow'),
        beside = TRUE,
        legend.text = T)

ha <- c(54659,61028,53307,46161,54180)
he <- c(31215,29863,32098,39684,29707)
mc <- c(15104,16133,15222,13208,9986)
vs <- c(13470,14231,13401,13552,13193)
bs <- c(16513,14947,15112,14392,17091)
ds <-rbind(ha,he,mc,vs,bs)
colnames(ds) <- c('19.1Q', '19.2Q', '19.3Q', '19.4Q', '20.1Q')
barplot(ds)

data("Diamonds")
ds <- Diamonds$PricePerCt

color <- rep('yellow',9)
color[3] <- 'blue'
hist(ds, breaks = 9, xlab = '가격', ylab = '빈도수', las = 1,
     col = color)
