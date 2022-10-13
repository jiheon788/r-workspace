# 2022.05.30
# 빅데이터개론 및 분석 실습 13주차
# e비즈니스학과 201823871 박지헌
getwd()
setwd('C:/Users/82104/my-workspace/R-workspace/')
rm(list = ls())
#install.packages("arulesViz") 
library(neuralnet)
library(party)
library(arules)
library(arulesViz)

#install.packages('party')
options("width"=500)
data <- read.csv("Concrete_Data.csv", header = T, fileEncoding="EUC-KR")
idxs <- sample(1:nrow(data), as.integer(0.7*nrow(data)))
traindata <- data[idxs,]
testdata <- data[-idxs,]
c("전체자료"=nrow(data), "학습자료"=nrow(traindata), "검정자료"=nrow(testdata))

normalize <- function(x) { # 정규화 함수
  return((x-min(x)) / (max(x)-min(x))) # 결괏값 내보내기
}
norm_testdata <- as.data.frame(lapply(testdata, normalize))
norm_traindata <- as.data.frame(lapply(traindata, normalize))
norm_traindata$Strength <- traindata$Strength # 반응 변수 원래 데이터로 변환

model <- ctree(Strength ~., traindata )
new <- data.frame(실젯값= testdata$Strength)
new$Strength <- predict(model, testdata) 
cor(new$실젯값, new$Strength) #accuracy

model <- ctree(Strength ~., norm_traindata )
new <- data.frame(실젯값= norm_testdata$Strength)
new$Strength <- predict(model, norm_testdata) 
cor(new$실젯값, new$Strength) #accuracy

head(new)


model <- neuralnet(Strength
                   ~ Cement+Slag+Ash+Water+Superplasticizer+Coarseagg+Fineagg+Age, norm_traindata )
new <- data.frame(실젯값= testdata$Strength) # 실제값
new$Strength = compute(model, norm_testdata[-length(norm_testdata)])$net.resul

cor(new$실젯값, new$Strength) #accuracy

data <- read.csv("mart_data.csv", header = T, fileEncoding="EUC-KR")
write.csv(data[-1], file = "basket_data.csv", row.names = FALSE, quote = FALSE)
                 # 거래번호 제거된 파일은 거래 형식의 데이터 구조로 생성
data.trans <- read.transactions("basket_data.csv", format = "basket", sep=",", skip = 1)
inspect(data.trans)
itemFrequencyPlot(data.trans, topN=20, type="absolute")
rules <- apriori(data.trans, parameter = list(supp = 0.001, conf = 0.8)) # 연관규칙 생성
rules
inspect(rules[1:20]) # 상위 20개의 연관 규칙들
rules <- sort(rules, by="confidence", decreasing=TRUE) 
rules <- apriori(data.trans, parameter=list(supp=0.001,conf = 0.08), appearance
                 = list(default="lhs",rhs="전지우유"))
rules <- sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])
plot(rules[1:50], method="graph", main="50개의 연관규칙")
plot(rules, method = "grouped") 


data <- read.csv("data.csv", header = T, fileEncoding="EUC-KR")
#데이터프레임 형식으로 저장된 자료
data <- subset(data, select=c(성별, 연령대, 직업, 주거지역)) 
data$성별 <- as.numeric(as.factor(data$성별))
data$연령대 <- as.numeric(as.factor(data$연령대))
data$직업 <- as.numeric(as.factor(data$직업))
data$주거지역 <- as.numeric(as.factor(data$주거지역))
clu_data <- subset(data, select=c(성별, 연령대, 직업, 주거지역))
clu_result <- kmeans(clu_data, 3)
clu_result 
