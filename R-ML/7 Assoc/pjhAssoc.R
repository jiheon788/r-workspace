data <- read.csv("POS50.csv")

str(data)

# 데이터 일정하게
# 제품컬럼 모두 만들기 read.transaction
#거래건수 50 아이템 20개 개별아이템의 출현ㄴ횟수 245
#아이템매트릭스 밀도 245/20*50
rm(list = ls())
install.packages('arules')
library(arules)
A.data <- read.transactions("POS50.csv",sep = ',',format = 'basket',skip = 1,cols = 1)
A.data
head(A.data)
summary(A.data)
inspect(A.data)
inspect(A.data[1:5])
itemFrequencyPlot(A.data)
itemFrequencyPlot(A.data,support=0.3)
itemFrequencyPlot(A.data,topN=10)

apriori(A.data)
apriori(A.data,parameter = list(support=0.2))
apriori(A.data,parameter = list(support=0.2,confidence=0.5))
rules20 <-apriori(A.data,parameter = list(support=0.2,confidence=0.5))
inspect(rules20)
inspect(sort(rules20,by='lift'))
inspect(sort(rules20,by='lift')[1:5])
inspect(sort(rules20,decreasing = F,by='lift')[1:5])
#소다가 포함된 룰들을 추출
#소다가포홤된 룰중 콘피던스가 0.9이상인 룰들 추출

sodaR <- subset(rules20,items %in% 'soda')
inspect(sodaR)
inspect(sort(sodaR,by='lift')[1:5])

chickenR <- subset(rules20,items %in% 'chicken')#적어서 안나옴
inspect(sort(chickenR,by='lift')[1:5])

length(sodaR)#갯수세기
length(chickenR)

sodaRc9 <- subset(rules20,items %in% 'soda' & confidence>=0.9)
inspect(sodaRc9)
#소다 또는 크래커 추출하라
scR <- subset(rules20,items %in% c('soda','cracker'))
inspect(scR)
#둘다나오게한다
sANDcR <- subset(rules20,items %ain% c('soda','cracker'))
inspect(sANDcR)
#then절에 beer가포함된룰
thenbeerR <-subset(rules20,rhs %in%'beer')
inspect(thenbeerR)
#if절에 위스키또는 비어
ifliquorrR <-subset(rules20,lhs %in%c('beer','whiskey'))
inspect(ifliquorrR)
#'er'이라는 음절이포함된 (부분음절)partly in
erR <- subset(rules20,items %pin%'er')
inspect(erR)

