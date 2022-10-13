library(tidyverse)
library(VIM)
library(cowplot)
library(ggcorrplot)
library(GGally)
library(ggthemes)
library(MASS)
library(car)
library(caret)
library(ROCR)
library(e1071)
library(gmodels)
library(splitstackshape)
data <- read.csv("ChurnData.csv",stringsAsFactors = F)
data <- data[1:1000,]

plot(data$성별)
#범주형변수 시각화
b1 <- ggplot(data,aes(x=성별,fill=이탈여부))+geom_bar(position = 'fill')
b2 <- ggplot(data,aes(x=통화량구분,fill=이탈여부))+geom_bar(position = 'fill')
b3 <- ggplot(data,aes(x=통화품질불만,fill=이탈여부))+geom_bar(position = 'fill')
plot_grid(b1,b2,b3)

# 상관관계분석
tel_cor1 <- round(cor(data[,c("서비스기간","단선횟수","총통화요금","부과요금")]),1)

tel_cor2 <- round(cor(data[,c("주간통화시간_분","야간통화시간_분","주말통화시간_분","국제통화시간_분","국내통화시간_분","총통화시간_분")]),1)
ggcorrplot(tel_cor1,title = "Correlation")+theme(plot.title = element_text(hjust = 0.5))
ggcorrplot(tel_cor2,title = "Correlation2")+theme(plot.title = element_text(hjust = 0.5))
## 총통화요금-부과요금간에만 관계??

t1 <- ggplot(data,aes(x=서비스기간,fill=이탈여부))+geom_density(alpha=0.7)+theme_minimal()+xlab("서비스기간")+ylab("Prop")+theme(legend.position = "top")

t2 <- ggplot(data,aes(x=단선횟수,fill=이탈여부))+geom_density(alpha=0.7)+theme_minimal()+xlab("단선횟수")+ylab("Prop")+theme(legend.position = "none")
t3 <- ggplot(data,aes(x=총통화요금,fill=이탈여부))+geom_density(alpha=0.7)+theme_minimal()+xlab("총통화요금")+ylab("Prop")+theme(legend.position = "none")

t4 <- ggplot(data,aes(x=부과요금,fill=이탈여부))+geom_density(alpha=0.7)+theme_minimal()+xlab("부과요금")+ylab("Prop")+theme(legend.position = "none")
t4
plot_grid(t1,t2,t3,t4,nrow = 4,ncol = 1)

ggpairs(data,columns = c(3,4,5,17,18),ggplot2::aes(color = 이탈여부)) +theme_economist()
ggpairs(data,columns = c(7,9,11,12,14,15),ggplot2::aes(color = 이탈여부)) +theme_economist()
###### 데이터분류 #명목형


str(data)
d <- data[,c(1,2,16,19,20)]
d$고객ID <- c(1:1000)
attach(d)
d$성별[성별=="남"] <- 1
d$성별[성별=="여"] <- 0
d$통화품질불만[통화품질불만==T]<-1 #논리형이라 F는 자동으로 0
d$이탈여부[이탈여부=="이탈"]<-1
d$이탈여부[이탈여부=="유지"]<-0
detach(d)
d$d.dummy.고 <- ifelse(d$통화량구분=="고",1,0)
d$d.dummy.중고 <- ifelse(d$통화량구분=="중고",1,0)
d$d.dummy.중 <- ifelse(d$통화량구분=="중",1,0)
d$d.dummy.중저 <- ifelse(d$통화량구분=="중저",1,0)
d$d.dummy.저 <- ifelse(d$통화량구분=="저",1,0)
d<-d[,-3]
finalData[,2]<-factor(finalData[,2])
finalData[,3]<-factor(finalData[,3])
finalData[,4]<-factor(finalData[,4])
finalData[,5]<-factor(finalData[,5])
finalData[,6]<-factor(finalData[,6])
finalData[,7]<-factor(finalData[,7])
finalData[,8]<-factor(finalData[,8])
finalData[,9]<-factor(finalData[,9])


###수치형 +표준화 
d_int <- data[,c(3,4,5,17,18)]
d_int <- scale(d_int)
d_time <- data[,c(7,9,11,12,14,15)]
d_time <- scale(d_time)
finalData<- cbind(d,data[,-c(1,2,16,19,20)])
finalData$성별<-as.numeric(finalData$성별)
finalData$이탈여부<-factor(finalData$이탈여부)
str(finalData)

save(finalData,file = "telData20210317.Rda")
finalData$이탈여부

load("telData20210317.Rda")
### 로지스틱회귀분석
str(finalData)
summary(finalData)
pSeed<-12345
set.seed(pSeed)
training800 <-stratified(finalData,"이탈여부",0.8)
testing200 <-finalData[!finalData$고객ID%in%training800$고객ID,]
table(finalData$이탈여부)
table(training800$이탈여부)
table(testing200$이탈여부)
training800<-training800[,-1]
model1_1 <- glm(이탈여부~.,data = training800,family = binomial(link = "logit"))
summary(model1_1)

model1_2 <- stepAIC(model1_1,direction = "both")
summary(model1_2)
formula(model1_2)
(vif_vars <- as.data.frame(vif(model1_2)))

model1_3 <- glm(이탈여부~성별+d.dummy.고+d.dummy.중저+연령+서비스기간+단선횟수+주간통화횟수+주간통화시간_분+야간통화횟수+주말통화횟수+주말통화시간_분+국내통화시간_분,data = training800,family = binomial)
summary(model1_3)
#model1_1 AIC 936.85
#model1_2 AIC 923.18 lowest 채택
#model1_3 AIC 933.06


pred <- predict(model1_2,newdata = testing200[,which(colnames(testing200)!= "이탈여부")],type = "response")
pred
summary(pred)
# check model test
pred_churn <- factor(ifelse(pred >= 0.50, "이탈", "유지"))
actual_churn <- factor(ifelse(testing200$이탈여부==1,"이탈","유지"))
pred_churn
actual_churn
table(actual_churn,pred_churn)

caret::confusionMatrix(pred_churn,actual_churn,positive = "이탈탈")
###로지스틱회귀분석 적중률 72


############ Bayes
bayDf <- finalData[,c(1,4,10:20)]
bay.train <- stratified(bayDf,"이탈여부",0.8)
bay.test <- bayDf[!bayDf$고객ID%in%bay.train$고객ID,]

trainingLabels <- bay.train$이탈여부
testingLabels <- bay.test$이탈여부
model2 <- naiveBayes(bay.train,trainingLabels)
pred2<-predict(model2,newdata=bay.test[,which(colnames(bay.test)!= "이탈여부")])
pred2
table(pred2)
CrossTable(pred2,testingLabels,prop.chisq = FALSE, prop.t = FALSE, 
           prop.r = FALSE, dnn = c('predicted', 'actual'))

pred_churn2 <- factor(ifelse(pred2==1,"Yes","No"))
caret::confusionMatrix(pred_churn2,actual_churn,positive = "No")
###bayes적중률71.5

### LDA
ldaDf <- finalData[,-c(10:20)]
lda.train <- stratified(ldaDf,"이탈여부",0.8)
lda.test <- ldaDf[!ldaDf$고객ID%in%lda.train$고객ID,]

model3 <- lda(이탈여부 ~성별+통화품질불만, data =lda.train)
model3
pred3 <- predict(model3,newdata = lda.test[,which(colnames(lda.test)!= "이탈여부")])

pred_churn3 <- factor(ifelse(pred3$class==1,"Yes","No"))
caret::confusionMatrix(pred_churn3,actual_churn,positive = "No")
### LDA적중률 69.5

###########에측
telTest <- read.csv("ChurnDataTest.csv",stringsAsFactors = F)
telTest <-telTest[,-c(21:26)]
telTest <- telTest[1:200,]
str
d2 <- telTest[,c(2,16,19,20)]
attach(d2)
d2$성별[성별=="남"] <- 1
d2$성별[성별=="여"] <- 0
d2$통화품질불만[통화품질불만==T]<-1 #논리형이라 F는 자동으로 0
detach(d2)
d2$d.dummy.고 <- ifelse(d2$통화량구분=="고",1,0)
d2$d.dummy.중고 <- ifelse(d2$통화량구분=="중고",1,0)
d2$d.dummy.중 <- ifelse(d2$통화량구분=="중",1,0)
d2$d.dummy.중저 <- ifelse(d2$통화량구분=="중저",1,0)
d2$d.dummy.저 <- ifelse(d2$통화량구분=="저",1,0)
d2<-d2[,-2]
###수치형 +표준화 
d_int2 <- telTest[,c(3,4,5,17,18,7,9,11,12,14,15)]
d_int2 <- scale(d_int2)
testDf<- cbind(d2,telTest[,-c(1,2,16,19,20)])
testDf$성별<-as.numeric(testDf$성별)
str(testDf)

testDf[,1]<-factor(testDf[,1])
testDf[,2]<-factor(testDf[,2])

testDf[,4]<-factor(testDf[,4])
testDf[,5]<-factor(testDf[,5])
testDf[,6]<-factor(testDf[,6])
testDf[,7]<-factor(testDf[,7])
testDf[,8]<-factor(testDf[,8])

str(testDf)
###로지스틱 test
test.pred <- predict(model1_2,newdata = testDf[,which(colnames(testDf)!= "이탈여부")],type = "response")
test.pred
이탈여부 <- factor(ifelse(test.pred >= 0.5, "이탈", "유지"))
이탈여부
test.pred_churn
table(이탈여부)
output <- cbind(telTest,이탈여부)
output <-output[,-20]
write.csv(output,"민호준_Churn.csv")
## bayes test
bayDf2 <- testDf[,c(3,9:19)]
test.pred2<-predict(model2,bayDf2[,which(colnames(bayDf2)!= "이탈여부")])
test.pred2
이탈여부 <- factor(ifelse(test.pred2 == 1, "이탈", "유지"))
output2 <- cbind(telTest,이탈여부)
output2 <- output2[,-20]
write.csv(output2,"박지헌_Churn.csv")
