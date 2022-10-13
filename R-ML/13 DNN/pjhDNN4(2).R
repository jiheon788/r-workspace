rm(list = ls())
#install.packages('doBy')
library(doBy)
library(tensorflow)
library(keras)

D.data <-read.csv("bCancerWC.csv",header = T)
D.data$Class <- ifelse(D.data$Class==2,0,1)

pSeedList <-c(12345,11111,22222,33333,44444,55555,66666,77777,88888,99999)

for (pSeed in pSeedList) {
  set.seed(pSeed)  
  trainingRate <- 0.5
  validationRate <- 0.3
  
  Tra<-sampleBy(formula = ~Z,frac = trainingRate,replace = F,systematic = F,data=D.data)
  tra.row <-sort(Tra$Record)
  Tra<-D.data[tra.row,]
  temp <-D.data[-tra.row,]
  Val<-sampleBy(formula = ~Z,frac = validationRate/(1-trainingRate),replace = F,systematic = F,data=temp)
  val.row <-sort(Val$Record)
  Val<-D.data[val.row,]
  Tes<-D.data[-c(tra.row,val.row),]
  
  x_Tra <- as.matrix(subset(Tra, select = -c(Record,Class)))
  y_Tra <- as.matrix(Tra$Class)
  x_Val <- as.matrix(subset(Val, select = -c(Record,Class)))
  y_Val <- as.matrix(Val$Class)
  x_Tes <- as.matrix(subset(Tes, select = -c(Record,Class)))
  y_Tes <- as.matrix(Tes$Class)
  
  wSeed <- 13579
  tf$random$set_seed(wSeed)
  
  dnnH3 <- keras_model_sequential() %>%
    layer_dense(units = 20,input_shape = 9) %>%
    layer_dense(units = 15,activation = 'relu') %>%
    layer_dense(units = 7,activation = 'relu') %>%
    layer_dense(units = 1,activation = 'sigmoid')
  
  Rate <- 0.05 # lr=Learning Rate
  dnnH3 %>% compile(optimizer=optimizer_sgd(lr=Rate),
                    loss='binary_crossentropy',
                    metrics=c('accuracy'))
  
  for(i in 1:10) {
    nEpochs <- 50
    dnnH3 %>% fit(x_Tra,y_Tra,epochs=nEpochs,verbose=0)
    accuTra <- dnnH3 %>% evaluate(x_Tra,y_Tra,verbose=0)
    accuVal <- dnnH3 %>% evaluate(x_Val,y_Val,verbose=0)
    Upto <- nEpochs*i
    Accu.tra <- round(accuTra[[2]]*100,digits = 2)
    Accu.val <- round(accuVal[[2]]*100,digits = 2)
    cat(sprintf('\n Seed : %5d Epoch : %5d Train : %6.2f%% Valid : %6.2f%%',pSeed, Upto, Accu.tra,Accu.val))
    
  }
  
}
  