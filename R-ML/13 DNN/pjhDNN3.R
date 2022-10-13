rm(list = ls())
library(tensorflow)
library(keras)

Tra <- read.csv("Tra1.csv",header = T)
Val <- read.csv("Val1.csv",header = T)
Tes <- read.csv("Tes1.csv",header = T)

#D.tra <- Tra[,-c(1)]
#D.val <- Val[,-c(1)]
#D.tes <- Tes[,-c(1)]

Tra$Class <- ifelse(Tra$Class==2,0,1)
Val$Class <- ifelse(Val$Class==2,0,1)
Tes$Class <- ifelse(Tes$Class==2,0,1)

x_Tra <- as.matrix(subset(Tra, select = -c(Record,Class)))
y_Tra <- as.matrix(Tra$Class)
x_Val <- as.matrix(subset(Val, select = -c(Record,Class)))
y_Val <- as.matrix(Val$Class)
x_Tes <- as.matrix(subset(Tes, select = -c(Record,Class)))
y_Tes <- as.matrix(Tes$Class)

wSeed <- 13579
tf$random$set_seed(wSeed)

dnnH4 <- keras_model_sequential() %>%
  layer_dense(units = 20,input_shape = 9) %>%
  layer_dense(units = 30,activation = 'relu') %>%
  layer_dense(units = 15,activation = 'relu') %>%
  layer_dense(units = 7,activation = 'relu') %>%
  layer_dense(units = 1,activation = 'sigmoid')

Rate <- 0.05
dnnH4 %>% compile(optimizer=optimizer_sgd(lr=Rate),
                  loss='binary_crossentropy',
                  metrics=c('accuracy'))


for(i in 1:30) {
  nEpochs <- 10
  dnnH4 %>% fit(x_Tra,y_Tra,epochs=nEpochs,verbose=0)
  accuTra <- dnnH4 %>% evaluate(x_Tra,y_Tra,verbose=0)
  accuVal <- dnnH4 %>% evaluate(x_Val,y_Val,verbose=0)
  Upto <- nEpochs*i
  Accu.tra <- round(accuTra[[2]]*100,digits = 2)
  Accu.val <- round(accuVal[[2]]*100,digits = 2)
  cat(sprintf('\n Epoch : %5d Train : %6.2f%% Valid : %6.2f%%', Upto, Accu.tra,Accu.val))
  
}




nEpochs <- 100
dnnH3 %>% fit(x_Tra,y_Tra,epochs=nEpochs,verbose=1)

accuTra <- dnnH3 %>% evaluate(x_Tra,y_Tra,verbose=0)
accuTra
accuTra$accuracy
accuTra[[2]]

accuVal <- dnnH3 %>% evaluate(x_Val,y_Val,verbose=0)
accuVal[[2]]
