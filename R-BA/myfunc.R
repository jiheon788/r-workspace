# 2022.04.11 
# 빅데이터개론 및 분석 6주차 실습 - 함수
# e비즈니스학과 201823871 박지헌
getwd()
setwd('C:/Users/82104/my-workspace/R-workspace/')
rm(list = ls())

determine <- function(score){
  total<- apply(score, 1, sum)
  scoreSet <- cbind(score, total)
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
    #cat(i, '번째 응시생은 ', result[i], '입니다.\n')
  }
  return(result)
}

meanbycol.tip <- function(colname){
  value <- unique(tips[, colname])
  result <- list()
  for(i in 1:length(value)){
    idx <- which(tips[,colname] == value[i])
    result[i] <- mean(tips[idx, 'tip'])
  }
  names(result) <- value
  return(result)
}

categorize.tip <- function(tips){
  tip_ratio <- tips$tip / tips$total_bill * 100
  
  class <- c()
  
  for(i in 1:nrow(tips)){
    if(tip_ratio[i] < 10){
      class[i] <- 1
    }else if(tip_ratio[i] < 15){
      class[i] <- 2
    }else if(tip_ratio[i] < 20){
      class[i] <- 3
    }else{
      class[i] <- 4
    }
  }
  tips.new <- cbind(tips, type = class, ratio = tip_ratio)
  return(tips.new)
}


