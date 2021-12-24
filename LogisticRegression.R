#library(tidyverse)
#library(cvms)

## Read the data 
#HEADER DECLARED AS FALSE
new_xTrain=read.csv("C:/Users/Admin/Desktop/SEM1/DataMining/Project 1/part 3/ecoli_new.xTrain.csv", header=FALSE)
new_yTrain=read.csv("C:/Users/Admin/Desktop/SEM1/DataMining/Project 1/part 3/ecoli_new.yTrain.csv", header=FALSE)
new_xTest=read.csv("C:/Users/Admin/Desktop/SEM1/DataMining/Project 1/part 3/ecoli_new.xTest.csv", header=FALSE)
new_yTest=read.csv("C:/Users/Admin/Desktop/SEM1/DataMining/Project 1/part 3/ecoli_new.yTest.csv", header=FALSE)

#Logistic Regression
sigmoidProb <- function(y, x, w){
  if(y==0)
  {
    py_x <- 1/(1+exp(sum(-x * w)))
  }
  else
  {
    py_x <- 1-(1/(1+exp(sum(-x * w))))
  }
  return(py_x)
}

logisticRegressionWeights <- function(xTrain, yTrain, w0, nIter){
  weights <- rep(w0,ncol(xTrain))
  for(k in seq(1, nIter)){
    for(i in 1:ncol(xTrain)){
      gradient <- rep(0,1)
      for(j in 1:nrow(xTrain)){
        gradient <- gradient + (xTrain[j,i] * (yTrain[j,1]) - (xTrain[j,i] * sigmoidProb(1,xTrain[j,], weights)))
      }
      weights[i] <- weights[i] - (0.1 * gradient)
    }
  }
  return(weights)
}

w <- logisticRegressionWeights(new_xTrain, new_yTrain, 0, 50)

logisticRegressionClassify <- function(xTest, w){
  out <- rep(0,nrow(xTest))
  for(i in 1:nrow(xTest)){
    prob <- sigmoidProb(1,xTest[i,], w)
    if (prob >= 0.5) 
    {
      out[i] = 1
    }
    else 
    {
      out[i] = 0
    }
  }
  return(c(out))
}

classify2 <- logisticRegressionClassify(new_xTest,w)

#Evaluation
#Fraction of test samples classified correctly
tp_log <- rep(0,1)
for (i in 1:length(classify2)) {
  if (classify2[i] == new_yTest[i,1])
  {
    tp_log <- tp_log + 1
  }
}
samplesLog <- length(classify2)
correctClassLog <- round(tp_log/samplesLog,3)

#Precision and Recall for class 1
class1origLog <- data.frame(new_yTest)
class1predLog <- data.frame(classify2)

conf_matLog <- tibble("target"= class1origLog$V1, "prediction"= class1predLog$classify2)
basic_tableLog <- table(conf_matLog)
new_conf_matLog <- confusion_matrix(targets = class1origLog$V1, predictions = class1predLog$classify2)
TPLog <- basic_tableLog[2,2]
FPLog <- basic_tableLog[1,2]
FNLog <- basic_tableLog[2,1]
precClassLog1 <- round(TPLog/(TPLog+FPLog),3)
recClassLog1 <- round(TPLog/(TPLog+FNLog),3)
