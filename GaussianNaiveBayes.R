#library(tidyverse)
#library(cvms)

## Read the data 
#HEADER DECLARED AS FALSE
xTrain=read.csv("ecoli_xTrain.csv")
yTrain=read.csv("ecoli_yTrain.csv")
xTest=read.csv("ecoli_xTest.csv")
yTest=read.csv("ecoli_yTest.csv")

#Gaussian Naive Bayes
prior <- function(yTrain){
  prob <- table(yTrain)/(nrow(yTrain))
  GNBprior <- c(as.numeric(prob))
  p <- t(t(GNBprior))
  #print(p)
  return(p)
}

likelihood <- function(xTrain, yTrain){
  m1 <- as.numeric(by(data = xTrain$V1, INDICES = yTrain$V1, FUN = mean))
  m2 <- as.numeric(by(data = xTrain$V2, INDICES = yTrain$V1, FUN = mean))
  m3 <- as.numeric(by(data = xTrain$V3, INDICES = yTrain$V1, FUN = mean))
  m4 <- as.numeric(by(data = xTrain$V4, INDICES = yTrain$V1, FUN = mean))
  m5 <- as.numeric(by(data = xTrain$V5, INDICES = yTrain$V1, FUN = mean))
  M <- rbind(c(m1),c(m2),c(m3),c(m4),c(m5))
  
  v1 <- sqrt(as.numeric(by(data = xTrain$V1, INDICES = yTrain$V1, FUN = var)))
  v2 <- sqrt(as.numeric(by(data = xTrain$V2, INDICES = yTrain$V1, FUN = var)))
  v3 <- sqrt(as.numeric(by(data = xTrain$V3, INDICES = yTrain$V1, FUN = var)))
  v4 <- sqrt(as.numeric(by(data = xTrain$V4, INDICES = yTrain$V1, FUN = var)))
  v5 <- sqrt(as.numeric(by(data = xTrain$V5, INDICES = yTrain$V1, FUN = var)))
  V <- rbind(c(v1),c(v2),c(v3),c(v4),c(v5))
  
  mean_var <- list("Mean" = M, "Variance" = V)
  return(mean_var)
}

p <- prior(yTrain)
meanVar <- likelihood(xTrain, yTrain)

naiveBayesClassify <- function(xTest, M, V, p){
  t <- rep(0,ncol(xTest))
  for(i in 1:nrow(xTest)){
    py_x <- rep(1,length(p))
    for(j in 1:length(p)){
      prod <- rep(1,length(p))
      for(k in 1:ncol(xTest)){
        prod[j] <- prod[j]*dnorm(as.vector(t(xTest[i,k])), M[k,j], V[k,j])
      }
      py_x[j] <- p[j]*prod[j]
    }
    t[i] <- which.max(py_x)
  }
  return(c(t))
}

classify <- naiveBayesClassify(xTest,meanVar$Mean,meanVar$Variance,p)

#Evaluation
#Fraction of test samples classified correctly
tp1 <- rep(0,1)
for (i in 1:length(classify)) {
  if (classify[i] == yTest[i,1])
  {
    tp1 <- tp1 + 1
  }
}
samples <- length(classify)
correctClass <- round(tp1/samples,3)

#Precision and Recall for class 1
class1orig <- rep(0, nrow(yTest))
for (i in 1:nrow(yTest)) {
  if(yTest[i,1] == 1){
    class1orig[i] = 1
  }
  else {
    class1orig[i] = 0
  }
}

class1pred <- rep(0, length(classify))
for (i in 1:length(classify)) {
  if(classify[i]== 1){
    class1pred[i] = 1
  }
  else {
    class1pred[i] = 0
  }
}

class1orig <- data.frame(class1orig)
class1pred <- data.frame(class1pred)

conf_mat <- tibble("target"= class1orig$class1orig, "prediction"= class1pred$class1pred)
basic_table <- table(conf_mat)
new_conf_mat <- confusion_matrix(targets = class1orig$class1orig, predictions = class1pred$class1pred)
TP1 <- basic_table[2,2]
FP1 <- basic_table[1,2]
FN1 <- basic_table[2,1]
precClass1 <- round(TP1/(TP1+FP1),3)
recClass1 <- round(TP1/(TP1+FN1),3)

