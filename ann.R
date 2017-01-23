setwd("C:/Users/Wayne/Desktop/Subject/R/Otto-Group-Product-Classification-Challenge")
train <- read.csv(file = "train.csv", header = TRUE)
test <- read.csv(file = "test.csv", header = TRUE)

maxs <- apply(train[,2:94],2,max)
mins <- apply(train[,2:94],2,min)

scaled.data <- as.data.frame(scale(train[,2:94],center = mins, scale = maxs - mins))
feats <- names(scaled.data)
f <- paste(feats,collapse=' + ')
f <- paste('target ~ ',f)
#Class_1 + Class_2 + Class_3 + Class_4 + Class_5 + Class_6 + Class_7 + Class_8 + Class_9 ~ 
f <- as.formula(f)
f

library(neuralnet)
library(nnet)
nn <- nnet(target ~ ., train[,-1], size = 3, rang = 0.1, decay = 5e-4, maxit = 500)

library(NeuralNetTools)

plotnet(nn, alpha=0.6)

