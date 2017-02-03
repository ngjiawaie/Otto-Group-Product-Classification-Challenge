library(nnet)
library(pROC)
library(caret)

data <- read.csv(file = "train.csv", header = TRUE)
data <- data[,-1]

ind <- sample(1:nrow(data), floor(nrow(data)*0.3))
test <- data[ind,]
train <- data[-ind,]

fit<-nnet(target ~ ., train[,-1], size = 3, rang = 0.1, decay = 5e-4, maxit = 500) 

#predict on the test data
predicted<- predict(fit,test[1:93],type="class")
(conf <- table(pred=predicted, true=test$target))
confusionMatrix(conf)

#ROC
predicted<- predict(fit,test[,-94],type="raw")
multi <- multiclass.roc(test$target, predicted[,1])
auc(multi)
