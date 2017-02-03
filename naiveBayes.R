set.seed(1)
library(e1071)
#library(ggplot2)
library(caret)
library(klaR)
library(pROC)

data <- read.csv("train.csv")
data <- data[,-1]
data <- as.data.frame(lapply(data, function(x) as.factor(x)))
#ggplot(data, aes(x = factor(target))) + geom_bar(stat = "count")

ind <- sample(1:nrow(data), floor(nrow(data)*0.3))
test <- data[ind,]
train <- data[-ind,]

#Naive Bayes
classifier <- NaiveBayes(target ~., train,laplace=1)
classifier

#Confusion matrix
prediction.naivebayes <- predict(classifier,test[,-(ncol(test))])
(conf <- table(pred=prediction.naivebayes$class, true=test$target))
mean(prediction.naivebayes$class==test$target)

library(caret) 
f.conf <- confusionMatrix(conf)
f.conf

#ROC
nbprediction = predict(classifier, test[,-(ncol(test))], type='prob')
multi <- multiclass.roc(test$target, as.numeric(nbprediction$posterior[,1]))
auc(multi)
