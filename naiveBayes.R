#install.packages("e1071")
set.seed(1)
library(e1071)
library(ggplot2)

data <- read.csv("train.csv")
data <- data[,-1]
data <- as.data.frame(lapply(data, function(x) as.factor(x)))
ggplot(data, aes(x = factor(target))) + geom_bar(stat = "count")

test <- data[sample(1:nrow(data), floor(nrow(data)*0.3)),]
train <- downSample(x = data[, -ncol(data)],y = data$target)

#Naive Bayes
classifier <- naiveBayes(Class ~., train,laplace=1)
classifier

#Confusion matrix
prediction.naivebayes <- predict(classifier,test[,-(ncol(test))])
(conf <- table(pred=prediction.naivebayes, true=test$target))
mean(prediction.naivebayes==test$target)

library(caret) 
f.conf <- confusionMatrix(conf)
f.conf
