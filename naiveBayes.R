#install.packages("e1071")
set.seed(1)
library(e1071)

data <- read.csv("train.csv")
#Data needs to be in factor before dumping in
data <- as.data.frame(lapply(data, function(x) as.factor(x)))

#Sampling for training and test sets 4:1
trainingSize <- floor(nrow(data)*0.8)
train <- sample(1:nrow(data), trainingSize)
test <- data[-train,]

#Naive Bayes
classifier <- naiveBayes(target ~.-id, data,laplace=3, subset=train)
classifier

#Confusion matrix
prediction.naivebayes <- predict(classifier,test[,-(ncol(test))])
(conf <- table(prediction.naivebayes, test$target))

library(caret) 
f.conf <- confusionMatrix(conf)
f.conf
