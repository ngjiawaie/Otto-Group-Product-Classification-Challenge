#Reference: http://stats.stackexchange.com/questions/71700/how-to-draw-roc-curve-with-three-response-variable/110550#110550
#Reference: https://chandramanitiwary.wordpress.com/2014/03/17/r-tips-part2-rocr-example-with-randomforest/
library(ROCR)
library(tree)
library(randomForest)
library(caret)

data <- read.csv("train.csv")
data <- data[,-1]
ind <- sample(1:nrow(data), floor(nrow(data)*0.3))
test <- data[ind,]
train <- data[-ind,]

train <- downSample(x = data[, -ncol(train)],y = data$target)
train$target <- train$Class
train$Class <- NULL
lvls = levels(train$target)

aucs = c()
plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
     ylab='True Positive Rate',
     xlab='False Positive Rate',
     bty='n')
legend(0.7, 0.8, legend=lvls, col= 2:10, lwd=2, cex=0.8)

for (type.id in 1:9) {
  type = as.factor(train$target == lvls[type.id])
  
  tree_model <- randomForest(type ~ ., data = train, importance=TRUE, mtry=19) 
  tree_predict <- predict(tree_model, test, type = "prob")[,2]
  
  actual.class = test$target == lvls[type.id]
  pred = prediction(tree_predict, actual.class)
  nbperf = performance(pred, "tpr", "fpr")
  
  roc.x = unlist(nbperf@x.values)
  roc.y = unlist(nbperf@y.values)
  lines(roc.y ~ roc.x, col=type.id+1, lwd=2)
  nbauc = performance(pred, "auc")
  nbauc = unlist(slot(nbauc, "y.values"))
  aucs[type.id] = nbauc
}

lines(x=c(0,1), c(0,1))
mean(aucs)
#----------------------NEW---------------------#
install.packages("pROC")
library(pROC)
tree_model <- randomForest(target ~ ., data = train, importance=TRUE, mtry=9) 
tree_predict <- predict(tree_model, test[,-94], type = "prob")[,2]

multi <- multiclass.roc(test$target, tree_predict)
auc(multi)

