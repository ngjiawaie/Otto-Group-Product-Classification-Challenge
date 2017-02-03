#Reference: http://stats.stackexchange.com/questions/71700/how-to-draw-roc-curve-with-three-response-variable/110550#110550
library(ROCR)
library(klaR)

data <- read.csv("train.csv")
data <- data[,-1]
data <- as.data.frame(lapply(data, function(x) as.factor(x)))
ind <- sample(1:nrow(data), floor(nrow(data)*0.3))
test <- data[ind,]
train <- data[-ind,]
lvls = levels(train$target)

aucs = c()
plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
     ylab='True Positive Rate',
     xlab='False Positive Rate',
     bty='n')
legend(0.7, 0.8, legend=lvls, col= 2:10, lwd=2, cex=0.8)

for (type.id in 1:9) {
  type = as.factor(train$target == lvls[type.id])
  
  nbmodel = NaiveBayes(type ~ ., data=train)
  nbprediction = predict(nbmodel, test[,-(ncol(test))], type='raw')
  
  score = nbprediction$posterior[, 'TRUE']
  actual.class = test$target == lvls[type.id]
  
  pred = prediction(score, actual.class)
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
library(pROC)
nbmodel = NaiveBayes(target ~ ., data=train)
nbprediction = predict(nbmodel, test[,-(ncol(test))], type='prob')

multi <- multiclass.roc(test$target, as.numeric(nbprediction$posterior[,1]))
auc(multi)
