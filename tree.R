#clear existing data
rm(list=ls())
library(ISLR)
library(tree)
library(randomForest)
library(ggplot2)
library(dplyr)
library(caret)
set.seed(5)

# import data
train_data <- read.csv("train.csv")

#check data
dim(train_data)
head(train_data)
str(train_data)
#test_data no "target", hence train and test data need to be created from train_data

#delete id column first, don't want t to be included
train_data <- train_data[,-1]

#create train and test data from train_data
ind <- sample(1:nrow(train_data), floor(nrow(train_data)*0.3))
test_d <- train_data[ind,]
train_d <- train_data[-ind,]

test_target <- test_d$target
#downsampling is use to make sure balanced "target" class is obtained
train_d <- downSample(x = train_data[, -ncol(train_data)],y = train_data$target)
table(train_d$Class)

#try with tree
x <- train_d
x$Class <- NULL
tunemtry <- tuneRF(x,train_d$Class, stepFactor=1.5, plot=TRUE,improve=0.01)
#tunemtry shows the best mtry is 19
tree_model <- randomForest(Class ~ ., train_d, importance=TRUE, mtry=19) 
print(tree_model)
plot(tree_model, main="Tree")

#test the model by using predict
tree_predict <- predict(tree_model, test_d[,-94], type="class")
mean(tree_predict != test_target) #around 0.17

#view confusion Matrix
confusionMatrix(tree_predict,test_target)

#importance
imp <- importance(tree_model, type=1)
ShowImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])
#ggplot, arranged according to the importance value
p <- ggplot(ShowImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#32CD32") + #identity represent values
  coord_flip() + 
  xlab("Features") +
  ylab("Importance") + 
  ggtitle("Importance of Features \n") +
  theme(plot.title=element_text(size=20))
p #convert to pdf and increase the length to get a clearer view

#too many, we only interested in top 10 features
imp.ordered <- arrange(ShowImportance, -Importance)
imp.top10 <- imp.ordered[1:10,]
p_mini <- ggplot(imp.top10, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#32CD32") + #identity represent values
  coord_flip() + 
  xlab("Features") +
  ylab("Importance") + 
  ggtitle("Importance of Features \n") +
  theme(plot.title=element_text(size=20))
p_mini


