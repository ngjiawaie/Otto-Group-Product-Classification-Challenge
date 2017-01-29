#clear existing data
rm(list=ls())
library(ISLR)
library(tree)
library(rpart)
set.seed(5)

# import data
train_data <- read.csv("train.csv")
test_data <- read.csv("test.csv")

#check data
dim(train_data)
dim(test_data)
head(train_data)
head(test_data)
#test_data no "target", hence train and test data need to be created from train_data

#delete id column first, don't want t to be included
train_data <- train_data[,-1]

#create train and test data from train_data
train <- sample(1:nrow(train_data),nrow(train_data)/2)
test <- -train
train_d <- train_data[train,]
test_d <- train_data[test,]
test_target <- test_d$target

#try with tree
tree_model <- tree(target ~., train_d) 
summary(tree_model)
plot(tree_model)
text(tree_model, pretty = 0)
#printcp(tree_model)

#test the model by using predict
tree_predict <- predict(tree_model, test_d, type="class")
mean(tree_predict != test_target) #0.4144607, abit high

#prune, start with cross validation
set.seed(3)
cv_tree <- cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)
plot(cv_tree$size , cv_tree$dev, type="b")
#best is 9

#prune
prune_model <- prune.misclass(tree_model, best = 9)
plot(prune_model)
text(prune_model, pretty=0)

#check prune_model
tree_predict <- predict(prune_model, test_d, type="class")
mean(tree_predict != test_target)
#nothing is pruned, cause initial tree start with 9(lowest error rate) alrdy

#try with rpart to see if it delivers a better value
set.seed(4)
tree_model2 <- rpart(target ~., data = train_d, method="class") 
summary(tree_model2)
plot(tree_model2)
text(tree_model2, pretty = 0)
printcp(tree_model2)
plotcp(tree_model2, upper="size")

tree_predict2 <- predict(tree_model2, test_d, type="class")
mean(tree_predict2 != test_target)
#0.387763, lower compare to tree

#prune tree according to the cp of lowest xerror(lowest error rate)
prune_tree2<- prune(tree_model2, cp = tree_model2$cptable[which.min(tree_model2$cptable[,"xerror"]),"CP"])
printcp(prune_tree2)
plot(prune_tree2,uniform=TRUE)
text(prune_tree2,pretty = 0)

#check how the predict perform
tree_predict2 <- predict(prune_tree2, test_d, type="class")
mean(tree_predict2 != test_target)
#0.387763
