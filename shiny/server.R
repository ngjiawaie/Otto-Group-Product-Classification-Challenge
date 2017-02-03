library(ISLR)
library(tree)
library(randomForest)
library(ggplot2)
library(dplyr)
library(caret)

library(PerformanceAnalytics)
library(e1071)
library(klaR)
set.seed(5)

# import data
train_data <- read.csv("train.csv")

train_data <- train_data[,-1]

ind <- sample(1:nrow(train_data), floor(nrow(train_data)*0.3))
test_d <- train_data[ind,]
train_d <- train_data[-ind,]
test_target <- test_d$target

train_d <- downSample(x = train_data[, -ncol(train_data)],y = train_data$target)
tree_model <- randomForest(Class ~ ., train_d, importance=TRUE, mtry=19)

#naive bayes data preperation
n_data <- as.data.frame(lapply(train_data, function(x) as.factor(x)))

ind <- sample(1:nrow(n_data), floor(nrow(n_data)*0.3))
test <- n_data[ind,]
train <- n_data[-ind,]

#Naive Bayes
classifier <- NaiveBayes(target ~., train,laplace=1)
prediction.naivebayes <- predict(classifier,test[,-(ncol(test))])
conf <- table(pred=prediction.naivebayes$class, true=test$target)

#ann 
a_data <- train_data
ind_a <- sample(1:nrow(a_data), floor(nrow(a_data)*0.3))
test_a <- a_data[ind_a,]
train_a <- a_data[-ind_a,]

fit_a<-nnet(target ~ ., train_a[,-1], size = 3, rang = 0.1, decay = 5e-4, maxit = 500) 

#predict on the test data
predicted_a<- predict(fit,test_a[1:93],type="class")
conf_a <- table(pred=predicted_a, true=test$target)
#------------------------------------------------------------------End of Data Preparation


shinyServer(function(input, output) {
  
  output$plot<- renderPlot({
    
    #Initial Plot
    if(input$TypeOfGraph == "Random Forest"){
      if(input$RF_type == "Random Forest"){
        plot(tree_model, main="Tree")
      }
      else if(input$RF_type == "Importance of Features"){
        imp <- importance(tree_model, type=1)
        ShowImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])
        imp.ordered <- arrange(ShowImportance, -Importance)
        imp.top <- imp.ordered[1:input$RF_filter,]
        p_mini <- ggplot(imp.top, aes(x=reorder(Feature, Importance), y=Importance)) +
          geom_bar(stat="identity", fill="#32CD32") + #identity represent values
          coord_flip() + 
          xlab("Features") +
          ylab("Importance") + 
          ggtitle("Importance of Features \n") +
          theme(plot.title=element_text(size=20))
        p_mini
      }
    }
    else if(input$TypeOfGraph == "Naive Bayes"){
      textplot(      
        capture.output(     
          confusionMatrix(conf)  
        ),cex=0.65      
      )  
    }
    else if(input$TypeOfGraph == "ANN"){
      textplot(      
        capture.output(     
          confusionMatrix(conf_a)  
        ),cex=0.65      
      ) 
    }
    
  })
  
})
