library(ISLR)
library(tree)
library(randomForest)
library(ggplot2)
library(dplyr)
set.seed(5)

# import data
train_data <- read.csv("train.csv")
test_data <- read.csv("test.csv")

train_data <- train_data[,-1]

test <- sample(1:nrow(train_data),nrow(train_data)/2)
test_d <- train_data[test,]
test_target <- test_d$target
train_d <- downSample(x = train_data[, -ncol(train_data)],y = train_data$target)
tree_model <- randomForest(Class ~ ., train_d, importance=TRUE, mtry=19)
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
    
  })
  
})
