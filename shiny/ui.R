library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Otto-Group-Product-Classification-Challenge"),
  sidebarLayout(
    sidebarPanel(
      helpText("Classification"),br(),
      selectInput("TypeOfGraph",
                  label = "Type:",
                  choices = c("Random Forest", "Naive Bayes", "ANN"),
                  selected = "Random Forest"),br(),
      conditionalPanel(
        condition = "input.TypeOfGraph == 'Random Forest'",
        selectInput("RF_type",
                    label = "Display: ",
                    choice = c("Random Forest", "Importance of Features"),
                    selected = "Random Forest")
      ),br(),
      conditionalPanel(
        condition = "input.RF_type == 'Importance of Features'",
        sliderInput(inputId="RF_filter", 
                    label="Select the number of features to be displayed(ordered by importance):", 
                    min=1, max=93, value=93, step=1)
      )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)