#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("PredictionModel.R")

start_time <- Sys.time()
p <- get_predictor()
end_time <- Sys.time()
print(end_time - start_time)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$predictedTerm <- renderText(paste(tolower(input$text), p(tolower(input$text))))

})
