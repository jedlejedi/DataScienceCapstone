#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tictoc)

source("PredictionModel3GS.R")

tic("Initialisation")
p <- get_predictor3gs()
toc()

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$response <- renderText({
    
    input_text <- trimws(tolower(input$text))
    
    if(input_text == "") {
      output_text <- ""
    }
    else {
      output_text <- paste0("Is the next word \"",p(input_text),"\" ?")  
    }    
  })
  
})
