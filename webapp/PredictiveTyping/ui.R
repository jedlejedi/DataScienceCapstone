#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    includeCSS("styles.css"),
    
    titlePanel("Predictive Typing Model Demo"),
    
    fluidRow(
        column(12, textInput("text","Type something in the text box below and click on the button", width = 450))
    ),
    fluidRow(
        column(12, submitButton("Guess the next word!"))
    ),
    fluidRow(
        column(12, textOutput("response"))
    )
))