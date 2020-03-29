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
    
    titlePanel("Hello Shiny!"),
    
    fluidRow(
        column(12,
               textInput("text", "Enter text here"), submitButton("Guess next term")
        ),
    ),
    fluidRow(
        column(12,
               textOutput("predictedTerm")
        )
    )
    
))