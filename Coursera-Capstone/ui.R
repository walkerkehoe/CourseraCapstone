#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

fluidPage(
  
  # Copy the line below to make a text input box
  textInput("text", label = h3("Enter partial sentence:"), value = "Enter text..."),
  
  hr(),
  fluidRow(column(3, verbatimTextOutput("value")))
  
)
