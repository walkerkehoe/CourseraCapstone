#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(tm)
library(RWeka)
library(parallel)

cleanseInput <- function(text){
  cleanText <- tolower(text)
  cleanText <- removePunctuation(cleanText)
  cleanText <- removeNumbers(cleanText)
  cleanText <- stripWhitespace(cleanText)
  cleanText
}

Predict <- function(x){
  textInput <- cleanseInput(x)
  
  
  if (length(unlist(strsplit(textInput," ")))>= 3){
    prediction <- quadgram[grep(textInput, quadgram$String), ][1]
    prediction <- unlist(strsplit(as.character(prediction$String)," "))
    out <- tail(prediction, n=1)
    
  } else if (length(unlist(strsplit(textInput," ")))>= 2){
    prediction <- trigram[grep(textInput, trigram$String), ][1]
    prediction <- unlist(strsplit(as.character(prediction$String)," "))
    out <- tail(prediction, n=1)
    
  } else if (length(unlist(strsplit(textInput," ")))>= 1){
    prediction <- bigram[grep(textInput, bigram$String), ][1]
    prediction <- unlist(strsplit(as.character(prediction$String)," "))
    out <- tail(prediction, n=1)
    
  } else {
    out <- as.character(unigram$String[1])
  }
  # if (is.null(out)) { 
  #   out <- as.character(unigram$String[1])
  # }
  out
}

unigram <- readRDS(file="./unigram.RData")
bigram <- readRDS(file="./bigram.RData")
trigram <- readRDS(file="./trigram.RData")
quadgram <- readRDS(file="./quadgram.RData")
  
  
function(input, output) {
  
  output$value <- renderPrint({ Predict(input$text) })
  
}
