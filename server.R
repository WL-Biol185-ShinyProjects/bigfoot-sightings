bigfoot_data <- read.csv("bigfoot_data_wordcount_filtered_less_200.csv")

library(wordcloud)
library(tm)
library(memoise)
library(tidyverse)
library(stringr)
library(ggplot2)




function(input, output) {
  output$WordCloud <- renderPlot({
    
    removed_words <- c("like", "the", "also", "didnt", "there", "got", "this", "just", "didnt")
    
    kept_words <- sapply( 1:nrow(bigfoot_data),
                          function(n) {
                            words <- str_split(bigfoot_data$observed[n], "\\s")[[1]]
                            words <- tolower(words)
                            words <- gsub(".", "", words, fixed = TRUE)
                            paste( words[ !words %in% removed_words], collapse = " ")
                          })
    #This code created list of words that will show up on the wordcloud
    par(bg = NA)
    
    wordcloud(kept_words, 
              colors = brewer.pal(8, "Dark2"),
              max.words = input$max_words
    )
  }, height = 600, bg = "transparent")
  
  
  
  
  output$selectedPlot <- renderPlot({
    if(input$plotChoice == "Sightings by Season") {
      source("Sightings-per-season-bar.R")
      season_bar
    } else if(input$plotChoice == "Sightings by State") {
      source("sightings-per-state.R")
      state_bar
    } else if(input$plotChoice == "Sightings by Temperature") {
      source("sightings-by-temp.R")
      tempgrouped_bar
    }
  })
}