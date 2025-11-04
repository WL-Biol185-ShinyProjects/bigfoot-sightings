# Load required libraries
library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Weather Map - Bigfoot Sightings"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Weather Layers"),
      checkboxGroupInput("layers",
                         "Select layers to display:",
                         choices = c("High Temperature" = "temp_high",
                                     "Low Temperature" = "temp_low",
                                     "Precipitation" = "precip",
                                     "Wind" = "wind",
                                     "Visibility" = "visibility"),
                         selected = "temp_high"),
      
      hr(),
      
      h4("Time Filter"),
      sliderInput("date_range",
                  "Select Date Range:",
                  min = as.Date("1980-01-01"),
                  max = as.Date("2020-12-31"),
                  value = c(as.Date("1980-01-01"), as.Date("2020-12-31")),
                  timeFormat = "%b %Y",
                  step = 30),
      
      hr(),
      
      h4("Legend"),
      htmlOutput("legend_info")
    ),
    
    mainPanel(
      width = 9,
      leafletOutput("map", height = "700px")
    )
  )
)
