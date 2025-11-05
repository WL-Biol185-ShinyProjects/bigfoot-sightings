library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)  # Required for heatmaps!
library(sf)
library(dplyr)
library(lubridate)

navbarPage(
  "Select a Page to Explore!", # Title of the navigation bar
  tabPanel(
    "Home Page", 
    fluidPage(
      div(style = "text-align: center;",
          tags$img(src = "bigfoot-landing-page.jpg", width = "900px"),
          tags$h1("Welcome to Bigfoot Sightings"),
          tags$h2("Have you ever wondered where Bigfoot has been found? 
        Want to know where to look next? Well, you've come to the right place!")), 
      
      tags$head(
        tags$style(HTML("
      @keyframes slide {
        0% {
          left: -100px);
        }
        100% {
          left: 100%;
        }
      }
      
      #moving-image {
        position: fixed;
        top: 50%;
        left: -100px;
        animation: slide 3.6s ease-in-out forwards;
        z-index: 9999;
      }
    "))
      ),
      
      
      tags$audio(src = "growl-and-roar-102417.mp3", 
                 autoplay = "autoplay",
                 type = "audio/mpeg"),
      
      
      tags$img(id = "moving-image", 
               src = "new-bigfoot-image-removebg-preview.png",
               width = "300px")
      
      # website: https://pixabay.com/sound-effects/search/bigfoot/
      # https://en.wikipedia.org/wiki/Bigfoot
      # https://www.shutterstock.com/search/big-foot-creature
    )
    
    
  ), #tabPanel for the main page
  
  # Word cloud for the words most used in the bigfoot sighting reports 
  tabPanel("WordCloud", 
           fluidPage(
             h2("WordCloud of Words Used to Describe BigFoot Sightings"),
             theme = shinytheme("darkly"),
             plotOutput("WordCloud", height = "auto"),
             
             hr(),
             
             div(
               style = "background-color: #2c3e50; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
               h3("What's in the Word Cloud?"),
               tags$p("Each report made of a Bigfoot sighting had a section available to give an explanation of what the person saw."),
               tags$p("This word cloud displays the most frequently used words in these descriptions."),
               tags$p("Let's find out what all of these stories from first eye witnesses have in common...")
             ),
             
             sidebarLayout(
               sidebarPanel(
                 sliderInput("max_words",
                             "Select Amount of Words Shown:",
                             min = 1,
                             max = 100,
                             value = 50,
                             step = 1)
               ),
               mainPanel(
                 plotOutput("wordcloud")
               ) 
             )
             
           )#fluidPage for wordcloud
  ),#tabPanel for the word cloud
  
  tabPanel("Visualizations for Bigfoot Sightings",
           fluidPage(
             h3("Multiple Visualizations of Bigfoot Sightings"), 
             selectInput("plotChoice" , "Choose a Plot:",
                         choices = c("Sightings by Season", "Sightings by State", "Sightings by Temperature")),
             hr(),
             h4("Click and drag on the top of any bar to see details"),
             verbatimTextOutput("info"),
             plotOutput("selectedPlot", brush = "plot_click")
           )#fluidpage for visualizations page
           
  ),#Tabpanel for visualizations page
  
  tabPanel("Sightings and Weather",
           fluidPage(
             tags$head(
               tags$style(HTML("
               body {
                 background-color: #1a1a1a;
                 color: white;
               }
               .box {
                 background-color: #2c3e50;
                 border: none;
                 padding: 15px;
                 border-radius: 5px;
                 margin-bottom: 20px;
               }
               .leaflet-container {
                 background-color: #1a1a1a;
               }
               .well {
                 background-color: #1a1a1a;
                 border: 1px solid #2c3e50;
               }
               h4 {
                 font-weight: bold;
                 color: white;
               }
               #sighting_count, #year_info {
                 color: white;
                 font-size: 14px;
                 margin: 5px 0;
               }
             "))
             ),
             
             titlePanel("Bigfoot Sightings & Weather Analysis"),
             
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 
                 # Bigfoot Sighting Controls
                 h4("Bigfoot Sightings", style = "color: #ff6b6b;"),
                 
                 selectInput("view_mode", "Visualization Mode:",
                             choices = c("Heatmap" = "heat",
                                         "Clustered Markers" = "cluster", 
                                         "Circle Markers" = "circles"),
                             selected = "heat"),
                 
                 conditionalPanel(
                   condition = "input.view_mode == 'heat'",
                   sliderInput("heatmap_radius", "Heatmap Radius:",
                               min = 5, max = 50, value = 25, step = 5),
                   sliderInput("heatmap_blur", "Heatmap Blur:",
                               min = 5, max = 50, value = 20, step = 5)
                 ),
                 
                 conditionalPanel(
                   condition = "input.view_mode == 'circles'",
                   sliderInput("circle_size", "Circle Size:",
                               min = 2, max = 15, value = 5, step = 1)
                 ),
                 
                 sliderInput("year_slider", "Year (Cumulative):",
                             min = 1900, max = 2024, value = 1900, 
                             step = 1, sep = "",
                             animate = animationOptions(interval = 300, loop = FALSE)),
                 
                 sliderInput("year_range", "Year Range Filter:",
                             min = 1900, max = 2024, value = c(1900, 2024),
                             step = 1, sep = ""),
                 
                 # Weather Data Controls
                 h4("Weather Layers", style = "color: #4ecdc4; margin-top: 20px;"),
                 
                 checkboxGroupInput("weather_layers", "Show Weather Data:",
                                    choices = c("High Temperature" = "temp_high",
                                                "Low Temperature" = "temp_low",
                                                "Precipitation" = "precip",
                                                "Wind Direction" = "wind",
                                                "Visibility" = "visibility"),
                                    selected = NULL),
                 
                 conditionalPanel(
                   condition = "input.weather_layers.includes('wind')",
                   selectInput("state_filter", "Wind: Select States",
                               choices = NULL,
                               multiple = TRUE,
                               selectize = TRUE)
                 ),
                 
                 sliderInput("date_range", "Date Range for Weather:",
                             min = as.Date("1900-01-01"),
                             max = as.Date("2024-12-31"),
                             value = c(as.Date("1900-01-01"), as.Date("2024-12-31")),
                             timeFormat = "%Y-%m-%d"),
                 
                 # General Map Controls
                 h4("Map Options", style = "margin-top: 20px;"),
                 
                 selectInput("map_state", "Zoom to State:",
                             choices = "All States",
                             selected = "All States"),
                 
                 checkboxInput("show_state_boundaries", "Show State Boundaries", 
                               value = FALSE),
                 
                 hr(),
                 
                 # Statistics Display
                 div(style = "padding: 15px; background-color: #2c3e50; border-radius: 5px;",
                     textOutput("sighting_count"),
                     textOutput("year_info")
                 )
               ),
               
               mainPanel(
                 width = 9,
                 
                 div(class = "box",
                     h4("Interactive Map"),
                     
                     # Add instructions here
                     div(style = "padding: 10px; margin-bottom: 15px; background-color: #000000; border-radius: 5px;",
                         tags$h4("How to Use This Map"),
                         tags$p("The first dropdown that comes up is the heat map. This is demonstrating the sightings of Bigfoot across the United States using color gradients to show density; the more prevelent, the more red. 
                                Next is the Clustered markers. this is demonstrating individual sightings grouped as circles with an exact number per cluster. 
                                Last is circle markers, which show the exact points on the map where Bigfoot was sighted. If you click on a point, the observation pops up for you to read. 
                                For the weather layers: If you click on high temperature, it is showing a a gradient of the high temperature (the darker the red the warmer) for that day of the sighting. Similarly, with low temperature the darker the blue the colder.
                                Precipitation is showing what was occuring for that day for example: raining or snowing and how much.
                                Wind direction is shown through the direction of the arrows that pop up. Please select a specific state to not overcrowd the map.
                                Visibility is shown through the transparency or opaqueness of the circle. 
                                If you click on any of the points when looking at weather, it will show the county and the respective weather information.")
                     ),
                     
                     leafletOutput("map", height = "600px")
                 ),
                 
                 fluidRow(
                   column(8,
                          div(class = "box",
                              h4("Sightings Timeline"),
                              plotOutput("timeline_plot", height = "300px")
                          )
                   ),
                   column(4,
                          div(class = "box",
                              h4("Legend"),
                              htmlOutput("legend_info")
                          )
                   )
                 )
               )
             )
           )
  ),#tabPanel for Sightings and Weather
  
  tabPanel("Moon Phase Tracker for Bigfoot Sightings",
           fluidPage(
             tags$head(
               tags$style(HTML("
      body { background-color: #0a0e27; color: #ffffff; }
      .well { background-color: #1e2742; border: none; }
      h2, h3, h4 { color: #f9ca24; }
      .info-box { 
        background-color: #1e2742; 
        padding: 15px; 
        border-radius: 5px; 
        margin: 10px 0;
        border-left: 4px solid #f9ca24;
      }
    "))
             ),
             
             titlePanel("Bigfoot Sightings by Moon Phase"),
             
             sidebarLayout(
               sidebarPanel(
                 h4("Animation Controls"),
                 actionButton("animate", "Animate Through Phases", 
                              icon = icon("play"),
                              class = "btn-primary btn-lg",
                              style = "width: 100%; margin-bottom: 10px;"),
                 actionButton("stop", "Stop", 
                              icon = icon("stop"),
                              class = "btn-danger",
                              style = "width: 100%; margin-bottom: 10px;"),
                 sliderInput("phase_slider", "Select Moon Phase:", 
                             min = 0, max = 1, value = 0, step = 0.01),
                 sliderInput("speed", "Animation Speed:", 
                             min = 1, max = 20, value = 10, step = 1),
                 hr(),
                 div(class = "info-box",
                     h4("Current Phase:"),
                     textOutput("phase_name")
                 ),
                 div(class = "info-box",
                     h4("Sightings in Current Phase:"),
                     textOutput("current_sightings")
                 ),
                 hr(),
                 div(class = "info-box",
                     h4("Most Sightings:"),
                     textOutput("most_sightings_phase"),
                     textOutput("most_sightings_count")
                 ),
                 div(class = "info-box",
                     h4("Least Sightings:"),
                     textOutput("least_sightings_phase"),
                     textOutput("least_sightings_count")
                 )
               ),
               
               mainPanel(
                 plotOutput("moon_plot", height = "500px"),
                 hr(),
                 h3("Bigfoot Sightings by Moon Phase"),
                 plotOutput("phase_distribution", height = "300px")
               )
             )
           )#fluidPage for moonphase tracker
           
  )#tabPanel for moonphase tracker
  
)#navbarPage