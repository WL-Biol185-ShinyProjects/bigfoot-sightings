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
             h4("Click on top of any bar to see the number of sightings"),
             fluidRow(plotOutput("selectedPlot", click = "plots_click")),
             fluidRow(verbatimTextOutput("plots_text"))
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
                         tags$p("The first visualization option in the dropdown is a Heat Map. This demonstrates the Bigfoot sightings across the United States using color gradients to show density; the more sightings there are, the more red the map appears."), 
                         tags$p("The second option in the dropdown is Clustered Markers. This demonstrates multiple sightings within the same general area grouped as circles with an exact number of sightings per circle."),
                         tags$p("The third option in nthe dropdown is Circle Markers. This demonstrates the exact points on the map where a Bigfoot sighting is recorded. If you click on a point, the observation description pops up for you to read."), 
                         tags$h4("Weather Layers description:"),
                         tags$p("Clicking on High Temperature shows a gradient of the high temperature on the respective day of a sighting (the darker the red, the warmer).")                 ,
                         tags$p("Clicking on Low Temperature shows a gradient of the low temperature on the respective day of a sighting (the darker the blue, the colder).")                  ,
                         tags$p("Clicking on Precipitation shows the type of precipitative weather on the respective day of a sighting (for example: rain, snow, and the total amount).")      ,
                         tags$p("Clicking on Wind Direction shows the direction the wind on the respective day of a sighting (shown by arrows).")                                              ,
                         tags$p("Clicking on Visibility showns the visibility on the respective day of a sighting (shown by transparency of the circle; the more opaque, the less visible).")  ,
                         tags$p("Please select a specific state for clearer visualization.")                                                                                                   ,
                         tags$p("If you click on any of the points when looking at weather, the map will show the county and the respective weather information.")                             ,
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