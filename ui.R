library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(lubridate)

# Layout for all dropdowns
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Selected item display (what shows in the box after selection) */
      .selectize-input {
        color: #2c3e50 !important;
        background-color: #FFFFFF !important;
      }
      
      /* Dropdown menu options */
      .selectize-dropdown, .selectize-dropdown-content, .selectize-dropdown .option {
        color: #FFFFFF !important;
        background-color: #2c3e50 !important;
      }
      
      /* Hover effect - matching navbar style */
      .selectize-dropdown .option:hover,
      .selectize-dropdown .active {
        background-color: rgba(255, 107, 107, 0.1) !important;
        color: #ff6b6b !important;
      }
      
      /* Selected option in dropdown (when highlighted/chosen) */
      .selectize-dropdown .option.selected {
        color: #000000 !important;
        background-color: #FFFFFF !important;
      }
    "))
  ),

  # start of webpage
navbarPage(
  title = div(
    style = "display: flex; align-items: center;",
    tags$span(style = "font-size: 20px; font-weight: 600; letter-spacing: 0.5px;", "Bigfoot Research Portal")
  ),
  windowTitle = "Bigfoot Sightings Database",
  
  # Global CSS
  header = tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');
      
      body {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        background-color: #1a1a1a;
        color: #e4e4e4;
      }
      
      .navbar-default {
        background-color: #2c3e50;
        border: none;
        box-shadow: 0 4px 20px rgba(0,0,0,0.4);
      }
      
      .navbar-default .navbar-brand {
        color: #ffffff !important;
        font-weight: 600;
      }
      
      .navbar-default .navbar-nav > li > a {
        color: #e4e4e4 !important;
        font-weight: 500;
        transition: all 0.3s ease;
        padding: 15px 20px;
      }
      
      .navbar-default .navbar-nav > li > a:hover {
        color: #ff6b6b !important;
        background: rgba(255, 107, 107, 0.1) !important;
      }
      
      .navbar-default .navbar-nav > .active > a {
        background: rgba(249, 202, 36, 0.2) !important;
        color: #f9ca24 !important;
      }
      
      h1, h2, h3, h4, h5 {
        font-weight: 600;
        color: #ffffff;
      }
      
      .well {
        background-color: #2c3e50;
        border: 1px solid rgba(78, 205, 196, 0.2);
        border-radius: 12px;
        box-shadow: 0 8px 32px rgba(0,0,0,0.2);
      }
      
      .box {
        background-color: #2c3e50;
        border: 1px solid rgba(78, 205, 196, 0.2);
        padding: 20px;
        border-radius: 12px;
        margin-bottom: 20px;
        box-shadow: 0 8px 32px rgba(0,0,0,0.2);
      }
      
      .btn-primary {
        background-color: #4ecdc4;
        border: none;
        border-radius: 8px;
        font-weight: 500;
        transition: all 0.3s ease;
        padding: 10px 20px;
      }
      
      .btn-primary:hover {
        background-color: #44a08d;
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(78, 205, 196, 0.4);
      }
      
      .btn-danger {
        background-color: #ff6b6b;
        border: none;
        border-radius: 8px;
        font-weight: 500;
        transition: all 0.3s ease;
      }
      
      .btn-danger:hover {
        background-color: #ee5a52;
        transform: translateY(-2px);
      }
      
      .form-control, .selectize-input {
        background-color: #1a1a1a;
        border: 1px solid rgba(78, 205, 196, 0.3);
        color: #e4e4e4;
        border-radius: 8px;
      }
      
      .form-control:focus, .selectize-input.focus {
        border-color: #4ecdc4;
        box-shadow: 0 0 0 0.2rem rgba(78, 205, 196, 0.25);
        background-color: #1a1a1a;
      }
      
      .selectize-dropdown {
        background-color: #2c3e50;
        border: 1px solid rgba(78, 205, 196, 0.3);
      }
      
      .selectize-dropdown-content .option {
        color: #e4e4e4;
      }
      
      .selectize-dropdown-content .option:hover {
        background-color: rgba(78, 205, 196, 0.2);
      }
      
      .slider-animate-button {
        background-color: #4ecdc4 !important;
        border: none !important;
      }
      
      hr {
        border-top: 1px solid rgba(78, 205, 196, 0.3);
      }
      
      .leaflet-container {
        border-radius: 12px;
        box-shadow: 0 8px 32px rgba(0,0,0,0.3);
      }
      
      .info-box {
        background-color: #1e2742;
        padding: 15px;
        border-radius: 8px;
        margin: 10px 0;
        border-left: 4px solid #f9ca24;
        box-shadow: 0 4px 16px rgba(0,0,0,0.2);
      }
      
      @keyframes slide {
        0% { left: -100px; }
        100% { left: 100%; }
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
  
  # HOME PAGE
  tabPanel(
    "Home",
    fluidPage(
      div(style = "text-align: center; padding: 40px 20px;",
          tags$img(src = "bigfoot-landing-page.jpg", width = "900px", style = "border-radius: 12px; box-shadow: 0 8px 32px rgba(0,0,0,0.4);"),
          tags$h1(style = "margin-top: 30px; font-size: 48px; color: #f9ca24;", "Welcome to Bigfoot Sightings"),
          tags$h2(style = "color: #ff6b6b; margin-top: 20px;", "Bigfoot...Fact or Fiction?"),
          tags$h3(style = "color: #e4e4e4; margin-top: 20px; max-width: 800px; margin-left: auto; margin-right: auto; line-height: 1.6;", 
                  "Have you ever wondered where Bigfoot has been found? Want to know where to look next? Well, you've come to the right place!")
      ), 
      
      tags$audio(src = "growl-and-roar-102417.mp3", 
                 autoplay = "autoplay",
                 type = "audio/mpeg"),
      
      tags$img(id = "moving-image", 
               src = "new-bigfoot-image-removebg-preview.png",
               width = "300px")
    )#fluid page end 
  ),
  
  # WORD CLOUD
  tabPanel("Word Cloud", 
           fluidPage(
             style = "padding: 20px;",
             h2("Word Cloud of Words Used to Describe Bigfoot Sightings", style = "text-align: center; color: #f9ca24; margin-bottom: 30px;"),
             
             div(
               style = "background-color: #2c3e50; padding: 30px; border-radius: 12px; margin-bottom: 30px; box-shadow: 0 8px 32px rgba(0,0,0,0.2);",
               h3("What's in the Word Cloud?", style = "color: #ff6b6b;"),
               tags$p(style = "font-size: 16px; line-height: 1.6;", "Each report made of a Bigfoot sighting had a section available to give an explanation of what the person saw."),
               tags$p(style = "font-size: 16px; line-height: 1.6;", "This word cloud displays the most frequently used words in these descriptions."),
               tags$p(style = "font-size: 16px; line-height: 1.6;", "Let's find out what all of these stories from first eye witnesses have in common...")
             ),
             
             sidebarLayout(
               sidebarPanel(
                 style = "background-color: #2c3e50; border-radius: 12px; padding: 20px;",
                 sliderInput("max_words",
                             "Select Amount of Words Shown:",
                             min = 1,
                             max = 100,
                             value = 50,
                             step = 1)
               ),
               mainPanel(
                 div(style = "background-color: #1a1a1a; border-radius: 12px; padding: 20px; box-shadow: 0 8px 32px rgba(0,0,0,0.2);",
                     plotOutput("WordCloud", height = "600px")
                 )
               ) 
             )
           )
  ),
  
  # VISUALIZATIONS
  tabPanel("Sightings Visualizations",
           fluidPage(
             style = "padding: 20px;",
             h2("Visualizations of Bigfoot Sightings by Season, State, and Temperature", style = "text-align: center; color: #f9ca24; margin-bottom: 30px;"),
             
             div(style = "max-width: 1200px; margin: 0 auto;",
                 div(style = "background-color: #2c3e50; padding: 20px; border-radius: 12px; margin-bottom: 20px;",
                     selectInput("plotChoice", "Choose a Plot:",
                                 choices = c("Sightings per Season", "Sightings per State", "Sightings per Temperature"),
                                 width = "100%")
                 ),
                 
                 div(style = "background-color: #2c3e50; padding: 30px; border-radius: 12px; box-shadow: 0 8px 32px rgba(0,0,0,0.2);",
                     h4("Click on top of any bar to see the number of sightings", style = "color: #f9ca24; text-align: center;"),
                     plotOutput("selectedPlot", click = "plots_click"),
                     tags$style("#plots_text { font-size: 16px; color: white; }"),
                     textOutput("plots_text")
                 )
             )
           )
  ),
  
  # MAP OF SIGHTINGS AND WEATHER
  tabPanel("Map of Sightings & Weather",
           fluidPage(
             titlePanel(div(style = "color: #f9ca24; text-align: center; margin-bottom: 30px;", "Bigfoot Sightings & Weather Analysis")),
             
             fluidRow(
               # Left sidebar with controls
               # In your Map tab, find the left sidebar column(3, ...) section
               # Replace the current controls with this:
               
               column(3,
                      div(style = "background-color: #2c3e50; border-radius: 12px; padding: 20px; position: sticky; top: 20px;",
                          h4("Bigfoot Sightings", style = "color: #ff6b6b;"),
                          
                          selectInput("view_mode", "Visualization Mode:",
                                      choices = c("None (Hide Sightings)" = "none",
                                                  "Heatmap" = "heat",
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
                          
                          # NEW: Radio buttons for view type
                          radioButtons("view_type", "Time View:",
                                       choices = c("Cumulative (All years up to selected)" = "cumulative",
                                                   "Single Year Only" = "single"),
                                       selected = "cumulative"),
                          
                          sliderInput("year_slider", "Year:",
                                      min = 1900, max = 2024, value = 1900, 
                                      step = 1, sep = "",
                                      animate = animationOptions(interval = 300, loop = FALSE)),
                          
                          # Shows what data is being displayed
                          textOutput("year_description"),
                          
                          h4("Weather Layers", style = "color: #4ecdc4; margin-top: 20px;"),
                          
                          selectInput("weather_layers", "Show Weather Data:",
                                      choices = c("None" = "none",
                                                  "High Temperature" = "temp_high",
                                                  "Low Temperature" = "temp_low",
                                                  "Precipitation" = "precip",
                                                  "Wind Direction" = "wind",
                                                  "Visibility" = "visibility"),
                                      selected = "none"),
                          
                          conditionalPanel(
                            condition = "input.weather_layers == 'wind'",
                            selectInput("state_filter", "Wind: Select States",
                                        choices = NULL,
                                        multiple = TRUE,
                                        selectize = TRUE),
                            div(style = "padding: 10px; background-color: #1a1a1a; border-radius: 8px; margin-top: 10px;",
                                tags$p(style = "color: #f9ca24; font-size: 12px; margin: 0;",
                                       icon("info-circle"),
                                       " Select specific states to view wind arrows. Limited to 100 arrows total for performance.")
                            )
                          ),
                          
                          h4("Map Options", style = "margin-top: 20px;"),
                          
                          selectInput("map_state", "Zoom to State:",
                                      choices = "All States",
                                      selected = "All States"),
                          
                          checkboxInput("show_state_boundaries", "Show State Boundaries", 
                                        value = FALSE),
                          
                          hr(),
                          
                          div(style = "padding: 15px; background-color: #1a1a1a; border-radius: 8px;",
                              textOutput("sighting_count"),
                              textOutput("year_info")
                          )
                      )
               ),
               
               # Main content area
               column(9,
                      div(class = "box",
                          h4("Interactive Map", style = "color: #f9ca24;"),
                          
                          div(style = "padding: 15px; margin-bottom: 15px; background-color: #1a1a1a; border-radius: 8px;",
                              tags$h4("How to Use This Map", style = "color: #ff6b6b;"),
                              tags$p(style = "font-size: 14px; line-height: 1.6;", strong("Heat Map:"), " Demonstrates Bigfoot sightings across the United States using color gradients to show density; the more sightings, the more red the map appears."), 
                              tags$p(style = "font-size: 14px; line-height: 1.6;", strong("Clustered Markers:"), " Demonstrates multiple sightings within the same area grouped as circles with exact counts."),
                              tags$p(style = "font-size: 14px; line-height: 1.6;", strong("Circle Markers:"), " Shows exact sighting locations. Click on a point to read the observation description."), 
                              tags$h4("Weather Layers:", style = "color: #f9ca24; margin-top: 15px;"),
                              tags$p(style = "font-size: 14px; line-height: 1.6;", strong("High Temperature:"), " Gradient showing temperature (darker red = warmer)."),
                              tags$p(style = "font-size: 14px; line-height: 1.6;", strong("Low Temperature:"), " Gradient showing temperature (darker blue = colder)."),
                              tags$p(style = "font-size: 14px; line-height: 1.6;", strong("Precipitation:"), " Shows weather type and amount on sighting days."),
                              tags$p(style = "font-size: 14px; line-height: 1.6;", strong("Wind Direction:"), " Arrows showing wind direction. Select specific states for clarity."),
                              tags$p(style = "font-size: 14px; line-height: 1.6;", strong("Visibility:"), " Circle transparency indicates visibility (more opaque = less visible).")
                          ),
                          
                          leafletOutput("map", height = "600px")
                      ),
                      
                      # Weather Correlation Plot Section
                      div(class = "box", style = "margin-top: 20px;",
                          h4("Weather Correlation Analysis", style = "color: #f9ca24;"),
                          plotOutput("weather_correlation_scatter_plot", height = "700px")
                      ),
                      
                      fluidRow(
                        column(8,
                               div(class = "box",
                                   h4("Sightings Timeline", style = "color: #f9ca24;"),
                                   plotOutput("timeline_plot", height = "300px")
                               )
                        ),
                        column(4,
                               div(class = "box",
                                   h4("Legend", style = "color: #f9ca24;"),
                                   htmlOutput("legend_info")
                               )
                        )
                      )
               )
             )  # closes fluidRow
           )    # closes fluidPage  
  ),           # closes "Map of Sightings & Weather" tabPanel
  

#Weather Correlations

#Correlation tab
tabPanel("Correlations",
         fluidPage(
           titlePanel("Bigfoot Sightings vs Various Metrics"),
           style = "background-color: #0a0e27;",
           
           titlePanel(div(style = "color: #f9ca24; text-align: center;", "Various Other explanations of Bigfoot sightings")),
           
           sidebarLayout(
             sidebarPanel(
               selectInput("modelChoice", 
                           "Select Model:",
                           choices = c("Forest Coverage Percent" = "model1",
                                       "Forest Land Area" = "model2",
                                       "Census Land Area" = "model3",
                                       "Bear Sightings by State" = "model4",
                                       "Bear Sightings by County" = "model5"),
                           selected = "model1")
             ),
             
             mainPanel(
               plotOutput("scatterPlot", height = "1000px"),
               verbatimTextOutput("modelStats"),
               uiOutput("modelDescription")
             )
           )
         )  
),


# MOON PHASE TRACKER
  tabPanel("Moon Phase Tracker",
           fluidPage(
             style = "background-color: #0a0e27;",
             
             titlePanel(div(style = "color: #f9ca24; text-align: center;", "Bigfoot Sightings by Moon Phase")),
             
             sidebarLayout(
               sidebarPanel(
                 style = "background-color: #1e2742; border-radius: 12px; padding: 20px;",
                 h4("Animation Controls", style = "color: #f9ca24;"),
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
                     h4("Current Phase:", style = "color: #f9ca24;"),
                     textOutput("phase_name")
                 ),
                 div(class = "info-box",
                     h4("Sightings in Current Phase:", style = "color: #f9ca24;"),
                     textOutput("current_sightings")
                 ),
                 hr(),
                 div(class = "info-box",
                     h4("Most Sightings:", style = "color: #f9ca24;"),
                     textOutput("most_sightings_phase"),
                     textOutput("most_sightings_count")
                 ),
                 div(class = "info-box",
                     h4("Least Sightings:", style = "color: #f9ca24;"),
                     textOutput("least_sightings_phase"),
                     textOutput("least_sightings_count")
                 )
               ),
               
               mainPanel(
                 plotOutput("moon_plot", height = "500px"),
                 hr(),
                 h3("Bigfoot Sightings vs. Moon Brightness", style = "color: #f9ca24; text-align: center;"),
                 plotOutput("phase_distribution", height = "300px")
               )
             )
           )
  ),
  
# TOPOGRAPHIC MAP
tabPanel("Topographic Map",
         fluidPage(
           div(style = "background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%); color: white; padding: 30px; margin-bottom: 20px; border-radius: 12px; box-shadow: 0 8px 32px rgba(0,0,0,0.3);",
               h2("United States Elevation & Bigfoot Analysis", style = "margin: 0;"),
               p("Explore topographic elevation and Bigfoot sightings across the 50 states", 
                 style = "margin: 10px 0 0 0; opacity: 0.9; font-size: 16px;")
           ),
           
           leafletOutput("topographic_map", height = "700px"),
           
           div(style = "margin-top: 20px; padding: 30px; background-color: #2c3e50; border-radius: 12px; box-shadow: 0 8px 32px rgba(0,0,0,0.3);",
               h3("Statistical Analysis: Elevation vs. Bigfoot Sightings", style = "margin-top: 0; color: #f9ca24;"),
               uiOutput("analysis_output"),
               div(style = "margin-top: 20px; background-color: #1a1a1a; padding: 20px; border-radius: 12px;",
                   plotOutput("correlation_plot", height = "800px")
               )
           ),
           
           div(style = "margin-top: 20px; padding: 20px; background-color: #2c3e50; border-radius: 12px;",
               h4("About This Map", style = "color: #f9ca24;"),
               p(style = "font-size: 15px; line-height: 1.6;", "This interactive map displays topographic information for the United States along with documented Bigfoot sighting locations."),
               p(style = "font-size: 15px; line-height: 1.6;", strong("Features:"), " Each blue dot represents a reported Bigfoot sighting. Click on any dot to see its exact coordinates. The OpenTopoMap background provides detailed topographic visualization including contour lines and terrain shading."),
               p(style = "font-size: 15px; line-height: 1.6;", strong("Data Sources:"), " OpenTopoMap tiles for topographic features and filtered_bigfoot_data.csv for sighting locations.")
           )
         )
),

# BIGFOOT PROBABILITY PREDICTOR 

tabPanel("Bigfoot Probability Predictor",
         fluidPage(
           style = "padding: 40px 20px;",
           
           div(style = "text-align: center; margin-bottom: 40px;",
               tags$h1(style = "color: #f9ca24; font-size: 48px;", "Will You Spot Bigfoot?"),
               tags$p(style = "font-size: 18px; color: #e4e4e4;", 
                      "Enter your conditions below to calculate your probability of a Bigfoot encounter!")
           ),
           
           fluidRow(
             # Input Panel
             column(5,
                    div(style = "background-color: #2c3e50; padding: 30px; border-radius: 12px; box-shadow: 0 8px 32px rgba(0,0,0,0.2);",
                        h3("Your Conditions", style = "color: #ff6b6b; margin-bottom: 20px;"),
                        
                        selectInput("pred_state", "Select State:",
                                    choices = NULL,
                                    selected = NULL),
                        
                        selectInput("pred_season", "Select Season:",
                                    choices = c("Spring", "Summer", "Fall", "Winter"),
                                    selected = "Summer"),
                        
                        sliderInput("pred_temp", "Temperature (°F):",
                                    min = 0, max = 100, value = 65, step = 5),
                        
                        sliderInput("pred_moon", "Moon Phase:",
                                    min = 0, max = 1, value = 0.5, step = 0.1,
                                    post = ""),
                        
                        selectInput("pred_time", "Time of Day:",
                                    choices = c("Dawn (5am-7am)" = "dawn",
                                                "Morning (7am-12pm)" = "morning", 
                                                "Afternoon (12pm-5pm)" = "afternoon",
                                                "Dusk (5pm-7pm)" = "dusk",
                                                "Night (7pm-5am)" = "night"),
                                    selected = "dusk"),
                        
                        sliderInput("pred_visibility", "Visibility (miles):",
                                    min = 0, max = 10, value = 5, step = 0.5),
                        
                        hr(),
                        
                        actionButton("calculate_prob", "Calculate My Chances!",
                                     class = "btn-primary btn-lg",
                                     style = "width: 100%; font-size: 18px;")
                    )
             ),
             
             # Results Panel
             column(7,
                    div(style = "background-color: #2c3e50; padding: 30px; border-radius: 12px; box-shadow: 0 8px 32px rgba(0,0,0,0.2); min-height: 600px;",
                        h3("Your Bigfoot Encounter Probability", style = "color: #f9ca24; text-align: center;"),
                        
                        # Probability Display
                        div(style = "text-align: center; margin: 40px 0;",
                            uiOutput("probability_display")
                        ),
                        
                        # Gauge/Meter visualization
                        plotOutput("probability_gauge", height = "200px"),
                        
                        hr(),
                        
                        # Breakdown by factor
                        h4("Factor Breakdown:", style = "color: #ff6b6b; margin-top: 20px;"),
                        uiOutput("factor_breakdown"),
                        
                        hr(),
                        
                        # Recommendations
                        h4("Tips to Increase Your Chances:", style = "color: #4ecdc4; margin-top: 20px;"),
                        uiOutput("recommendations")
                    )
             )
           ),
           
           # Educational Note
           div(style = "background-color: #2c3e50; padding: 20px; border-radius: 12px; margin-top: 30px; text-align: center;",
               tags$p(style = "font-size: 14px; color: #e4e4e4; font-style: italic;",
                      "Note: This predictor is based on historical sighting data patterns and is intended for educational and entertainment purposes. 
                      Actual Bigfoot encounters cannot be predicted with certainty!")
           )
         )#Fluid page end
),

  # ABOUT US
  tabPanel("About Us",
           fluidPage(
             style = "padding: 40px 20px;",
             div(style = "max-width: 1000px; margin: 0 auto;",
                 
                 div(style = "text-align: center; margin-bottom: 50px;",
                     tags$h1(style = "color: #f9ca24; font-size: 48px;", "About Us")
                 ),
                 
                 div(style = "background-color: #2c3e50; padding: 30px; border-radius: 12px; margin-bottom: 30px; box-shadow: 0 8px 32px rgba(0,0,0,0.2);",
                     tags$h3("About the Creators", style = "color: #ff6b6b; margin-bottom: 20px;"),
                     tags$p(style = "font-size: 16px; line-height: 1.8;", "As science majors, our group wanted to step outside our usual realm and explore a topic that blurs the line between myth and reality; Bigfoot. Being able to research without having to read an exhausting scientific paper gave us much joy, and we hope you got a similar amount of joy going through our website!"),
                     div(style = "text-align: center; margin: 30px 0;",
                         tags$img(src = "Bigfoot_Selfie.png", width = "600px", style = "border-radius: 12px; box-shadow: 0 8px 32px rgba(0,0,0,0.4);")
                     ),
                     tags$p(style = "font-size: 15px; line-height: 1.7; margin-top: 20px;", strong("Sophia Taylor (far left)"), " is a Neuroscience and Spanish double major on the pre-med track from Pennsylvania. On campus she is a Community Assistant for Residence Life, Vice President of Recruitment for Panhellenic Council, President of the LEAD program, President of the Pre-Health club, member of the Leadership Excellence Awards Committee, member of Alpha Delta Pi, Traveller and University Store employee, and does research in the Neuroscience Department at W&L."),
                     tags$p(style = "font-size: 15px; line-height: 1.7;", strong("Jake Walters (second from left)"), " is a Neuroscience major on the pre-med track from Kentucky. On campus he is collecting all the frat infinity stones: Phi Delt, Fiji, and now Phi Psi, ILoveWater club co-president, Campus Kitchen Shift Leader, logistics chair of Remote Area Medical, and is a Sustainability intern."),
                     tags$p(style = "font-size: 15px; line-height: 1.7;", strong("Ella Moser (second from right)"), " is a Biology major and art history minor on the pre-med track from Florida. On campus she is the president of Washington and Lee Brain Exercise Initiative, a member of the Generals Activity board, and a member of Remote Area Medical."),
                     tags$p(style = "font-size: 15px; line-height: 1.7;", strong("Sarah Stockton (far right)"), " is a Neuroscience and Politics double major on the pre-med track from North Carolina. On campus she is in W&L Dance Company, a Kathekon Program Ambassador, University Ambassador, an Interview Fellow in the W&L Admissions Office, and a member of Pi Beta Phi.")
                 ),
                 
                 div(style = "background-color: #2c3e50; padding: 30px; border-radius: 12px; margin-bottom: 30px; box-shadow: 0 8px 32px rgba(0,0,0,0.2);",
                     tags$h3("What We Offer", style = "color: #f9ca24; margin-bottom: 20px;"),
                     tags$ul(style = "font-size: 16px; line-height: 1.8;",
                             tags$li("Interactive maps showing sighting locations"),
                             tags$li("Weather data analysis for reported encounters"),
                             tags$li("Filtering tools to explore patterns and trends"),
                             tags$li("Detailed information about each sighting report")
                     )
                 ),
                 
                 div(style = "background-color: #2c3e50; padding: 30px; border-radius: 12px; margin-bottom: 30px; box-shadow: 0 8px 32px rgba(0,0,0,0.2);",
                     tags$h3("Data Sources", style = "color: #f9ca24; margin-bottom: 20px;"),
                     tags$p(style = "font-size: 16px; line-height: 1.8;", "Our Bigfoot Data comes from the Bigfoot Field Researchers Organization (BFRO) database via Timothy Renner, which contains thousands of reported sightings dating back several decades. Weather data is sourced from historical meteorological records:",
                        tags$a(style = "font-size: 16px; line-height: 1.8;" , href = "https://github.com/timothyrenner/bfro_sightings_data", "Bigfoot Sightings Data", target = "_blank")),
                     tags$p(style = "font-size: 16px; line-height: 1.8;", "Forest and Land Area data is sourced from the USDA Forest Inventory and Analysis 2016 Report:",
                        tags$a(style = "font-size: 16px; line-height: 1.8;" , href = "https://www.fs.usda.gov/sites/default/files/fs_media/fs_document/publication-15817-usda-forest-service-fia-annual-report-508.pdf", "Forest and Land Area Data", target = "_blank")),
                     tags$p(style = "font-size: 16px; line-height: 1.8;", "Bear Observations data is sourced from the Global Biodiversity Information Facility (GBIF) via iNaturalist Research Grade Observations of Ursus americanus Pallus and Ursus linnaeus:",
                        tags$a(style = "font-size: 16px; line-height: 1.8;", href = "https://doi.org/10.15468/dl.kp96h3", "Bear Observations Data", target = "_blank"))
                 ),
                 
                 div(style = "background-color: #2c3e50; padding: 30px; border-radius: 12px; margin-bottom: 30px; box-shadow: 0 8px 32px rgba(0,0,0,0.2);",
                     tags$h3("About the Creation", style = "color: #f9ca24; margin-bottom: 20px;"),
                     tags$p(style = "font-size: 16px; line-height: 1.8;", "This application was developed as part of a data visualization project. For questions or feedback, please contact Gregg Whitworth / gwhitworth@wlu.edu.")
                 ),
                 
                 # Download Data Section
                 div(style = "background-color: #2c3e50; padding: 30px; border-radius: 12px; margin-bottom: 30px; box-shadow: 0 8px 32px rgba(0,0,0,0.2); text-align: center;",
                     tags$h3("Download our refined data set that we used to create the web applications you see! The original data set is linked above", 
                             style = "color: #f9ca24; margin-bottom: 25px; line-height: 1.4;"),
                     downloadButton("downloadData", "Download Bigfoot Data", 
                                    style = "background-color: #ff6b6b; color: white; font-size: 18px; font-weight: bold; padding: 15px 30px; border: none; border-radius: 8px; cursor: pointer; box-shadow: 0 4px 12px rgba(255,107,107,0.4);")
                 ),
                 
                 div(style = "text-align: center; margin-top: 50px; color: #999;",
                     tags$p(tags$em("Last updated: December 2025"))
                 )
             )
           )#fluid page end
  )
)
)