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
)