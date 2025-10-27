bigfoot_data <- read.csv("bigfoot_data_wordcount_filtered_less_200.csv")

library(wordcloud)
library(tm)
library(memoise)
library(tidyverse)
library(stringr)
library(ggplot2)




function(input, output, session) {
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
  
  # Load and prepare data with SF
  bigfoot_sf <- reactive({
    # Read CSV
    df <- read.csv("filtered_data_lat_long_date.csv", stringsAsFactors = FALSE)
    
    # DEBUGGING: Print column names to see what you have
    print("Column names in CSV:")
    print(colnames(df))
    
    print(paste("Total rows loaded:", nrow(df)))
    
    # Rename columns to standard names
    if("latitude" %in% colnames(df)) {
      df <- df %>% rename(lat = latitude)
    }
    if("longitude" %in% colnames(df)) {
      df <- df %>% rename(long = longitude)
    }
    if("Date" %in% colnames(df)) {
      df <- df %>% rename(date = Date)
    }
    if("lon" %in% colnames(df)) {
      df <- df %>% rename(long = lon)
    }
    
    # Remove empty date strings
    df$date[df$date == ""] <- NA
    
    # Parse date - your format is m/d/Y (e.g., "12/3/2005")
    df$date_parsed <- as.Date(df$date, format = "%m/%d/%Y")
    
    # Try alternate format if first attempt fails
    if(sum(!is.na(df$date_parsed)) == 0) {
      df$date_parsed <- as.Date(df$date, format = "%Y-%m-%d")
    }
    
    print(paste("Dates parsed successfully:", sum(!is.na(df$date_parsed))))
    
    df$year <- year(df$date_parsed)
    
    print(paste("Years extracted:", sum(!is.na(df$year))))
    
    # Remove rows with missing coordinates or dates
    df_clean <- df %>%
      filter(!is.na(lat) & !is.na(long) & !is.na(year))
    
    print(paste("Rows after removing NAs:", nrow(df_clean)))
    
    # Filter reasonable coordinate ranges
    df_clean <- df_clean %>%
      filter(lat >= -90 & lat <= 90) %>%
      filter(long >= -180 & long <= 180)
    
    print(paste("Final rows after coordinate filter:", nrow(df_clean)))
    
    # Check if we have any data left
    if(nrow(df_clean) == 0) {
      stop("No valid data after filtering. Check your date format and coordinates.")
    }
    
    # Convert to SF object
    sf_obj <- st_as_sf(df_clean, 
                       coords = c("long", "lat"),
                       crs = 4326,  # WGS84 coordinate system
                       remove = FALSE)
    
    print(paste("SF object created with", nrow(sf_obj), "rows"))
    
    return(sf_obj)
  })
  
  # Update slider ranges based on actual data
  observe({
    data <- bigfoot_sf()
    
    if(nrow(data) > 0) {
      min_year <- min(data$year, na.rm = TRUE)
      max_year <- max(data$year, na.rm = TRUE)
      
      updateSliderInput(session, "year_slider",
                        min = min_year,
                        max = max_year,
                        value = min_year)
      
      updateSliderInput(session, "year_range",
                        min = min_year,
                        max = max_year,
                        value = c(min_year, max_year))
    }
  })
  
  # Filter data based on inputs
  filtered_sf <- reactive({
    data <- bigfoot_sf()
    
    # Filter by year range (cumulative up to selected year)
    data <- data %>%
      filter(year >= input$year_range[1] & year <= input$year_slider)
    
    return(data)
  })
  
  # Load state boundaries (optional)
  state_boundaries <- reactive({
    if(input$show_state_boundaries) {
      # Using built-in US states data
      states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
      return(states)
    }
    return(NULL)
  })
  
  # Create base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = -98, lat = 39, zoom = 4) %>%
      addScaleBar(position = "bottomleft")
  })
  
  # Update map based on view mode
  observe({
    data <- filtered_sf()
    
    # Clear existing layers
    leafletProxy("map") %>%
      clearHeatmap() %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      clearShapes()
    
    # Add state boundaries if requested
    if(input$show_state_boundaries && !is.null(state_boundaries())) {
      leafletProxy("map") %>%
        addPolygons(
          data = state_boundaries(),
          fillColor = "transparent",
          color = "white",
          weight = 1,
          opacity = 0.5
        )
    }
    
    # Add visualization based on mode
    if(input$view_mode == "heat" && nrow(data) > 0) {
      leafletProxy("map", data = data) %>%
        addHeatmap(
          lng = ~long,
          lat = ~lat,
          intensity = 2,  # Increased from 1 to 2 for more intensity
          radius = input$heatmap_radius,
          blur = input$heatmap_blur,
          max = 0.8,  # Increased from 0.5 to 0.8 for more visible colors
          minOpacity = 0.3  # Added minimum opacity for better visibility
        )
    } else if(input$view_mode == "cluster" && nrow(data) > 0) {
      leafletProxy("map", data = data) %>%
        addCircleMarkers(
          lng = ~long,
          lat = ~lat,
          radius = 5,
          color = "#ff6b6b",
          fillOpacity = 0.6,
          stroke = TRUE,
          weight = 1,
          clusterOptions = markerClusterOptions(
            showCoverageOnHover = FALSE,
            zoomToBoundsOnClick = TRUE
          ),
          popup = ~paste0(
            "<b>Date:</b> ", date, "<br>",
            "<b>Location:</b> ", round(lat, 4), ", ", round(long, 4)
          )
        )
    } else if(input$view_mode == "circles" && nrow(data) > 0) {
      leafletProxy("map", data = data) %>%
        addCircleMarkers(
          lng = ~long,
          lat = ~lat,
          radius = input$circle_size,
          color = "#ff6b6b",
          fillColor = "#ff6b6b",
          fillOpacity = 0.4,
          stroke = TRUE,
          weight = 1,
          opacity = 0.8,
          popup = ~paste0(
            "<b>Date:</b> ", date, "<br>",
            "<b>Year:</b> ", year, "<br>",
            "<b>Location:</b> ", round(lat, 4), ", ", round(long, 4)
          )
        )
    }
  })
  
  # Display statistics
  output$sighting_count <- renderText({
    data <- filtered_sf()
    paste("Total Sightings:", nrow(data))
  })
  
  output$year_info <- renderText({
    paste("Showing:", input$year_range[1], "to", input$year_slider)
  })
  
  # Timeline plot showing all data
  output$timeline_plot <- renderPlot({
    data <- bigfoot_sf() %>%
      st_drop_geometry() %>%  # Remove geometry for faster processing
      filter(year >= input$year_range[1] & year <= input$year_range[2])
    
    # Check if we have data
    if(nrow(data) == 0) {
      plot.new()
      text(0.5, 0.5, "No data available", cex = 1.5, col = "white")
      return()
    }
    
    year_counts <- data %>%
      group_by(year) %>%
      summarise(count = n(), .groups = "drop")
    
    # Check if we have counts
    if(nrow(year_counts) == 0 || max(year_counts$count) == 0) {
      plot.new()
      text(0.5, 0.5, "No data to plot", cex = 1.5, col = "white")
      return()
    }
    
    par(bg = "#1a1a1a", col.axis = "white", col.lab = "white", col.main = "white")
    plot(year_counts$year, year_counts$count,
         type = "h",
         col = ifelse(year_counts$year <= input$year_slider, "#ff6b6b", "#555555"),
         lwd = 3,
         xlab = "Year",
         ylab = "Sightings",
         main = "Sightings Timeline",
         las = 1,
         ylim = c(0, max(year_counts$count, na.rm = TRUE) * 1.1))
    
    # Add vertical line for current year
    abline(v = input$year_slider, col = "#4ecdc4", lwd = 2, lty = 2)
    
    # Add grid
    grid(col = "#333333", lty = 1)
  })
  
  
}