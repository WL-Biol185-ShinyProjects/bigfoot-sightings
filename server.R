

bigfoot_data_for_words <- read.csv("bigfoot_data_wordcount_filtered_less_200.csv")


# for the elevation data, I deleted the front page that described the data. I edited the numbers so they had no commas or decimal points. I removed any of the non 50 U.S. states (U.S. territories) got the data from here: https://www.statista.com/statistics/1325529/lowest-points-united-states-state/
# https://www.fs.usda.gov/sites/default/files/fs_media/fs_document/publication-15817-usda-forest-service-fia-annual-report-508.pdf got got tree data from here. Deleted everything else that wasnt a state and only kept the first two columns to keep things organized



#loading the appropriate libraries
library(wordcloud)
library(tm)
library(memoise)
library(tidyverse)
library(stringr)
library(ggplot2)
library(readxl)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(sf)
library(lubridate)
library(maps)
library(gridExtra)

# defines the server function in a Shiny application
function(input, output, session) {
  # Create custom Bigfoot icon
  bigfoot_icon <- makeIcon(
    iconUrl = "monkey.png",  # Example Bigfoot icon
    iconWidth = 25, 
    iconHeight = 25,
    iconAnchorX = 12, 
    iconAnchorY = 25
  )
  
  # ============================================
  # Download button on about us page
  # ============================================
  output$downloadData <- downloadHandler(
    filename = function() {
      "bigfoot_data_clean.csv"
    },
    content = function(file) {
      file.copy("bigfoot_data_clean.csv", file)
    }
  )
  
  
  
  # ============================================
  # WORD CLOUD
  # ============================================
  output$WordCloud <- renderPlot({
    
    removed_words <- c("like", "the", "also", "didnt", "there", "got", "this", "just", "didnt", "one")
    
    kept_words <- sapply(1:nrow(bigfoot_data_for_words),
                         function(n) {
                           words <- str_split(bigfoot_data$observed[n], "\\s")[[1]]
                           words <- tolower(words)
                           words <- gsub(".", "", words, fixed = TRUE)
                           paste(words[!words %in% removed_words], collapse = " ")
                         })
    
    par(bg = NA)
    
    wordcloud(kept_words, 
              colors = brewer.pal(8, "Dark2"),
              max.words = input$max_words
    )
  }, height = 600, bg = "transparent")
  
  # ============================================
  # VISUALIZATIONS (Season, State, Temperature)
  # ============================================

  # making the visualizations for sightings by season, state, and temperature and adding an interactive component
 
  output$selectedPlot <- renderPlot({
    if(input$plotChoice == "Sightings per Season") {
      source("Sightings-per-season-bar.R")
      season_bar
    } else if(input$plotChoice == "Sightings per State") {
      source("sightings-per-state.R")
      state_bar
    } else if(input$plotChoice == "Sightings per Temperature") {
      source("sightings-by-temp.R")
      temp_high_bar
    }
  })
  
  output$plots_text <- renderText({
    if(!is.null(input$plots_click)) {
      if(input$plotChoice == "Sightings per Season") {
        source("Sightings-per-season-bar.R")
        keepRows <- round(input$plots_click$x) == as.numeric(bigfoot_season$season)
        rows <- bigfoot_season[keepRows,]
        return(paste("Sightings:", rows$n))
        
      } else if(input$plotChoice == "Sightings per State") {
        source("sightings-per-state.R")
        keepRows <- round(input$plots_click$x) == as.numeric(number_sightings_per_state$state)
        rows <- number_sightings_per_state[keepRows, ]
        return(paste("Sightings:", rows$n))
        
      } else if(input$plotChoice == "Sightings per Temperature") {
        source("sightings-by-temp.R")
        keepRows <- round(input$plots_click$x) == as.numeric(high_temp_summary$high_temp_bin)
        rows <- high_temp_summary[keepRows, ]
        return(paste("Sightings:", rows$n))
        
      } else {
        return("")
      }
    }
  })
  

  # ============================================
  # SIGHTINGS AND WEATHER MAP
  # ============================================
  
  # Load Bigfoot sightings data
  bigfoot_observations <- reactive({
    tryCatch({
      read.csv("bigfoot_data_clean.csv", stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification("Error loading bigfoot data", type = "error")
      return(data.frame())
    })
  })
  
  # Convert to Simple Features (SF) object with cleaned data
  bigfoot_sf <- reactive({
    df <- bigfoot_observations()
    
    if(nrow(df) == 0) return(NULL)
    
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
    
    # Parse date
    df$date_parsed <- as.Date(df$date, format = "%m/%d/%Y")
    
    # Try alternate format if first attempt fails
    if(sum(!is.na(df$date_parsed)) == 0) {
      df$date_parsed <- as.Date(df$date, format = "%Y-%m-%d")
    }
    
    df$year <- year(df$date_parsed)
    
    # Remove rows with missing coordinates or dates
    df_clean <- df %>%
      filter(!is.na(lat) & !is.na(long) & !is.na(year)) %>%
      filter(lat >= -90 & lat <= 90) %>%
      filter(long >= -180 & long <= 180)
    
    if(nrow(df_clean) == 0) {
      return(NULL)
    }
    
    # Convert to SF object
    sf_obj <- st_as_sf(df_clean, 
                       coords = c("long", "lat"),
                       crs = 4326,
                       remove = FALSE)
    
    return(sf_obj)
  })
  
  # Load weather data
  weather_data <- reactive({
    req(bigfoot_sf())
    df <- bigfoot_sf() %>% st_drop_geometry()
    return(df)
  })
  
  # Update slider ranges based on actual data
  observe({
    data <- bigfoot_sf()
    req(data)
    
    if(nrow(data) > 0) {
      min_year <- min(data$year, na.rm = TRUE)
      max_year <- max(data$year, na.rm = TRUE)
      
      updateSliderInput(session, "year_slider",
                        min = min_year,
                        max = max_year,
                        value = min_year)
    }
    
    # Update state filter choices
    states <- sort(unique(data$state[!is.na(data$state)]))
    
    updateSelectInput(session, "map_state",
                      choices = c("All States", states),
                      selected = "All States")
    
    updateSelectInput(session, "state_filter",
                      choices = states,
                      selected = NULL)
  })
  
  # Filter Bigfoot data based on inputs
  filtered_sf <- reactive({
    req(input$view_type, input$year_slider)
    data <- bigfoot_sf()
    req(data)
    
    # Filter by year based on view type
    if (input$view_type == "cumulative") {
      data <- data %>% filter(year <= input$year_slider)
    } else {
      data <- data %>% filter(year == input$year_slider)
    }
    
    # Filter by selected state if not "All States"
    if(!is.null(input$map_state) && input$map_state != "All States") {
      data <- data %>% filter(state == input$map_state)
    }
    
    return(data)
  })
  
  # Filter weather data based on year filters and selected state
  filtered_weather <- reactive({
    req(input$view_type, input$year_slider)
    df <- weather_data()
    
    # Filter by year based on view type
    if (input$view_type == "cumulative") {
      df <- df %>% filter(!is.na(year) & year <= input$year_slider)
    } else {
      df <- df %>% filter(!is.na(year) & year == input$year_slider)
    }
    
    # Filter by selected state if not "All States"
    if(!is.null(input$map_state) && input$map_state != "All States") {
      df <- df %>% filter(state == input$map_state)
    }
    
    # LIMIT ROWS BEFORE AGGREGATION to prevent memory issues
    if(nrow(df) > 50000) {
      df <- df %>% sample_n(50000)
    }
    
    # Group by location and aggregate weather data
    df %>%
      group_by(lat, long, county, state) %>%
      summarise(
        temp_high_avg = mean(temperature_high, na.rm = TRUE),
        temp_low_avg = mean(temperature_low, na.rm = TRUE),
        precip_total = sum(precip_intensity, na.rm = TRUE),
        wind_speed_avg = mean(wind_speed, na.rm = TRUE),
        wind_bearing_avg = mean(wind_bearing, na.rm = TRUE),
        visibility_avg = mean(visibility, na.rm = TRUE),
        n_observations = n(),
        .groups = "drop"
      ) %>%
      filter(!is.na(lat) & !is.na(long))
  })
  
  # Load state boundaries
  state_boundaries <- reactive({
    if(input$show_state_boundaries) {
      states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
      return(states)
    }
    return(NULL)
  })
  
  # Zoom to selected state
  observe({
    req(input$map_state)
    
    if(input$map_state == "All States") {
      leafletProxy("map") %>%
        setView(lng = -98, lat = 39, zoom = 4)
    } else {
      df <- weather_data()
      state_data <- df %>% filter(state == input$map_state)
      
      if(nrow(state_data) > 0) {
        center_lng <- mean(state_data$long, na.rm = TRUE)
        center_lat <- mean(state_data$lat, na.rm = TRUE)
        
        leafletProxy("map") %>%
          setView(lng = center_lng, lat = center_lat, zoom = 7)
      }
    }
  })
  
  # Color palettes for weather
  temp_high_pal <- colorNumeric(palette = "YlOrRd", domain = c(0, 100))
  temp_low_pal <- colorNumeric(palette = "Blues", domain = c(0, 80))
  precip_pal <- colorNumeric(palette = c("#31a354", "#006d2c", "#00441b"), domain = c(0, 5))
  visibility_pal <- colorNumeric(palette = "Purples", domain = c(0, 10))
  
  # Create base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = -98, lat = 39, zoom = 4) %>%
      addScaleBar(position = "bottomleft")
  })
  
  # Update map based on all selections
  observe({
    bigfoot_data_clean <- filtered_sf()
    weather_df <- filtered_weather()
    
    req(bigfoot_data_clean)
    
    # Clear existing layers
    map <- leafletProxy("map") %>%
      clearHeatmap() %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      clearShapes() %>%
      clearControls()
    
    # Add state boundaries if requested
    if(input$show_state_boundaries && !is.null(state_boundaries())) {
      map <- map %>%
        addPolygons(
          data = state_boundaries(),
          fillColor = "transparent",
          color = "white",
          weight = 1,
          opacity = 0.5
        )
    }
    
    # Add Bigfoot visualization based on mode
    if(input$view_mode == "none") {
      # Do nothing - no sightings layer shown
      
    } else if(input$view_mode == "heat" && nrow(bigfoot_data_clean) > 0) {
      
      # LIMIT HEATMAP POINTS
      heat_data <- bigfoot_data_clean
      if(nrow(heat_data) > 10000) {
        heat_data <- heat_data %>% sample_n(10000)
      }
      
      map <- map %>%
        addHeatmap(
          data = heat_data,
          lng = ~long,
          lat = ~lat,
          intensity = 2,
          radius = input$heatmap_radius,
          blur = input$heatmap_blur,
          max = 0.8,
          minOpacity = 0.3,
          group = "Bigfoot Sightings"
        )
      
    } else if(input$view_mode == "cluster" && nrow(bigfoot_data_clean) > 0) {
      
      # Merge observation data - clustered
      data_with_obs <- bigfoot_data_clean %>%
        mutate(marker_id = paste0("marker_", seq_len(n())))
      
      map <- map %>%
        addCircleMarkers(
          data = data_with_obs,
          lng = ~long,
          lat = ~lat,
          radius = 4,
          color = "#ff6b6b",
          fillOpacity = 0.6,
          stroke = TRUE,
          weight = 1,
          layerId = ~marker_id,
          clusterOptions = markerClusterOptions(
            showCoverageOnHover = FALSE,
            zoomToBoundsOnClick = TRUE
          ),
          popup = ~paste0(
            "<b>Date:</b> ", date_parsed, "<br>",
            "<b>State:</b> ", state, "<br>",
            "<b>County:</b> ", county, "<br>",
            "<b>Observation:</b> ", observed
          ),
          group = "Bigfoot Sightings"
        )
      
    } else if(input$view_mode == "circles" && nrow(bigfoot_data_clean) > 0) {
      
      # LIMIT CIRCLE MARKERS
      data_with_obs <- bigfoot_data_clean
      if(nrow(data_with_obs) > 5000) {
        data_with_obs <- data_with_obs %>% sample_n(5000)
      }
      
      data_with_obs <- data_with_obs %>%
        mutate(marker_id = paste0("marker_", seq_len(n())))
      
      map <- map %>%
        addCircleMarkers(
          data = data_with_obs,
          lng = ~long,
          lat = ~lat,
          radius = input$circle_size,
          color = "#ff6b6b",
          fillOpacity = 0.4,
          stroke = TRUE,
          weight = 1,
          opacity = 0.7,
          layerId = ~marker_id,
          popup = ~paste0(
            "<b>Date:</b> ", date_parsed, "<br>",
            "<b>Year:</b> ", year, "<br>",
            "<b>State:</b> ", state, "<br>",
            "<b>County:</b> ", county, "<br>",
            "<b>Observation:</b> ", observed
          ),
          group = "Bigfoot Sightings"
        )
    }
    
    # Add Weather layers
    if(nrow(weather_df) > 0) {
      
      # Add High Temperature layer
      if("temp_high" %in% input$weather_layers) {
        map <- map %>%
          addCircleMarkers(
            data = weather_df,
            lng = ~long,
            lat = ~lat,
            radius = 8,
            color = ~temp_high_pal(temp_high_avg),
            fillColor = ~temp_high_pal(temp_high_avg),
            fillOpacity = 0.7,
            stroke = TRUE,
            weight = 2,
            popup = ~paste0("<b>", county, ", ", state, "</b><br>",
                            "High Temp: ", round(temp_high_avg, 1), "°F<br>",
                            "Observations: ", n_observations),
            group = "High Temperature"
          )
      }
      
      # Add Low Temperature layer
      if("temp_low" %in% input$weather_layers) {
        map <- map %>%
          addCircleMarkers(
            data = weather_df,
            lng = ~long,
            lat = ~lat,
            radius = 6,
            color = ~temp_low_pal(temp_low_avg),
            fillColor = ~temp_low_pal(temp_low_avg),
            fillOpacity = 0.7,
            stroke = TRUE,
            weight = 2,
            popup = ~paste0("<b>", county, ", ", state, "</b><br>",
                            "Low Temp: ", round(temp_low_avg, 1), "°F<br>",
                            "Observations: ", n_observations),
            group = "Low Temperature"
          )
      }
      
      # Add Precipitation layer
      if("precip" %in% input$weather_layers) {
        map <- map %>%
          addCircleMarkers(
            data = weather_df,
            lng = ~long,
            lat = ~lat,
            radius = ~sqrt(precip_total) * 5,
            color = ~precip_pal(precip_total),
            fillColor = ~precip_pal(precip_total),
            fillOpacity = 0.6,
            stroke = TRUE,
            weight = 1,
            popup = ~paste0("<b>", county, ", ", state, "</b><br>",
                            "Total Precip: ", round(precip_total, 2), " in<br>",
                            "Observations: ", n_observations),
            group = "Precipitation"
          )
      }
      
      # Add Wind arrows - RESTRICTED TO SELECTED STATES ONLY
      if("wind" %in% input$weather_layers) {
        
        wind_df <- weather_df
        
        # Check if user has selected states in state_filter
        if(!is.null(input$state_filter) && length(input$state_filter) > 0) {
          
          # Filter to selected states only
          wind_df <- weather_df %>% filter(state %in% input$state_filter)
          
          # CRITICAL: Limit to 100 arrows TOTAL across all selected states
          if(nrow(wind_df) > 100) {
            wind_df <- wind_df %>% sample_n(100)
          }
          
          # Only proceed if we have data
          if(nrow(wind_df) > 0) {
            for(i in 1:nrow(wind_df)) {
              map <- map %>%
                addMarkers(
                  lng = wind_df$long[i],
                  lat = wind_df$lat[i],
                  icon = makeIcon(
                    iconUrl = sprintf("data:image/svg+xml,%%3Csvg xmlns='http://www.w3.org/2000/svg' width='40' height='40' viewBox='0 0 40 40'%%3E%%3Cg transform='rotate(%f 20 20)'%%3E%%3Cpolygon points='20,5 28,18 23,18 23,30 17,30 17,18 12,18' fill='%%23FF4444' stroke='%%23CC0000' stroke-width='1.5'%%3E%%3C/polygon%%3E%%3C/g%%3E%%3C/svg%%3E", 
                                      wind_df$wind_bearing_avg[i]),
                    iconWidth = 40, iconHeight = 40
                  ),
                  popup = paste0("<b>", wind_df$county[i], ", ", wind_df$state[i], "</b><br>",
                                 "Wind Speed: ", round(wind_df$wind_speed_avg[i], 1), " mph<br>",
                                 "Direction: ", round(wind_df$wind_bearing_avg[i], 0), "°<br>",
                                 "Observations: ", wind_df$n_observations[i]),
                  group = "Wind"
                )
            }
          }
          
        } else {
          # No states selected - show notification
          showNotification(
            "Please select one or more states in the 'Wind: Select States' dropdown to view wind arrows",
            type = "warning",
            duration = 4
          )
        }
      }
      
      # Add Visibility layer
      if("visibility" %in% input$weather_layers) {
        map <- map %>%
          addCircleMarkers(
            data = weather_df,
            lng = ~long,
            lat = ~lat,
            radius = 7,
            color = ~visibility_pal(visibility_avg),
            fillColor = ~visibility_pal(visibility_avg),
            fillOpacity = ~visibility_avg / 10,
            stroke = TRUE,
            weight = 2,
            popup = ~paste0("<b>", county, ", ", state, "</b><br>",
                            "Visibility: ", round(visibility_avg, 1), " mi<br>",
                            "Observations: ", n_observations),
            group = "Visibility"
          )
      }
    }
    
    map
  })
  
  # Sighting count output
  output$sighting_count <- renderText({
    data <- filtered_sf()
    req(data)
    paste("Total Sightings:", format(nrow(data), big.mark = ","))
  })
  
  # Year info output
  output$year_info <- renderText({
    data <- filtered_sf()
    req(data)
    if(nrow(data) > 0) {
      if(input$view_type == "cumulative") {
        paste("Cumulative data through:", input$year_slider)
      } else {
        paste("Year:", input$year_slider)
      }
    } else {
      "No data available"
    }
  })
  
  # Year description output
  output$year_description <- renderText({
    req(bigfoot_sf())
    data <- filtered_sf()
    if(input$view_type == "cumulative") {
      paste("Showing all sightings from", min(bigfoot_sf()$year, na.rm = TRUE), 
            "to", input$year_slider)
    } else {
      paste("Showing sightings from", input$year_slider, "only")
    }
  })
  
  
  # Weather Correlation Scatter Plot
  output$weather_correlation_scatter_plot <- renderPlot({
    
    # Read the data directly
    bigfoot_data_clean <- read.csv("bigfoot_data_clean.csv", stringsAsFactors = FALSE)
    
    # Check if a valid weather layer is selected
    if (input$weather_layers %in% names(weather_config)) {
      
      # Get configuration for selected weather variable
      config <- weather_config[[input$weather_layers]]
      
      # Create the plot using the helper function
      create_weather_correlation_plot(
        data = bigfoot_data_clean,
        x_var = config$var,
        title = config$title,
        x_label = config$x_label,
        slope_digits = config$slope_digits
      )
      
    } else {
      # Return empty plot for other selections
      par(bg = "#0a0e27")
      plot.new()
      text(0.5, 0.5, "Select a Weather Variable to see correlation analysis", 
           cex = 1.2, col = "white")
    }
    
  }, bg = "#0a0e27")
  
  # Timeline plot showing all data
  output$timeline_plot <- renderPlot({
    data <- bigfoot_sf() %>%
      st_drop_geometry() %>%
      filter(year <= input$year_slider)
    
    if(nrow(data) == 0) {
      plot.new()
      text(0.5, 0.5, "No data available", cex = 1.5, col = "white")
      return()
    }
    
    year_counts <- data %>%
      group_by(year) %>%
      summarise(count = n(), .groups = "drop")
    
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
    
    abline(v = input$year_slider, col = "#4ecdc4", lwd = 2, lty = 2)
    grid(col = "#333333", lty = 1)
  })
  
  # Legend
  output$legend_info <- renderUI({
    HTML("
      <style>
        .legend-item { margin: 8px 0; }
        .legend-color { display: inline-block; width: 20px; height: 20px; margin-right: 8px; vertical-align: middle; }
      </style>
      <h4 style='color: white;'>Weather Layers</h4>
      <div class='legend-item'>
        <span class='legend-color' style='background: linear-gradient(to right, #FFFFB2, #FD8D3C, #BD0026);'></span>
        High Temp (°F)
      </div>
      <div class='legend-item'>
        <span class='legend-color' style='background: linear-gradient(to right, #EFF3FF, #6BAED6, #08519C);'></span>
        Low Temp (°F)
      </div>
      <div class='legend-item'>
        <span class='legend-color' style='background: linear-gradient(to right, #31a354, #006d2c, #00441b);'></span>
        Precipitation (in)
      </div>
      <div class='legend-item'>
        <span style='color: #FF4444; font-size: 20px; margin-right: 8px;'>▲</span>
        Wind Direction
      </div>
      <div class='legend-item'>
        <span class='legend-color' style='background: linear-gradient(to right, #FCFBFD, #9E9AC8, #54278F);'></span>
        Visibility (mi)
      </div>
      <h4 style='color: white; margin-top: 15px;'>Bigfoot Sightings</h4>
      <div class='legend-item'>
        <span style='color: #ff6b6b; font-size: 20px; margin-right: 8px;'>●</span>
        Sighting Location
      </div>
    ")
  })
  
  # ============================================
  # MOON PHASE TRACKER
  # ============================================
  
  # Load bigfoot moon phase data
  bigfoot_moon <- reactive({
    df <- read.csv("filtered_data_date_moon_phase.csv")
    df$date <- mdy(df$date)
    df <- df %>% filter(!is.na(moon_phase))
    return(df)
  })
  
  # Bin sightings by phase
  phase_bins <- reactive({
    df <- bigfoot_moon()
    
    # Create phase bins (8 major phases) - each sighting in exactly one bin
    df <- df %>%
      mutate(
        phase_bin = case_when(
          moon_phase >= 0 & moon_phase < 0.125 ~ "New Moon",
          moon_phase >= 0.125 & moon_phase < 0.25 ~ "Waxing Crescent",
          moon_phase >= 0.25 & moon_phase < 0.375 ~ "First Quarter",
          moon_phase >= 0.375 & moon_phase < 0.5 ~ "Waxing Gibbous",
          moon_phase >= 0.5 & moon_phase < 0.625 ~ "Full Moon",
          moon_phase >= 0.625 & moon_phase < 0.75 ~ "Waning Gibbous",
          moon_phase >= 0.75 & moon_phase < 0.875 ~ "Last Quarter",
          moon_phase >= 0.875 & moon_phase <= 1 ~ "Waning Crescent",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(phase_bin)) %>%
      group_by(phase_bin) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    return(df)
  })
  
  # Get count for current phase
  current_phase_count <- reactive({
    phase <- input$phase_slider
    df <- bigfoot_moon()
    
    # Find sightings within 0.0625 of current phase (1/16th of cycle)
    nearby <- df %>%
      filter(abs(moon_phase - phase) < 0.0625 | 
               abs(moon_phase - phase) > 0.9375)
    
    return(nrow(nearby))
  })
  
  # Reactive values for animation
  animation_active <- reactiveVal(FALSE)
  
  # Start animation
  observeEvent(input$animate, {
    animation_active(TRUE)
  })
  
  # Stop animation
  observeEvent(input$stop, {
    animation_active(FALSE)
  })
  
  # Animation loop
  observe({
    invalidateLater(1000 / input$speed, session)
    
    if (animation_active()) {
      new_phase <- input$phase_slider + 0.01
      if (new_phase > 1) new_phase <- 0
      updateSliderInput(session, "phase_slider", value = new_phase)
    }
  })
  
  # Render moon plot
  output$moon_plot <- renderPlot({
    create_moon_plot(input$phase_slider, current_phase_count())
  }, bg = "#0a0e27")
  
  # Display phase name
  output$phase_name <- renderText({
    get_phase_name(input$phase_slider)
  })
  
  # Display current sightings
  output$current_sightings <- renderText({
    paste(current_phase_count(), "sightings")
  })
  
  # Display most sightings phase
  output$most_sightings_phase <- renderText({
    bins <- phase_bins()
    as.character(bins$phase_bin[1])
  })
  
  output$most_sightings_count <- renderText({
    bins <- phase_bins()
    paste(bins$count[1], "sightings")
  })
  
  # Display least sightings phase
  output$least_sightings_phase <- renderText({
    bins <- phase_bins()
    as.character(bins$phase_bin[nrow(bins)])
  })
  
  output$least_sightings_count <- renderText({
    bins <- phase_bins()
    paste(bins$count[nrow(bins)], "sightings")
  })
  
  # Phase distribution correlation plot
  output$phase_distribution <- renderPlot({
    bins <- phase_bins()
    
    # Add moon brightness values (0 = New Moon/darkest, 1 = Full Moon/brightest)
    bins <- bins %>%
      mutate(
        brightness = case_when(
          phase_bin == "New Moon" ~ 0,
          phase_bin == "Waxing Crescent" ~ 0.25,
          phase_bin == "First Quarter" ~ 0.5,
          phase_bin == "Waxing Gibbous" ~ 0.75,
          phase_bin == "Full Moon" ~ 1,
          phase_bin == "Waning Gibbous" ~ 0.75,
          phase_bin == "Last Quarter" ~ 0.5,
          phase_bin == "Waxing Crescent" ~ 0.25
        )
      )
    
    # Calculate linear regression model
    model <- lm(count ~ brightness, data = bins)
    r_squared <- summary(model)$r.squared
    p_value <- summary(model)$coefficients[2, 4]  # p-value for brightness coefficient
    slope <- coef(model)[2]
    
    # Create scatter plot with trend line
    ggplot(bins, aes(x = brightness, y = count)) +
      geom_point(aes(color = phase_bin), size = 5, alpha = 0.8) +
      geom_smooth(method = "lm", se = TRUE, color = "#f9ca24", fill = "#f9ca24", alpha = 0.2, linewidth = 1.5) +
      # Add text labels showing count for each point
      geom_text(aes(label = count), 
                vjust = -1.2, 
                color = "#ffffff", 
                size = 4, 
                fontface = "bold") +
      scale_color_manual(values = c(
        "New Moon" = "#2c3e50",
        "Waxing Crescent" = "#95a5a6",
        "First Quarter" = "#bdc3c7",
        "Waxing Gibbous" = "#ecf0f1",
        "Full Moon" = "#f9ca24",
        "Waning Gibbous" = "#ecf0f1",
        "Last Quarter" = "#bdc3c7",
        "Waning Crescent" = "#95a5a6"
      )) +
      scale_x_continuous(
        breaks = c(0, 0.25, 0.5, 0.75, 1),
        labels = c("New\nMoon\n(Darkest)", "Crescent", "Quarter", "Gibbous", "Full\nMoon\n(Brightest)")
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +  # Add space at top for labels
      labs(
        title = paste0("Bigfoot Sightings vs Moon Brightness"),
        subtitle = paste0(
          "R² = ", round(r_squared, 3), 
          " | Slope = ", round(slope, 2),
          " | p-value = ", format.pval(p_value, digits = 3),
          "\n",
          if(p_value < 0.05 & slope < 0) {
            "Significant negative relationship: MORE sightings during darker moon phases"
          } else if(p_value < 0.05 & slope > 0) {
            "Significant positive relationship: MORE sightings during brighter moon phases"
          } else {
            "No significant relationship between moon phase and sightings"
          }
        ),
        x = "Moon Brightness",
        y = "Number of Sightings",
        color = "Moon Phase"
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#0a0e27", color = NA),
        panel.background = element_rect(fill = "#1e2742", color = NA),
        text = element_text(color = "#ffffff", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14, color = "#f9ca24"),
        axis.text = element_text(color = "#ffffff"),
        axis.title = element_text(size = 13, face = "bold"),
        legend.background = element_rect(fill = "#1e2742", color = NA),
        legend.key = element_rect(fill = "#1e2742"),
        legend.text = element_text(color = "#ffffff"),
        legend.title = element_text(color = "#ffffff", face = "bold"),
        panel.grid.major = element_line(color = "#2c3e50", linewidth = 0.5),
        panel.grid.minor = element_line(color = "#2c3e50", linewidth = 0.25)
      )
  }, bg = "#0a0e27")
  
  # Add a summary table output
  output$phase_summary_table <- renderTable({
    bins <- phase_bins()
    
    # Add moon brightness values
    bins <- bins %>%
      mutate(
        brightness = case_when(
          phase_bin == "New Moon" ~ 0,
          phase_bin == "Waxing Crescent" ~ 0.25,
          phase_bin == "First Quarter" ~ 0.5,
          phase_bin == "Waxing Gibbous" ~ 0.75,
          phase_bin == "Full Moon" ~ 1,
          phase_bin == "Waning Gibbous" ~ 0.75,
          phase_bin == "Last Quarter" ~ 0.5,
          phase_bin == "Waning Crescent" ~ 0.25
        )
      ) %>%
      arrange(brightness) %>%
      select(`Moon Phase` = phase_bin, 
             `Brightness` = brightness, 
             `Sightings` = count)
    
    return(bins)
  }, 
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  width = "100%",
  align = 'c',
  bg = "#1e2742",
  color = "#ffffff")
  
  # ============================================
  # Topographic map 
  # ============================================
  
  # Base map with OpenTopoMap and Bigfoot sightings
  output$topographic_map <- renderLeaflet({
    leaflet(bigfoot_data_for_topographic_map) %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addProviderTiles(providers$OpenTopoMap) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 4,
        color = "#0000FF",
        fillColor = "#4169E1",
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 1,
        popup = ~paste("Bigfoot Sighting<br>",
                       "Lat:", round(latitude, 4), "<br>",
                       "Lon:", round(longitude, 4))
      )
  })
  
  # Render analysis output
  output$analysis_output <- renderUI({
    tagList(
      fluidRow(
        column(4,
               div(style = "text-align: center; padding: 15px; background: rgba(255,255,255,0.2); border-radius: 8px; margin: 5px;",
                   h4(style = "margin: 5px 0;", "R-squared"),
                   p(style = "font-size: 24px; font-weight: bold; margin: 5px 0;", r_squared),
                   p(style = "font-size: 12px; margin: 5px 0; opacity: 0.9;", 
                     "Proportion of variance explained")
               )
        ),
        column(4,
               div(style = "text-align: center; padding: 15px; background: rgba(255,255,255,0.2); border-radius: 8px; margin: 5px;",
                   h4(style = "margin: 5px 0;", "P-value"),
                   p(style = "font-size: 24px; font-weight: bold; margin: 5px 0;", p_value),
                   p(style = "font-size: 12px; margin: 5px 0; opacity: 0.9;", 
                     ifelse(p_value < 0.05, "Statistically significant", "Not statistically significant"))
               )
        ),
        column(4,
               div(style = "text-align: center; padding: 15px; background: rgba(255,255,255,0.2); border-radius: 8px; margin: 5px;",
                   h4(style = "margin: 5px 0;", "Slope"),
                   p(style = "font-size: 24px; font-weight: bold; margin: 5px 0;", slope),
                   p(style = "font-size: 12px; margin: 5px 0; opacity: 0.9;", 
                     "Sightings per foot of elevation")
               )
        )
      ),
      p(style = "margin-top: 15px; font-size: 14px; opacity: 0.9;",
        strong("Interpretation: "), 
        if(p_value < 0.05) {
          paste("There is a statistically significant relationship between state elevation and Bigfoot sightings. ",
                ifelse(slope > 0, 
                       "Higher elevation states tend to have more sightings.", 
                       "Higher elevation states tend to have fewer sightings."))
        } else {
          "There is no statistically significant relationship between state elevation and Bigfoot sightings."
        }
      )
    )
  })
  
  # Render correlation plot
  output$correlation_plot <- renderPlot({
    par(mar = c(5, 5, 3, 2))
    plot(analysis_data$Average.Elevation, analysis_data$Sightings,
         xlab = "Average State Elevation (feet)",
         ylab = "Number of Bigfoot Sightings",
         main = "Correlation between Elevation and Bigfoot Sightings",
         pch = 19,
         col = "#4169E1",
         cex = 2.0,
         cex.lab = 1.8,
         cex.main = 1.8,
         cex.axis = 1.4)
    
    # Add regression line
    abline(lm_model, col = "#D7191C", lwd = 2)
    
    # Add state labels
    text(analysis_data$Average.Elevation, analysis_data$Sightings,
         labels = analysis_data$States,
         pos = 3,
         cex = 1.0,
         col = "#333333")
    
    # Add legend with statistics
    legend("topright",
           legend = c(
             paste("R² =", r_squared),
             paste("p-value =", p_value),
             paste("Slope =", slope)
           ),
           bty = "n",
           cex = 1.4)
    
    # Add grid
    grid(col = "gray80", lty = "dotted")
  })
  
  # Add this code to your server function (inside the main server function)
  
  # ============================================
  # BIGFOOT PROBABILITY PREDICTOR
  # ============================================
  
  # Create the matrix data with bins
  bigfoot_matrix_data <- reactive({
    df <- read.csv("bigfoot_data_clean_with_time.csv", stringsAsFactors = FALSE)
    
    # Create bins for continuous variables
    df <- df %>%
      mutate(
        # Temperature bins (10 degree ranges)
        temp_bin = cut(temperature_high, 
                       breaks = seq(0, 100, by = 10),
                       labels = c("0-10°F", "10-20°F", "20-30°F", "30-40°F", 
                                  "40-50°F", "50-60°F", "60-70°F", "70-80°F", 
                                  "80-90°F", "90-100°F"),
                       include.lowest = TRUE),
        
        # Visibility bins
        vis_bin = cut(visibility,
                      breaks = c(0, 5, 10, 15, 20, Inf),
                      labels = c("0-5 mi", "5-10 mi", "10-15 mi", "15-20 mi", "20+ mi"),
                      include.lowest = TRUE),
        
        # Moon phase bins (8 phases)
        moon_bin = case_when(
          is.na(moon_phase) ~ "Unknown",
          moon_phase >= 0 & moon_phase < 0.125 ~ "New Moon",
          moon_phase >= 0.125 & moon_phase < 0.25 ~ "Waxing Crescent",
          moon_phase >= 0.25 & moon_phase < 0.375 ~ "First Quarter",
          moon_phase >= 0.375 & moon_phase < 0.5 ~ "Waxing Gibbous",
          moon_phase >= 0.5 & moon_phase < 0.625 ~ "Full Moon",
          moon_phase >= 0.625 & moon_phase < 0.75 ~ "Waning Gibbous",
          moon_phase >= 0.75 & moon_phase < 0.875 ~ "Last Quarter",
          moon_phase >= 0.875 & moon_phase <= 1 ~ "Waning Crescent",
          TRUE ~ "Unknown"
        ),
        
        # Capitalize time_of_day for consistency
        time_bin = case_when(
          is.na(time_of_day) | time_of_day == "" ~ "Unknown",
          tolower(time_of_day) == "dawn" ~ "Dawn",
          tolower(time_of_day) == "morning" ~ "Morning",
          tolower(time_of_day) == "afternoon" ~ "Afternoon",
          tolower(time_of_day) == "dusk" ~ "Dusk",
          tolower(time_of_day) == "night" ~ "Night",
          TRUE ~ "Unknown"
        )
      ) %>%
      filter(!is.na(temp_bin) & !is.na(vis_bin) & !is.na(moon_bin) & 
               !is.na(season) & !is.na(state))
    
    return(df)
  })
  

# Create the lookup matrix
probability_matrix <- reactive({
  df <- bigfoot_matrix_data()
  
  total_sightings <- nrow(df)
  
  # Group by all 6 variables and calculate counts/percentages
  matrix <- df %>%
    group_by(state, season, temp_bin, moon_bin, time_bin, vis_bin) %>%
    summarise(
      count = n(),
      .groups = "drop"
    ) %>%
    mutate(
      percentage = (count / total_sightings) * 100
    )
  
  return(matrix)
})

# Update state choices for predictor
observe({
  data <- bigfoot_matrix_data()
  states <- sort(unique(data$state[!is.na(data$state)]))
  
  updateSelectInput(session, "pred_state",
                    choices = states,
                    selected = states[1])
})

# Helper function to convert user inputs to bins
user_inputs_to_bins <- function(temp, vis, moon) {
  
  # Temperature bin
  temp_bin <- case_when(
    temp >= 0 & temp < 10 ~ "0-10°F",
    temp >= 10 & temp < 20 ~ "10-20°F",
    temp >= 20 & temp < 30 ~ "20-30°F",
    temp >= 30 & temp < 40 ~ "30-40°F",
    temp >= 40 & temp < 50 ~ "40-50°F",
    temp >= 50 & temp < 60 ~ "50-60°F",
    temp >= 60 & temp < 70 ~ "60-70°F",
    temp >= 70 & temp < 80 ~ "70-80°F",
    temp >= 80 & temp < 90 ~ "80-90°F",
    temp >= 90 & temp <= 100 ~ "90-100°F",
    TRUE ~ NA_character_
  )
  
  # Visibility bin
  vis_bin <- case_when(
    vis >= 0 & vis < 5 ~ "0-5 mi",
    vis >= 5 & vis < 10 ~ "5-10 mi",
    vis >= 10 & vis < 15 ~ "10-15 mi",
    vis >= 15 & vis < 20 ~ "15-20 mi",
    vis >= 20 ~ "20+ mi",
    TRUE ~ NA_character_
  )
  
  # Moon phase bin
  moon_bin <- case_when(
    moon >= 0 & moon < 0.125 ~ "New Moon",
    moon >= 0.125 & moon < 0.25 ~ "Waxing Crescent",
    moon >= 0.25 & moon < 0.375 ~ "First Quarter",
    moon >= 0.375 & moon < 0.5 ~ "Waxing Gibbous",
    moon >= 0.5 & moon < 0.625 ~ "Full Moon",
    moon >= 0.625 & moon < 0.75 ~ "Waning Gibbous",
    moon >= 0.75 & moon < 0.875 ~ "Last Quarter",
    moon >= 0.875 & moon <= 1 ~ "Waning Crescent",
    TRUE ~ NA_character_
  )
  
  return(list(temp_bin = temp_bin, vis_bin = vis_bin, moon_bin = moon_bin))
}

# Lookup probability when button is clicked
matrix_probability_result <- eventReactive(input$calculate_prob, {
  
  matrix <- probability_matrix()
  
  # Convert user inputs to bins
  bins <- user_inputs_to_bins(input$pred_temp, input$pred_visibility, input$pred_moon)
  
  # Capitalize time input
  time_display <- case_when(
    input$pred_time == "dawn" ~ "Dawn",
    input$pred_time == "morning" ~ "Morning",
    input$pred_time == "afternoon" ~ "Afternoon",
    input$pred_time == "dusk" ~ "Dusk",
    input$pred_time == "night" ~ "Night",
    TRUE ~ "Unknown"
  )
  
  # Look up exact match in matrix
  match <- matrix %>%
    filter(
      state == input$pred_state,
      season == input$pred_season,
      temp_bin == bins$temp_bin,
      moon_bin == bins$moon_bin,
      time_bin == time_display,
      vis_bin == bins$vis_bin
    )
  
  # Get total sightings for context
  total_sightings <- nrow(bigfoot_matrix_data())
  
  if(nrow(match) > 0) {
    # Exact match found
    return(list(
      percentage = round(match$percentage[1], 3),
      count = match$count[1],
      total_sightings = total_sightings,
      found = TRUE,
      bins = bins,
      time_display = time_display
    ))
  } else {
    # No exact match - return 0%
    return(list(
      percentage = 0,
      count = 0,
      total_sightings = total_sightings,
      found = FALSE,
      bins = bins,
      time_display = time_display
    ))
  }
})

# Display probability
output$probability_display <- renderUI({
  req(input$calculate_prob)
  result <- matrix_probability_result()
  
  prob_value <- result$percentage
  
  # Color based on probability
  color <- if(prob_value == 0) {
    "#666666"  # Gray for zero
  } else if(prob_value < 0.1) {
    "#ff6b6b"  # Low - red
  } else if(prob_value < 0.5) {
    "#f9ca24"  # Medium - yellow
  } else {
    "#4ecdc4"  # High - cyan
  }
  
  div(
    tags$h1(style = paste0("color: ", color, "; font-size: 72px; font-weight: bold; margin: 20px 0;"),
            paste0(prob_value, "%")),
    tags$p(style = "font-size: 20px; color: #e4e4e4;",
           if(prob_value == 0) {
             "No Historical Sightings - Uncharted Territory!"
           } else if(prob_value < 0.1) {
             "Very Rare Conditions - But not impossible!"
           } else if(prob_value < 0.5) {
             "Moderate Probability - Promising conditions!"
           } else {
             "High Probability - Prime Bigfoot conditions!"
           }),
    tags$p(style = "font-size: 16px; color: #4ecdc4; margin-top: 15px;",
           paste0(result$count, " out of ", result$total_sightings, 
                  " historical sightings matched your exact conditions"))
  )
})

# Probability gauge visualization
output$probability_gauge <- renderPlot({
  req(input$calculate_prob)
  result <- matrix_probability_result()
  
  # Cap display at 2% for gauge readability
  prob <- min(result$percentage, 2)
  max_display <- 2
  
  # Create gauge chart
  par(bg = "#1a1a1a", mar = c(0, 0, 0, 0))
  
  # Draw arc
  theta <- seq(pi, 0, length.out = 100)
  x <- cos(theta)
  y <- sin(theta)
  
  plot(x, y, type = "n", xlim = c(-1.2, 1.2), ylim = c(-0.2, 1.2),
       axes = FALSE, xlab = "", ylab = "", asp = 1)
  
  # Background arc
  lines(x, y, lwd = 20, col = "#2c3e50")
  
  # Filled arc based on probability
  prob_theta <- seq(pi, pi - (prob/max_display) * pi, length.out = 100)
  prob_x <- cos(prob_theta)
  prob_y <- sin(prob_theta)
  
  # Color gradient
  gauge_color <- if(prob == 0) "#666666" else if(prob < 0.1) "#ff6b6b" else if(prob < 0.5) "#f9ca24" else "#4ecdc4"
  lines(prob_x, prob_y, lwd = 20, col = gauge_color)
  
  # Add labels
  text(0, -0.1, "Historical Probability Meter", col = "white", cex = 1.2)
  text(-1, 0, "0%", col = "white", cex = 1)
  text(1, 0, paste0(max_display, "%"), col = "white", cex = 1)
  
  if(result$percentage > max_display) {
    text(0, 0.5, paste0("(", result$percentage, "%)"), col = "#4ecdc4", cex = 1.5, font = 2)
  }
  
}, bg = "#1a1a1a")

# Factor breakdown
output$factor_breakdown <- renderUI({
  req(input$calculate_prob)
  result <- matrix_probability_result()
  
  tagList(
    div(style = "margin: 10px 0;",
        tags$strong(style = "color: #f9ca24;", "State: "),
        tags$span(style = "color: #e4e4e4;", input$pred_state)
    ),
    div(style = "margin: 10px 0;",
        tags$strong(style = "color: #f9ca24;", "Season: "),
        tags$span(style = "color: #e4e4e4;", input$pred_season)
    ),
    div(style = "margin: 10px 0;",
        tags$strong(style = "color: #f9ca24;", "Temperature Range: "),
        tags$span(style = "color: #e4e4e4;", result$bins$temp_bin)
    ),
    div(style = "margin: 10px 0;",
        tags$strong(style = "color: #f9ca24;", "Moon Phase: "),
        tags$span(style = "color: #e4e4e4;", result$bins$moon_bin)
    ),
    div(style = "margin: 10px 0;",
        tags$strong(style = "color: #f9ca24;", "Time of Day: "),
        tags$span(style = "color: #e4e4e4;", result$time_display)
    ),
    div(style = "margin: 10px 0;",
        tags$strong(style = "color: #f9ca24;", "Visibility: "),
        tags$span(style = "color: #e4e4e4;", result$bins$vis_bin)
    ),
    hr(),
    div(style = "margin-top: 15px; padding: 15px; background-color: #1a1a1a; border-radius: 8px;",
        tags$p(style = "color: #4ecdc4; margin: 0;",
               if(result$found) {
                 "✓ These exact conditions appear in our historical database"
               } else {
                 "✗ These exact conditions have never been recorded in our database"
               })
    )
  )
})

# Recommendations
output$recommendations <- renderUI({
  req(input$calculate_prob)
  result <- matrix_probability_result()
  matrix <- probability_matrix()
  
  recommendations <- list()
  
  if(result$percentage == 0) {
    # Find what would improve their chances
    
    # Best state
    best_states <- matrix %>%
      group_by(state) %>%
      summarise(total_prob = sum(percentage)) %>%
      arrange(desc(total_prob)) %>%
      head(3)
    
    recommendations <- c(recommendations,
                         paste0("• Try these high-activity states: ", 
                                paste(best_states$state, collapse = ", ")))
    
    # Best season
    best_seasons <- matrix %>%
      group_by(season) %>%
      summarise(total_prob = sum(percentage)) %>%
      arrange(desc(total_prob)) %>%
      head(1)
    
    if(input$pred_season != best_seasons$season[1]) {
      recommendations <- c(recommendations,
                           paste0("• ", best_seasons$season[1], 
                                  " has the highest overall sighting rate"))
    }
    
    # Moon phase tip
    recommendations <- c(recommendations,
                         "• New Moon and darker phases correlate with more sightings")
    
    # Time of day tip
    recommendations <- c(recommendations,
                         "• Dawn and dusk are peak sighting times")
    
  } else if(result$percentage < 0.1) {
    recommendations <- c(recommendations,
                         "• Your conditions are rare but have historical precedent",
                         "• Consider adjusting season or location for better odds")
  } else {
    recommendations <- c(recommendations,
                         "• Your conditions are excellent based on historical data!",
                         "• Make sure to bring a camera",
                         "• Stay alert during dawn and dusk hours")
  }
  
  div(style = "color: #e4e4e4; font-size: 14px;",
      HTML(paste(recommendations, collapse = "<br>")))
})
  # ============================================
  # Correlation tab graphs and text
  # ============================================
  
  
  output$scatterPlot <- renderPlot({
    if (input$modelChoice == "model1") {
      create_model_plot(
        data = merged_data,
        x_var = "Forest.Coverage.Percent",
        y_var = "Sightings",
        model = model1,
        title = "Bigfoot Sightings vs Forest Coverage by State",
        x_label = "Forest Coverage (%)",
        y_label = "Number of Bigfoot Sightings",
        nudge_x = 0.5,
        nudge_y = 2,
        slope_digits = 2
      )
      
    } else if (input$modelChoice == "model2") {
      create_model_plot(
        data = merged_data,
        x_var = "Forest.Land.Area",
        y_var = "Sightings",
        model = model2,
        title = "Bigfoot Sightings vs Forest Land Area by State",
        x_label = "Forest Land Area (sq. miles)",
        y_label = "Number of Bigfoot Sightings",
        nudge_x = 1000,
        nudge_y = 2,
        slope_digits = 4
      )
      
    } else if (input$modelChoice == "model3") {
      create_model_plot(
        data = merged_data,
        x_var = "Census.Land.Area",
        y_var = "Sightings",
        model = model3,
        title = "Bigfoot Sightings vs Census Land Area by State",
        x_label = "Census Land Area (sq. miles)",
        y_label = "Number of Bigfoot Sightings",
        nudge_x = 2000,
        nudge_y = 2,
        slope_digits = 4
      )
      
    } else if (input$modelChoice == "model4") {
      # Bear sightings by state plot
      ggplot(merged_bigfoot_bear_state, aes(x = bear_obs, y = bigfoot_obs, label = state)) +
        geom_point(size = 5, color = "#0047AB", alpha = 0.8) +
        geom_text_repel(size = 5, color = "#ffffff", 
                        nudge_x = 50, nudge_y = 5,
                        segment.color = "#ffffff", segment.alpha = 0.5) +
        geom_smooth(method = "lm", se = TRUE, color = "#f9ca24", 
                    fill = "#f9ca24", alpha = 0.2, linewidth = 1.5) +
        labs(
          title = "Bear vs. Bigfoot Sightings by State",
          subtitle = paste0(
            "R² = ", round(summary(bigfoot_bear_state_lm)$r.squared, 3), 
            " | Slope = ", round(summary(bigfoot_bear_state_lm)$coefficients[2, 1], 2),
            " | p-value = ", format.pval(summary(bigfoot_bear_state_lm)$coefficients[2, 4], digits = 3)
          ),
          x = "Bear Sightings",
          y = "Bigfoot Sightings"
        ) +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "#0a0e27", color = NA),
          panel.background = element_rect(fill = "#1e2742", color = NA),
          text = element_text(color = "#ffffff", size = 20),
          plot.title = element_text(size = 16, face = "bold", color = "#ffffff"),
          plot.subtitle = element_text(size = 14, color = "#f9ca24"),
          axis.text = element_text(color = "#ffffff"),
          axis.title = element_text(size = 13, face = "bold", color = "#ffffff"),
          panel.grid.major = element_line(color = "#2c3e50", linewidth = 0.5),
          panel.grid.minor = element_line(color = "#2c3e50", linewidth = 0.25)
        )
      
    } else if (input$modelChoice == "model5") {
      # Bear sightings by county plot
      ggplot(merged_bigfoot_bear_county, aes(x = bear_obs, y = bigfoot_obs)) +
        geom_point(size = 5, color = "#0047AB", alpha = 0.8) +
        geom_smooth(method = "lm", se = TRUE, color = "#f9ca24", 
                    fill = "#f9ca24", alpha = 0.2, linewidth = 1.5) +
        labs(
          title = "Bear vs. Bigfoot Sightings by County, State",
          subtitle = paste0(
            "R² = ", round(summary(bigfoot_bear_county_lm)$r.squared, 3), 
            " | Slope = ", round(summary(bigfoot_bear_county_lm)$coefficients[2, 1], 2),
            " | p-value = ", format.pval(summary(bigfoot_bear_county_lm)$coefficients[2, 4], digits = 3)
          ),
          x = "Bear Sightings",
          y = "Bigfoot Sightings"
        ) +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "#0a0e27", color = NA),
          panel.background = element_rect(fill = "#1e2742", color = NA),
          text = element_text(color = "#ffffff", size = 20),
          plot.title = element_text(size = 16, face = "bold", color = "#ffffff"),
          plot.subtitle = element_text(size = 14, color = "#f9ca24"),
          axis.text = element_text(color = "#ffffff"),
          axis.title = element_text(size = 13, face = "bold", color = "#ffffff"),
          panel.grid.major = element_line(color = "#2c3e50", linewidth = 0.5),
          panel.grid.minor = element_line(color = "#2c3e50", linewidth = 0.25)
        )
    }
  }, bg = "#0a0e27")
  
  output$modelDescription <- renderUI({
    switch(input$modelChoice,
           "model1" = div(
             style = "background-color: #1e272e; color: #f9ca24; padding: 15px; margin-top: 20px; border-radius: 5px;",
             h4("Forest Coverage Percent"),
             p("This model examines the relationship between the percentage of forest coverage in each state and Bigfoot sightings. States with higher percentage of their total land covered by forests may allow for more Bigfoot sightings as he is typically spotted in wooded areas. However, this correlation is almost non-existent providing evidence that the percent of land covered by forests in each state does not matter for Bigfoots wandering pattern.")
           ),
           "model2" = div(
             style = "background-color: #1e272e; color: #f9ca24; padding: 15px; margin-top: 20px; border-radius: 5px;",
             h4("Forest Land Area"),
             p("This model looks at the total forest land area in thousand miles. Larger forested areas may correlate with more sightings due to greater wilderness exposure and exploration. This correlation is actually strong but this may be because larger states with more forests may be larger overall and thus have more people residing in them to make the observations.")
           ),
           "model3" = div(
             style = "background-color: #1e272e; color: #f9ca24; padding: 15px; margin-top: 20px; border-radius: 5px;",
             h4("Census Land Area"),
             p("This model analyzes the total land area of each state with Bigfoot sightings. Larger states may have more sightings simply due to more available territory and population exposure to wilderness areas. This correlation is weak however providing more evidence that the best way to tell which states will have more Bigfoot sightings is by how much total land of forest there is in the state.")
           ),
           "model4" = div(
             style = "background-color: #1e272e; color: #f9ca24; padding: 15px; margin-top: 20px; border-radius: 5px;",
             h4("Bear Sightings by State"),
             p("This model explores whether bear sightings at the state level correlate with Bigfoot sightings. The hypothesis is that if people are mistaking bears for Bigfoot, we would see a strong positive correlation between the two. Alternatively, both bears and Bigfoot may simply prefer similar habitats (forested wilderness areas), which could also explain a correlation.")
           ),
           "model5" = div(
             style = "background-color: #1e272e; color: #f9ca24; padding: 15px; margin-top: 20px; border-radius: 5px;",
             h4("Bear Sightings by County"),
             p("This model examines the relationship between bear and Bigfoot sightings at a more granular county level. A county-level analysis provides more data points and controls for within-state variation, potentially revealing whether the bear-Bigfoot correlation holds when examining smaller geographic areas where habitat conditions are more uniform.")
           )
    )
  })
  
}