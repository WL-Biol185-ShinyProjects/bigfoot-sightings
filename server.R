
#For web usage: By placing a link with the text "designed by {Author's Name} from Flaticon" in a visible spot, so the author's authorship is noticeable
bigfoot_data_for_words <- read.csv("bigfoot_data_wordcount_filtered_less_200.csv")





# for the elevation data, I deleted the front page that described the data. I edited the numbers so they had no commas or decimal points. I removed any of the non 50 U.S. states (U.S. territories) got the data from here: https://www.statista.com/statistics/1325529/lowest-points-united-states-state/

# Pre-load Bigfoot sighting data
bigfoot_data_for_topographic_map <- read.csv("filtered_data_lat_long_state")


# Pre-load elevation data
elevation_data <- read.csv("elevation_data.csv")
message("Loaded elevation data")

# Calculate number of sightings per state
sightings_per_state <- as.data.frame(table(bigfoot_data$state))
colnames(sightings_per_state) <- c("States", "Sightings")

# Merge with elevation data
analysis_data <- merge(elevation_data, sightings_per_state, by = "States")

# Run linear regression
lm_model <- lm(Sightings ~ Average.Elevation, data = analysis_data)
lm_summary <- summary(lm_model)

# Extract key statistics
r_squared <- round(lm_summary$r.squared, 4)
p_value <- round(lm_summary$coefficients[2, 4], 4)
slope <- round(lm_summary$coefficients[2, 1], 6)

message(paste("R-squared:", r_squared, "| P-value:", p_value, "| Slope:", slope))
#LINES 12-36 FOR TOPOGRAPHIC MAP AND DATA


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
    if(input$plotChoice == "Sightings by Season") {
      source("Sightings-per-season-bar.R")
      season_bar
    } else if(input$plotChoice == "Sightings by State") {
      source("sightings-per-state.R")
      state_bar
    } else if(input$plotChoice == "Sightings by Temperature") {
      source("sightings-by-temp.R")
      temp_high_bar
    }
  })
  
  output$plots_text <- renderText({
    if(!is.null(input$plots_click)) {
      if(input$plotChoice == "Sightings by Season") {
        source("Sightings-per-season-bar.R")
        keepRows <- round(input$plots_click$x) == as.numeric(bigfoot_season$season)
        rows <- bigfoot_season[keepRows,]
        return(paste("Sightings:", rows$n))
        
      } else if(input$plotChoice == "Sightings by State") {
        source("sightings-per-state.R")
        keepRows <- round(input$plots_click$x) == as.numeric(number_sightings_per_state$state)
        rows <- number_sightings_per_state[keepRows, ]
        return(paste("Sightings:", rows$n))
        
      } else if(input$plotChoice == "Sightings by Temperature") {
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
    read.csv("bigfoot_data_clean.csv", stringsAsFactors = FALSE)
  })
  
  # Convert to SF object with cleaned data
  bigfoot_sf <- reactive({
    df <- bigfoot_observations()
    
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
  
  # Load weather data
  weather_data <- reactive({
    df <- bigfoot_sf() %>% st_drop_geometry()
    return(df)
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
    data <- bigfoot_sf()
    
    # Filter by year (cumulative up to selected year)
    data <- data %>%
      filter(year <= input$year_slider)
    
    # Filter by selected state if not "All States"
    if(!is.null(input$map_state) && input$map_state != "All States") {
      data <- data %>% filter(state == input$map_state)
    }
    
    return(data)
  })
  
  # Filter weather data based on date range and selected state
  # Filter weather data based on year filters and selected state
  # Filter weather data based on year filters and selected state
  filtered_weather <- reactive({
    df <- weather_data()
    
    # Filter by year range (cumulative up to selected year) - matching the bigfoot data filter
    df <- df %>%
      filter(!is.na(year) & year <= input$year_slider)
    
    # Filter by selected state if not "All States"
    if(!is.null(input$map_state) && input$map_state != "All States") {
      df <- df %>% filter(state == input$map_state)
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
    if(input$view_mode == "heat" && nrow(bigfoot_data_clean) > 0) {
      map <- map %>%
        addHeatmap(
          data = bigfoot_data_clean,
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
      # Merge observation data - use the reactive directly to avoid join issues
      data_with_obs <- bigfoot_data_clean %>%
        mutate(marker_id = paste0("marker_", seq_len(n())))
      
      map <- map %>%
        addCircleMarkers(
          data = data_with_obs,
          lng = ~long,
          lat = ~lat,
          radius=4,
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
      # Merge observation data
      data_with_obs <- bigfoot_data_clean %>%
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
      
      # Add Wind arrows
      if("wind" %in% input$weather_layers) {
        # Filter by selected states if any are chosen
        wind_df <- weather_df
        if(!is.null(input$state_filter) && length(input$state_filter) > 0) {
          wind_df <- weather_df %>% filter(state %in% input$state_filter)
        } else {
          wind_df <- weather_df %>% filter(FALSE)
        }
        
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
  
  # Display statistics
  output$sighting_count <- renderText({
    data <- filtered_sf()
    paste("Total Sightings:", nrow(data))
  })
  
  output$year_info <- renderText({
    paste("Showing: 1900 to", input$year_slider)
  })
  
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
        plot.subtitle = element_text(size = 12, color = "#f9ca24"),
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
         cex = 1.5,
         cex.lab = 1.2,
         cex.main = 1.3)
    
    # Add regression line
    abline(lm_model, col = "#D7191C", lwd = 2)
    
    # Add state labels
    text(analysis_data$Average.Elevation, analysis_data$Sightings,
         labels = analysis_data$States,
         pos = 3,
         cex = 0.7,
         col = "#333333")
    
    # Add legend with statistics
    legend("topright",
           legend = c(
             paste("R² =", r_squared),
             paste("p-value =", p_value),
             paste("Slope =", slope)
           ),
           bty = "n",
           cex = 1.1)
    
    # Add grid
    grid(col = "gray80", lty = "dotted")
  })
  
}