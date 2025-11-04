source("moon_phase_generator.R")

bigfoot_data <- read.csv("bigfoot_data_wordcount_filtered_less_200.csv")

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
  
  # ============================================
  # WORD CLOUD
  # ============================================
  output$WordCloud <- renderPlot({
    
    removed_words <- c("like", "the", "also", "didnt", "there", "got", "this", "just", "didnt", "one")
    
    kept_words <- sapply(1:nrow(bigfoot_data),
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
  
  
<<<<<<< HEAD
  # ============================================
  # VISUALIZATIONS (Season, State, Temperature)
  # ============================================
=======
  
  # making the visualizations for sightings by season, state, and temperature and adding an interactive component
>>>>>>> 67b73a3b460f0f9feeb3654b61eff7541e72a619
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
  output$info <- renderText({
    print(input$plotChoice)
    if (is.null(input$plot_click)) {
      return("Click on a bar to see the number of observations")
    }
    if(input$plotChoice == "Sightings by Season") {
      seasontable <- brushedPoints(bigfoot_season, input$plot_click)
      paste(seasontable$n)
    }
    else if(input$plotChoice == "Sightings by State") {
      statetable <- brushedPoints(number_sightings_per_state, input$plot_click)
      paste(statetable$n)
    }
    else if(input$plotChoice == "Sightings by Temperature") {
      temptable <- brushedPoints(temp_summary, input$plot_click)
      paste(temptable$n)
    }
    })
  
  output$info <- renderText({
    print(input$plotChoice)
    if (is.null(input$plot_click)) {
      return("Click on a bar to see the number of observations")
    }
    if(input$plotChoice == "Sightings by Season") {
      seasontable <- brushedPoints(bigfoot_season, input$plot_click)
      paste(seasontable$n)
    }
    else if(input$plotChoice == "Sightings by State") {
      statetable <- brushedPoints(number_sightings_per_state, input$plot_click)
      paste(statetable$n)
    }
    else if(input$plotChoice == "Sightings by Temperature") {
      temptable <- brushedPoints(temp_summary, input$plot_click)
      paste(temptable$n)
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
      
      updateSliderInput(session, "year_range",
                        min = min_year,
                        max = max_year,
                        value = c(min_year, max_year))
      
      # Update date range slider
      dates_available <- data$date_parsed[!is.na(data$date_parsed)]
      if(length(dates_available) > 0) {
        updateSliderInput(session, "date_range",
                          min = min(dates_available, na.rm = TRUE),
                          max = max(dates_available, na.rm = TRUE),
                          value = c(min(dates_available, na.rm = TRUE), 
                                    max(dates_available, na.rm = TRUE)))
      }
      
      # Update state filter choices
      states <- sort(unique(data$state[!is.na(data$state)]))
      
      updateSelectInput(session, "map_state",
                        choices = c("All States", states),
                        selected = "All States")
      
      updateSelectInput(session, "state_filter",
                        choices = states,
                        selected = NULL)
    }
  })
  
  # Filter Bigfoot data based on inputs
  filtered_sf <- reactive({
    data <- bigfoot_sf()
    
    # Filter by year range (cumulative up to selected year)
    data <- data %>%
      filter(year >= input$year_range[1] & year <= input$year_slider)
    
    # Filter by selected state if not "All States"
    if(!is.null(input$map_state) && input$map_state != "All States") {
      data <- data %>% filter(state == input$map_state)
    }
    
    return(data)
  })
  
  # Filter weather data based on date range and selected state
  filtered_weather <- reactive({
    df <- weather_data()
    
    # Filter by date range
    df <- df %>%
      filter(!is.na(date_parsed) & date_parsed >= input$date_range[1] & date_parsed <= input$date_range[2])
    
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
          radius = 5,
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
          fillColor = "#ff6b6b",
          fillOpacity = 0.4,
          stroke = TRUE,
          weight = 1,
          opacity = 0.8,
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
    paste("Showing:", input$year_range[1], "to", input$year_slider)
  })
  
  # Timeline plot showing all data
  output$timeline_plot <- renderPlot({
    data <- bigfoot_sf() %>%
      st_drop_geometry() %>%
      filter(year >= input$year_range[1] & year <= input$year_range[2])
    
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
    
    # Create phase bins (8 major phases)
    df <- df %>%
      mutate(
        phase_bin = cut(moon_phase, 
                        breaks = seq(0, 1, by = 0.125), 
                        labels = c("New Moon", "Waxing Crescent", "First Quarter", 
                                   "Waxing Gibbous", "Full Moon", "Waning Gibbous", 
                                   "Last Quarter", "Waning Crescent"),
                        include.lowest = TRUE)
      ) %>%
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
  
  # Phase distribution bar chart
  output$phase_distribution <- renderPlot({
    bins <- phase_bins()
    
    ggplot(bins, aes(x = reorder(phase_bin, -count), y = count, fill = phase_bin)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c(
        "New Moon" = "#2c3e50",
        "Waxing Crescent" = "#95a5a6",
        "First Quarter" = "#bdc3c7",
        "Waxing Gibbous" = "#ecf0f1",
        "Full Moon" = "#f9ca24",
        "Waning Gibbous" = "#ecf0f1",
        "Last Quarter" = "#bdc3c7",
        "Waning Crescent" = "#95a5a6"
      )) +
      labs(
        title = "Bigfoot Sightings by Moon Phase",
        x = "Moon Phase",
        y = "Number of Sightings"
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#0a0e27", color = NA),
        panel.background = element_rect(fill = "#1e2742", color = NA),
        text = element_text(color = "#ffffff"),
        axis.text = element_text(color = "#ffffff"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.grid = element_line(color = "#2c3e50")
      )
  }, bg = "#0a0e27")
  
}