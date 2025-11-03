source("moon_phase_generator.R")

bigfoot_data <- read.csv("bigfoot_data_wordcount_filtered_less_200.csv")

library(wordcloud)
library(tm)
library(memoise)
library(tidyverse)
library(stringr)
library(ggplot2)
library(readxl)
bigfoot_observations <- read_excel("bigfoot_observation.xlsx")



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
      # Merge observation data
      data_with_obs <- data %>%
        left_join(bigfoot_observations %>% 
                    select(latitude, longitude, state, county, observed = observation),
                  by = c("lat" = "latitude", "long" = "longitude")) %>%
        mutate(marker_id = paste0("marker_", seq_len(n())))
      
      leafletProxy("map", data = data_with_obs) %>%
        addCircleMarkers(
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
            "<b>Date:</b> ", date, "<br>",
            "<b>State:</b> ", state, "<br>",
            "<b>County:</b> ", county, "<br>",
            "<b>Observation:</b> ", observed 
          )
        )
    } else if(input$view_mode == "circles" && nrow(data) > 0) {
      # Merge observation data
      data_with_obs <- data %>%
        left_join(bigfoot_observations %>% 
                    select(latitude, longitude, state, county, observed = observation),
                  by = c("lat" = "latitude", "long" = "longitude")) %>%
        mutate(marker_id = paste0("marker_", seq_len(n())))
      
      leafletProxy("map", data = data_with_obs) %>%
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
          layerId = ~marker_id,
          popup = ~paste0(
            "<b>Date:</b> ", date, "<br>",
            "<b>Year:</b> ", year, "<br>",
            "<b>State:</b> ", state, "<br>",
            "<b>County:</b> ", county, "<br>",
            "<b>Observation:</b> ", observed
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
  
  
  #MOON TRACKER STARTS HERE
  
  
  
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