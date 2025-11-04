# Define Server
server <- function(input, output, session) {
  
  # Load and prepare data
  weather_data <- reactive({
    df <- read.csv("bigfoot_data.csv", stringsAsFactors = FALSE)
    # Convert date column
    df$date <- as.Date(df$date, format = "%m/%d/%Y")
    # Remove rows with missing coordinates
    df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
    return(df)
  })
  
  # Filter data based on date range
  filtered_data <- reactive({
    df <- weather_data()
    # Filter by date range
    df <- df %>%
      filter(!is.na(date) & date >= input$date_range[1] & date <= input$date_range[2])
    
    # Group by location and aggregate weather data
    df %>%
      group_by(latitude, longitude, county, state) %>%
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
      filter(!is.na(latitude) & !is.na(longitude))
  })
  
  # Update date slider based on actual data
  observe({
    df <- weather_data()
    if(nrow(df) > 0) {
      dates_available <- df$date[!is.na(df$date)]
      if(length(dates_available) > 0) {
        updateSliderInput(session, "date_range",
                          min = min(dates_available, na.rm = TRUE),
                          max = max(dates_available, na.rm = TRUE),
                          value = c(min(dates_available, na.rm = TRUE), 
                                    max(dates_available, na.rm = TRUE)))
      }
    }
  })
  
  # Color palettes
  temp_high_pal <- colorNumeric(palette = "YlOrRd", domain = c(0, 100))
  temp_low_pal <- colorNumeric(palette = "Blues", domain = c(0, 80))
  precip_pal <- colorNumeric(palette = "GnBu", domain = c(0, 5))
  visibility_pal <- colorNumeric(palette = "Purples", domain = c(0, 10))
  
  # Create base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -95, lat = 39, zoom = 4)
  })
  
  # Update map layers based on selections
  observe({
    df <- filtered_data()
    
    if(nrow(df) == 0) {
      leafletProxy("map") %>%
        clearMarkers() %>%
        clearControls()
      return()
    }
    
    map <- leafletProxy("map") %>%
      clearMarkers() %>%
      clearControls()
    
    # Add High Temperature layer
    if("temp_high" %in% input$layers) {
      map <- map %>%
        addCircleMarkers(
          data = df,
          lng = ~longitude,
          lat = ~latitude,
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
    if("temp_low" %in% input$layers) {
      map <- map %>%
        addCircleMarkers(
          data = df,
          lng = ~longitude,
          lat = ~latitude,
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
    if("precip" %in% input$layers) {
      map <- map %>%
        addCircleMarkers(
          data = df,
          lng = ~longitude,
          lat = ~latitude,
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
    if("wind" %in% input$layers) {
      for(i in 1:nrow(df)) {
        map <- map %>%
          addMarkers(
            lng = df$longitude[i],
            lat = df$latitude[i],
            icon = makeIcon(
              iconUrl = sprintf("data:image/svg+xml,%%3Csvg xmlns='http://www.w3.org/2000/svg' width='30' height='30' viewBox='0 0 24 24'%%3E%%3Cpath fill='%%23FF6B6B' d='M12 2L6 12h12L12 2z' transform='rotate(%f 12 12)'%%3E%%3C/path%%3E%%3C/svg%%3E", 
                                df$wind_bearing_avg[i]),
              iconWidth = 30, iconHeight = 30
            ),
            popup = paste0("<b>", df$county[i], ", ", df$state[i], "</b><br>",
                           "Wind Speed: ", round(df$wind_speed_avg[i], 1), " mph<br>",
                           "Direction: ", round(df$wind_bearing_avg[i], 0), "°<br>",
                           "Observations: ", df$n_observations[i]),
            group = "Wind"
          )
      }
    }
    
    # Add Visibility layer
    if("visibility" %in% input$layers) {
      map <- map %>%
        addCircleMarkers(
          data = df,
          lng = ~longitude,
          lat = ~latitude,
          radius = ~visibility_avg * 2,
          color = ~visibility_pal(visibility_avg),
          fillColor = ~visibility_pal(visibility_avg),
          fillOpacity = 0.5,
          stroke = TRUE,
          weight = 1,
          popup = ~paste0("<b>", county, ", ", state, "</b><br>",
                          "Visibility: ", round(visibility_avg, 1), " mi<br>",
                          "Observations: ", n_observations),
          group = "Visibility"
        )
    }
    
    map
  })
  
  # Legend
  output$legend_info <- renderUI({
    HTML("
      <style>
        .legend-item { margin: 8px 0; }
        .legend-color { display: inline-block; width: 20px; height: 20px; margin-right: 8px; vertical-align: middle; }
      </style>
      <div class='legend-item'>
        <span class='legend-color' style='background: linear-gradient(to right, #FFFFB2, #FD8D3C, #BD0026);'></span>
        High Temp (°F)
      </div>
      <div class='legend-item'>
        <span class='legend-color' style='background: linear-gradient(to right, #EFF3FF, #6BAED6, #08519C);'></span>
        Low Temp (°F)
      </div>
      <div class='legend-item'>
        <span class='legend-color' style='background: linear-gradient(to right, #F0F9E8, #7BCCC4, #0868AC);'></span>
        Precipitation (in)
      </div>
      <div class='legend-item'>
        <span style='color: #FF6B6B; font-size: 20px; margin-right: 8px;'>▲</span>
        Wind Direction
      </div>
      <div class='legend-item'>
        <span class='legend-color' style='background: linear-gradient(to right, #FCFBFD, #9E9AC8, #54278F);'></span>
        Visibility (mi)
      </div>
    ")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)