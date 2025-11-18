# Function to create weather correlation plots
create_weather_correlation_plot <- function(data, x_var, title, x_label, slope_digits = 2) {
  
  # Generate summary by grouping
  summary_data <- data %>%
    group_by(.data[[x_var]]) %>%
    summarise(sightings = n(), .groups = "drop")
  
  # Calculate linear regression model
  formula_str <- paste("sightings ~", x_var)
  lm_model <- lm(as.formula(formula_str), data = summary_data)
  r_squared <- summary(lm_model)$r.squared
  p_value <- summary(lm_model)$coefficients[2, 4]
  slope <- coef(lm_model)[2]
  
  # Create the plot
  ggplot(summary_data, aes(x = .data[[x_var]], y = sightings)) +
    geom_point(size = 5, color = "#0047AB", alpha = 0.8) +
    geom_smooth(method = "lm", se = TRUE, color = "#f9ca24", 
                fill = "#f9ca24", alpha = 0.2, linewidth = 1.5) +
    labs(
      title = title,
      subtitle = paste0(
        "R² = ", round(r_squared, 3), 
        " | Slope = ", round(slope, slope_digits),
        " | p-value = ", format.pval(p_value, digits = 3)
      ),
      x = x_label,
      y = "Number of Sightings"
    ) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#0a0e27", color = NA),
      panel.background = element_rect(fill = "#1e2742", color = NA),
      text = element_text(color = "#ffffff", size = 12),
      plot.title = element_text(size = 16, face = "bold", color = "#ffffff"),
      plot.subtitle = element_text(size = 14, color = "#f9ca24"),
      axis.text = element_text(color = "#ffffff"),
      axis.title = element_text(size = 13, face = "bold", color = "#ffffff"),
      panel.grid.major = element_line(color = "#2c3e50", linewidth = 0.5),
      panel.grid.minor = element_line(color = "#2c3e50", linewidth = 0.25)
    )
}

# Weather variable configuration lookup table
weather_config <- list(
  precip = list(
    var = "precip_intensity",
    title = "Precipitation Intensity vs Bigfoot Sightings",
    x_label = "Precipitation Intensity",
    slope_digits = 2
  ),
  visibility = list(
    var = "visibility",
    title = "Visibility vs Bigfoot Sightings",
    x_label = "Visibility (miles)",
    slope_digits = 2
  ),
  wind = list(
    var = "wind_speed",
    title = "Wind Speed vs Bigfoot Sightings",
    x_label = "Wind Speed (mph)",
    slope_digits = 2
  ),
  temp_low = list(
    var = "temperature_low",
    title = "Low Temperature vs Bigfoot Sightings",
    x_label = "Low Temperature (°F)",
    slope_digits = 2
  ),
  temp_high = list(
    var = "temperature_high",
    title = "High Temperature vs Bigfoot Sightings",
    x_label = "High Temperature (°F)",
    slope_digits = 2
  )
)