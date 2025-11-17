library(dplyr)
library(ggplot2)

# Function to generate precipitation correlation
generate_precip_correlation <- function(data_path = "bigfoot_data_clean.csv") {
  bigfoot_data_clean <- read.csv(data_path, stringsAsFactors = FALSE)
  
  precip_summary <- bigfoot_data_clean %>%
    group_by(precip_intensity) %>%
    summarise(sightings = n())
  
  lm_precip <- lm(sightings ~ precip_intensity, data = precip_summary)
  
  list(
    data = precip_summary,
    model = lm_precip,
    r_squared = summary(lm_precip)$r.squared,
    slope = summary(lm_precip)$coefficients[2, 1],
    p_value = summary(lm_precip)$coefficients[2, 4]
  )
}

# Function to generate visibility correlation
generate_visibility_correlation <- function(data_path = "bigfoot_data_clean.csv") {
  bigfoot_data_clean <- read.csv(data_path, stringsAsFactors = FALSE)
  
  vis_summary <- bigfoot_data_clean %>%
    group_by(visibility) %>%
    summarise(sightings = n())
  
  lm_vis <- lm(sightings ~ visibility, data = vis_summary)
  
  list(
    data = vis_summary,
    model = lm_vis,
    r_squared = summary(lm_vis)$r.squared,
    slope = summary(lm_vis)$coefficients[2, 1],
    p_value = summary(lm_vis)$coefficients[2, 4]
  )
}

# Function to generate cloud cover correlation
generate_cloud_correlation <- function(data_path = "bigfoot_data_clean.csv") {
  bigfoot_data_clean <- read.csv(data_path, stringsAsFactors = FALSE)
  
  cloud_summary <- bigfoot_data_clean %>%
    group_by(cloud_cover) %>%
    summarise(sightings = n())
  
  lm_cloud <- lm(sightings ~ cloud_cover, data = cloud_summary)
  
  list(
    data = cloud_summary,
    model = lm_cloud,
    r_squared = summary(lm_cloud)$r.squared,
    slope = summary(lm_cloud)$coefficients[2, 1],
    p_value = summary(lm_cloud)$coefficients[2, 4]
  )
}

# Function to generate weather conditions summary
generate_conditions_summary <- function(data_path = "bigfoot_data_clean.csv") {
  bigfoot_data_clean <- read.csv(data_path, stringsAsFactors = FALSE)
  
  weather_summary <- bigfoot_data_clean %>%
    filter(!is.na(conditions)) %>%
    group_by(conditions) %>%
    summarise(sightings = n()) %>%
    arrange(desc(sightings))
  
  list(
    data = weather_summary,
    total_conditions = nrow(weather_summary),
    top_condition = weather_summary$conditions[1],
    top_sightings = weather_summary$sightings[1]
  )
}

# Function to plot precipitation correlation
plot_precip_correlation <- function(precip_data) {
  ggplot(precip_data$data, aes(x = precip_intensity, y = sightings)) +
    geom_point(size = 3, color = "#0047AB") +
    geom_smooth(method = "lm", se = TRUE, color = "black") +
    labs(
      title = paste("Precipitation vs Sightings\nR² =", 
                    round(precip_data$r_squared, 3)),
      x = "Precipitation Intensity", 
      y = "Number of Sightings"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    )
}

# Function to plot visibility correlation
plot_visibility_correlation <- function(vis_data) {
  ggplot(vis_data$data, aes(x = visibility, y = sightings)) +
    geom_point(size = 3, color = "#0047AB") +
    geom_smooth(method = "lm", se = TRUE, color = "black") +
    labs(
      title = paste("Visibility vs Sightings\nR² =", 
                    round(vis_data$r_squared, 3)),
      x = "Visibility", 
      y = "Number of Sightings"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    )
}

# Function to plot cloud cover correlation
plot_cloud_correlation <- function(cloud_data) {
  ggplot(cloud_data$data, aes(x = cloud_cover, y = sightings)) +
    geom_point(size = 3, color = "#0047AB") +
    geom_smooth(method = "lm", se = TRUE, color = "black") +
    labs(
      title = paste("Cloud Cover vs. Sightings\nR² =",
                    round(cloud_data$r_squared, 3)),
      x = "Cloud Cover", 
      y = "Number of Sightings"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    )
}

# Function to plot weather conditions bar chart
plot_conditions_summary <- function(conditions_data) {
  ggplot(conditions_data$data, aes(x = reorder(conditions, -sightings), y = sightings)) +
    geom_bar(stat = "identity", fill = "#0047AB") +
    labs(
      title = "Sightings by Weather Condition",
      x = "Weather Condition", 
      y = "Number of Sightings"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      plot.title = element_text(size = 16, hjust = 0.5)
    )
}