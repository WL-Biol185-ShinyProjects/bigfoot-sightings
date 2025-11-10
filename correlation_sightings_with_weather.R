library(ggplot2)
library(dplyr)
library(gridExtra)

bigfoot_data_clean <- read.csv("bigfoot_data_clean.csv")

# 1. cloud cover vs. sightings

cloud_summary <- bigfoot_data_clean %>%
  group_by(cloud_cover) %>%
  summarise(sightings = n())

lm_cloud <- lm(sightings ~ cloud_cover, data = cloud_summary)

cloud_cover_plot <- ggplot(cloud_summary, aes(x = cloud_cover, y = sightings)) +
  geom_point(size = 3, color = "#0047AB")                                      +
  geom_smooth(method = "lm", se = TRUE, color = "black")                       +
  labs(title = paste("Cloud Cover vs. Sightings\nR² =",
                     round(summary(lm_cloud)$r.squared, 3)),
                     x = "Cloud Cover", y = "Number of Sightings")             +
         theme_minimal() 



# 2. Precipitation vs Sightings
precip_summary <- bigfoot_data_clean %>%
  group_by(precip_intensity) %>%
  summarise(sightings = n())

lm_precip <- lm(sightings ~ precip_intensity, data = precip_summary)

precip_intensity_plot <- ggplot(precip_summary, aes(x = precip_intensity, y = sightings)) +
  geom_point(size = 3, color = "#0047AB")                                                 +
  geom_smooth(method = "lm", se = TRUE, color = "black")                                  +
  labs(title = paste("Precipitation vs Sightings\nR² =", 
                     round(summary(lm_precip)$r.squared, 3)),
       x = "Precipitation Intensity", y = "Number of Sightings")                          +
  theme_minimal()



# 3. Visibility vs Sightings
vis_summary <- bigfoot_data_clean %>%
  group_by(visibility) %>%
  summarise(sightings = n())

lm_vis <- lm(sightings ~ visibility, data = vis_summary)

visibility_plot <- ggplot(vis_summary, aes(x = visibility, y = sightings))        +
  geom_point(size = 3, color = "#0047AB")                                         +
  geom_smooth(method = "lm", se = TRUE, color = "black")                          +
  labs(title = paste("Visibility vs Sightings\nR² =", 
                     round(summary(lm_vis)$r.squared, 3)),
       x = "Visibility", y = "Number of Sightings")                               +
  theme_minimal()


# 4. Weather Condition (categorical) - bar chart
weather_summary <- bigfoot_data_clean %>%
  filter(!is.na(conditions)) %>%
  group_by(conditions) %>%
  summarise(sightings = n()) %>%
  arrange(desc(sightings))

weather_conditions_plot <- ggplot(weather_summary, aes(x = reorder(conditions, -sightings), 
                                  y = sightings))                                              +
  geom_bar(stat = "identity", fill = "#0047AB")                                                +
  labs(title = "Sightings by Weather Condition",
       x = "Weather Condition", y = "Number of Sightings")                                     +
  theme_minimal()                                                                              +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Display all plots
grid.arrange(cloud_cover_plot, precip_intensity_plot, visibility_plot, weather_conditions_plot, ncol = 2)

# Print statistical summaries
cat("\n=== Cloud Cover Model ===\n")
print(summary(lm_cloud))

cat("\n=== Precipitation Model ===\n")
print(summary(lm_precip))

cat("\n=== Visibility Model ===\n")
print(summary(lm_vis))

# Multiple regression requires aggregating by all variables together
combined_summary <- bigfoot_data_clean %>%
  group_by(cloud_cover, precip_intensity, visibility) %>%
  summarise(sightings = n(), .groups = 'drop')

lm_full <- lm(sightings ~ cloud_cover + precip_intensity + visibility, 
              data = combined_summary)
cat("\n=== Full Model (All Variables) ===\n")
print(summary(lm_full))



