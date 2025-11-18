bigfoot_data <- read.csv("bigfoot_data.csv")

source("moon_phase_generator.R")


# ============================================
# For Loading data for correlation graphs on correlation tab
# ============================================

source("plot_generator_for_correlation_tab.R")

# Load the data
tree_data <- read.csv("tree_data.csv")
bigfoot_data_clean <- read.csv("bigfoot_data_clean.csv")

# Count bigfoot sightings per state
bigfoot_counts <- as.data.frame(table(bigfoot_data_clean$state))
names(bigfoot_counts) <- c("State", "Sightings")

# Merge the datasets
merged_data <- merge(tree_data, bigfoot_counts, by = "State")

# Run linear models
model1 <- lm(Sightings ~ Forest.Coverage.Percent, data = merged_data)
model2 <- lm(Sightings ~ Forest.Land.Area, data = merged_data)
model3 <- lm(Sightings ~ Census.Land.Area, data = merged_data)

# Get statistics for model 1
r_squared1 <- summary(model1)$r.squared
p_value1 <- summary(model1)$coefficients[2, 4]

# Get statistics for model 2
r_squared2 <- summary(model2)$r.squared
p_value2 <- summary(model2)$coefficients[2, 4]

r_squared3 <- summary(model3)$r.squared
p_value3 <- summary(model3)$coefficients[2, 4]

# Print summaries
print("Model 1: Sightings ~ Forest Coverage Percent")
print(summary(model1))
print("\nModel 2: Sightings ~ Forest Land Area")
print(summary(model2))
print("\nModel 3: Sightings ~ Census Land Area")
print(summary(model3))

# ============================================
# For Loading data for correlation graphs on topo graphic map tab
# ============================================

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
# ============================================
# for loading plots for weather map
# ============================================

source("plot_generator_for_weather_correlations.R")
