library(tidyverse)
library(ggplot2)
library(sf)
library(tigris)


bigfoot_data_clean <- read.csv("bigfoot_data_clean.csv")

# Grouping Bigfoot sightings by State

number_sightings_per_state <- bigfoot_data_clean %>%
  count(state)

# Grouping Bear Sightings by State

bear_sightings <- read.csv("BearSightingsdata.csv") %>%
  rename(
    lat = decimalLatitude,
    lon = decimalLongitude
  )
bear_state <- bear_sightings %>%
  count(stateProvince) %>%
  rename(state = stateProvince)

# merging the data sets

merged_bigfoot_bear_state <- merge(bear_state, number_sightings_per_state, by = "state") %>%
  rename(
    bear_obs = n.x,
    bigfoot_obs = n.y
  )

# Run linear regression
bigfoot_bear_state_lm <- lm(bear_obs ~ bigfoot_obs, data = merged_bigfoot_bear_state)

# View detailed summary
summary(bigfoot_bear_state_lm)

# Making plot to visualize relationship

ggplot(merged_bigfoot_bear_state, aes(x = bear_obs, y = bigfoot_obs)) +
  geom_point(size = 3, color = "#0047AB") +  # scatter points
  geom_smooth(method = "lm", se = TRUE, color = "black") +  # regression line with confidence interval
  labs(title = "Bear vs. Bigfoot Sightings by State",
       x = "Bear Sightings",
       y = "Bigfoot Sightings") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
#--------------------
# a county level regression

counties <- counties(cb = TRUE, resolution = "20m", year = 2022)
counties <- counties %>%
  select(STATEFP, COUNTYFP, NAME, NAMELSAD, STUSPS) %>%
  st_transform(4326)

# Create sf object from bear sightings data
bear_sightings_sf <- st_as_sf(
  bear_sightings,
  coords = c("lon", "lat"),
  crs = 4326  # WGS84 - same as counties
)
# Perform spatial join to find which county each point falls in
bear_sightings_with_county <- st_join(bear_sightings_sf, counties, join = st_within) %>%
  st_drop_geometry()  # Remove the geometry column because it isn't needed

# creating bear number of sightings per county:
bear_state_county <- bear_sightings_with_county %>%
  group_by(stateProvince, NAMELSAD) %>%
  summarize(
    count = n(),
    .groups = "drop"
  ) %>%
  arrange(stateProvince, desc(count)) %>%
  rename(
    state = stateProvince,
    county = NAMELSAD
  ) %>%
  mutate(county_state = paste(county, state, sep = ", ")) %>%
  select(-county, - state)

# creating bigfoot number of sightings per county:

bigfoot_state_county <- bigfoot_data_clean %>%
  group_by(state, county) %>%
  summarize(
    count = n(),
    .groups = "drop"
  ) %>%
  arrange(state, desc(count)) %>%
  mutate(county_state = paste(county, state, sep = ", ")) %>%
  select(-county, -state)

# merging the data sets:
merged_bigfoot_bear_county <- merge(bear_state_county, bigfoot_state_county, by = "county_state") %>%
  rename(
    bear_obs = count.x,
    bigfoot_obs = count.y
  )

# running regression

bigfoot_bear_county_lm <- lm(bear_obs ~ bigfoot_obs, data = merged_bigfoot_bear_county)
summary(bigfoot_bear_county_lm)

# making graph

ggplot(merged_bigfoot_bear_county, aes(x = bear_obs, y = bigfoot_obs)) +
  geom_point(size = 3, color = "#0047AB") +  # scatter points
  geom_smooth(method = "lm", se = TRUE, color = "black") +  # regression line with confidence interval
  labs(title = "Bear vs. Bigfoot Sightings by County, State",
       x = "Bear Sightings",
       y = "Bigfoot Sightings") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
 
