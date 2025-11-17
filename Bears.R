library(tidyverse)
library(ggplot2)

# Grouping Bigfoot sightings by State

number_sightings_per_state <- bigfoot_data %>%
  count(state)

# Grouping Bear Sightings by State

bear_sightings <- read.csv("BearSightingsdata.csv")
bear_state <- bear_sightings %>%
  count(stateProvince) %>%
  rename(state = stateProvince)

# merging the data sets

merged_bigfoot_bear <- merge(bear_state, number_sightings_per_state, by = "state") %>%
  rename(
    bear_obs = n.x,
    bigfoot_obs = n.y
  )

# Run linear regression
bigfoot_bear_lm <- lm(bear_obs ~ bigfoot_obs, data = merged_bigfoot_bear)

# View detailed summary
summary(bigfoot_bear_lm)

# Making plot to visualize relationship

ggplot(merged_bigfoot_bear, aes(x = bear_obs, y = bigfoot_obs)) +
  geom_point(size = 3, color = "#0047AB") +  # scatter points
  geom_smooth(method = "lm", se = TRUE, color = "black") +  # regression line with confidence interval
  labs(title = "Bear vs Bigfoot Sightings by State",
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
