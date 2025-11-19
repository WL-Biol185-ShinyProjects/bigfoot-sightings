library(tidyverse)
library(ggplot2)

bigfoot_data <- read.csv("bigfoot_data_clean.csv")

bigfoot_season <- bigfoot_data %>%
  count(season) %>%
  filter(season != "Unknown")  # Remove "Unknown" season

bigfoot_season$season <- factor(bigfoot_season$season)

season_bar <- ggplot(bigfoot_season, aes(season, n)) + 
  geom_col(fill = "#0047AB") + 
  labs(title = "Number of Bigfoot Sightings Per Season") + 
  xlab("Season") + 
  ylab("Sightings") +
  theme(
    plot.title = element_text(size = 20),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    panel.background = element_rect(fill = "#F0FFFF"), 
    plot.background = element_rect(fill = "#F0FFFF")
  )
