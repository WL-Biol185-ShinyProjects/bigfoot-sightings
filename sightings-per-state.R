number_sightings_per_state <- bigfoot_data %>%
  count(state)

number_sightings_per_state$state <- factor(number_sightings_per_state$state)

state_bar <- ggplot(number_sightings_per_state,
                    aes(state, n)) +
  geom_col(fill = "#0047AB") + 
  labs(title = "Number of Bigfoot Sightings Per State")  + 
  xlab("State") + ylab("Sightings") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.background =      
          element_rect(fill = "#F0FFFF"), plot.background = element_rect(fill = "#F0FFFF"), axis.ticks = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())