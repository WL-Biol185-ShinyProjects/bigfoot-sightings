# make a graphical representation of the average high temp, med temp, and low temp associated with number of sightings
# condense data set into necessary categories
library(ggplot2)
library(shiny)
library(dplyr)

# use cut() to create the bins
bigfoot_data <- bigfoot_data %>%
  
  mutate(high_temp_bin = cut(temperature_high,
                             breaks = seq(0, 100, by = 20),
                             include.lowest = TRUE,
                             right = FALSE))

# count sightings per temperature bins
high_temp_summary <- bigfoot_data %>%
  
  filter(!is.na(high_temp_bin))   %>%
  
  count(high_temp_bin)

# create factor for interaction
high_temp_summary$high_temp_bin <- factor(high_temp_summary$high_temp_bin)

# create bar plot
temp_high_bar <- ggplot(high_temp_summary, aes(x = high_temp_bin, y = n))           +
  
  geom_col(fill = "#0047AB", color = "black")                      +
  
  labs(title =     "Bigfoot Sightings by High Temperature Range",
       x     =     "Temperature Range (°F)",
       y     =     "Number of Sightings")                          +
  
  theme_minimal()                                                  +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_rect(fill = "#F0FFFF"), 
        plot.background = element_rect(fill = "#F0FFFF"))

# or create histogram
ggplot(bigfoot_data, aes(x = temperature_high))                      +
  
  geom_histogram(binwidth = 20, fill = "#0047AB", color = "black")   +
  
labs(title = "Bigfoot Sightings by High Temperature Range",
     x = "Temperature (°F)",
     y = "Number of Sightings")                                      +
  
  theme_minimal()                                                    +
  theme( panel.background = element_rect(fill = "#F0FFFF"), 
         plot.background = element_rect(fill  = "#F0FFFF"))





# COMBINING all high, mid, and low temps into ONE graphical representation
library(ggplot2)
library(dplyr)
library(tidyr)

# reshape data to proper format
bigfoot_sightings_by_temperature <- bigfoot_data             %>%
  
  select(temperature_high, temperature_mid, temperature_low) %>%
  
  pivot_longer(cols      = everything(),
               names_to  = "temp_type",
               values_to = "temperature")                    %>%
  
  filter(!is.na(temperature))

# create bind for each temperature reading
bigfoot_sightings_by_temperature <- bigfoot_sightings_by_temperature %>%
  
  mutate(temp_bin = cut(temperature,
                        breaks             = seq(0, 100, by = 20),
                        include.lowest     = TRUE,
                        right              = FALSE))

# count sightings per bin and temperature type
temp_summary <- bigfoot_sightings_by_temperature %>%
  
  filter(!is.na(temp_bin))                       %>%
  
  count(temp_bin, temp_type)

# create grouped bar plot
tempgrouped_bar <- ggplot(temp_summary, aes(x = temp_bin, y = n, fill = temp_type))    +
  
  geom_col(position = "dodge")                                      +
  
  scale_fill_manual(values = c("temperature_high" =   "#0041c2",
                               "temperature_mid"  =   "#6495ed",
                               "temperature_low"  =   "#4169e1"),
                    labels = c("High", "Mid", "Low"))               +
  
  labs(title   =    "Bigfoot Sightings by Temperature Range",
       x       =    "Temperature Range (°F)"                ,
       y       =    "Number of Sightings"                   ,
       fill    =    "Temperature Type")                             +
  
  theme_minimal()                                                   +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        panel.background = element_rect(fill = "#F0FFFF"), 
        plot.background = element_rect(fill = "#F0FFFF"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# or do a histogram
ggplot(bigfoot_sightings_by_temperature, aes(x = temperature, fill = temp_type))   +
  
  geom_histogram(binwidth = 20, alpha = 0.5, position = "identity")                +
  
  scale_fill_manual(values    =    c("temperature_high"     =   "#0041c2" ,
                                     "temperature_mid"      =   "#6495ed" ,
                                     "temperature_low"      =   "#4169e1"),
                    labels    =    c("High", "Mid", "Low"))                        +
  
  labs(title    =   "Bigfoot Sightings by Temperature",
       x        =   "Temperature (°F)"                ,
       y        =   "Number of Sightings"             ,
       fill     =   "Temperature Type")                                            +
  
  theme_minimal()                                                                  +
  theme(panel.background = element_rect(fill = "#F0FFFF"), 
        plot.background = element_rect(fill = "#F0FFFF"))


# or use facets
ggplot(temp_summary, aes(x = temp_bin, y = n, fill = temp_type))                   +
  
  geom_col()                                                                       +
  
  facet_wrap(~ temp_type, ncol = 1)                                                +
  
  labs(title = "Bigfoot Sightings by Temperature Range"  ,
       x = "Temperature Range (°F)"                      ,
       y = "Number of Sightings")                                                  +
  
  theme_minimal()                                                                  +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")



