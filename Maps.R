# 1️⃣ Install required packages (run once)
install.packages(c("leaflet", "readxl", "dplyr"))

# 2️⃣ Load packages
library(leaflet)
library(readxl)
library(dplyr)

# 3️⃣ Read in your Excel data
air_data <- read_excel("AQI_clean_2021.xlsx")

# 4️⃣ Rename columns for easier reference
air_data <- air_data %>%
  rename(
    state = `State Name`,
    date = Date,
    AQI = AQI,
    category = Category,
    parameter = `Defining Parameter`,
    lat = Latitude,
    lon = Longitude,
    county = `County Name`
  )

leaflet(data = air_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    ~lon, ~lat,
    radius = 5,
    color = ~case_when(
      AQI <= 50 ~ "green",
      AQI <= 100 ~ "yellow",
      AQI <= 150 ~ "orange",
      AQI <= 200 ~ "red",
      TRUE ~ "purple"
    ),
    stroke = FALSE,
    fillOpacity = 0.7,
    label = ~paste0(
      county, ", ", state, "\n",
      "AQI: ", AQI
    ),
    labelOptions = labelOptions(direction = "auto", style = list("font-size" = "12px"))
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("green", "yellow", "orange", "red", "purple"),
    labels = c("Good (0–50)", "Moderate (51–100)", "Unhealthy for Sensitive Groups (101–150)",
               "Unhealthy (151–200)", "Very Unhealthy (200+)"),
    title = "Air Quality Index"
  )
