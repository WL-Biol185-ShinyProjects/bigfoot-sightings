# Script to extract time of day from observation text
# Run this once to add time_of_day column to your dataset

library(dplyr)
library(stringr)

# Read your data
bigfoot_data <- read.csv("bigfoot_data_clean.csv", stringsAsFactors = FALSE)

# Function to extract time of day from text
extract_time_of_day <- function(text) {
  if(is.na(text) || text == "") return("unknown")
  
  text_lower <- tolower(text)
  
  # Dawn patterns (5am-7am)
  dawn_patterns <- c("dawn", "daybreak", "sunrise", "early morning", 
                     "first light", "crack of dawn", "sun was coming up", 
                     "sun came up", "sun was rising")
  
  # Morning patterns (7am-12pm)
  morning_patterns <- c("morning", "forenoon", "mid-morning", "late morning",
                        "before noon", "a\\.m\\.", " am ", "^am ")
  
  # Afternoon patterns (12pm-5pm)
  afternoon_patterns <- c("afternoon", "mid-day", "midday", "lunch time",
                          "early afternoon", "late afternoon", "p\\.m\\.", " pm ")
  
  # Dusk patterns (5pm-7pm)
  dusk_patterns <- c("dusk", "twilight", "sunset", "sun was setting", 
                     "sun set", "getting dark", "almost dark", "sundown",
                     "sun was going down", "evening", "early evening")
  
  # Night patterns (7pm-5am)
  night_patterns <- c("night", "midnight", "dark", "after dark", 
                      "late night", "night time", "nighttime",
                      "\\d+:\\d+ p\\.m\\.", "10 pm", "11 pm", "12 pm",
                      "1 am", "2 am", "3 am", "4 am")
  
  # Check patterns in order of specificity
  # Dawn first (most specific)
  if(any(str_detect(text_lower, dawn_patterns))) {
    return("dawn")
  }
  
  # Dusk (also specific)
  if(any(str_detect(text_lower, dusk_patterns))) {
    return("dusk")
  }
  
  # Night (check before afternoon due to "p.m." overlap)
  if(any(str_detect(text_lower, night_patterns))) {
    return("night")
  }
  
  # Morning
  if(any(str_detect(text_lower, morning_patterns))) {
    return("morning")
  }
  
  # Afternoon
  if(any(str_detect(text_lower, afternoon_patterns))) {
    return("afternoon")
  }
  
  # Default to unknown
  return("unknown")
}

# Apply function to observed column
bigfoot_data$time_of_day <- sapply(bigfoot_data$observed, extract_time_of_day)

# Show summary of extracted times
print("Time of Day Distribution:")
print(table(bigfoot_data$time_of_day))

# Show some examples
print("\nExamples of extracted times:")
sample_indices <- sample(which(bigfoot_data$time_of_day != "unknown"), 
                         min(10, sum(bigfoot_data$time_of_day != "unknown")))
print(bigfoot_data[sample_indices, c("observed", "time_of_day")])

# Save updated dataset
write.csv(bigfoot_data, "bigfoot_data_clean_with_time.csv", row.names = FALSE)

cat("\nDataset saved as 'bigfoot_data_clean_with_time.csv'\n")
cat("Total rows:", nrow(bigfoot_data), "\n")
cat("Rows with time extracted:", sum(bigfoot_data$time_of_day != "unknown"), "\n")
cat("Rows without time (unknown):", sum(bigfoot_data$time_of_day == "unknown"), "\n")
