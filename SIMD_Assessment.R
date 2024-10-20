## Script to assess bus accessibility in Scotland against
## SIMD deprivation quintiles in SPT and GCC areas
# Note; the quintiles are based on the national dataset
# Load the necessary libraries
library(tidyverse)
library(sf)
library(mapview)
library(classInt)

# Set working directory
setwd("~/Google Drive/My Drive/Consulting/Scotland_Bus_Access")
# Read data from shapefile created in Scot_Bus_access.R
Scot_buses <- read_csv("Data/Scot_Bus_Access.csv")
# Read SIMD shapefile downloaded from Scottish Government website
SIMD <- read_sf("Data/SIMD/SG_SIMD_2020/SG_SIMD_2020.shp") %>%
  as.data.frame() %>% 
  select(DataZone, Quintilev2) 
# Join datasets
Bus_SIMD <- left_join(Scot_buses, SIMD, by = c("DataZone" = "DataZone"))

## SPT Area Analysis
# Calculate total count of datazones by SIMD quintile
total_datazones_SPT <- Bus_SIMD %>%
  filter(SPT == "Y") %>%
  group_by(Quintilev2) %>%
  summarise(TotalCount = n()) %>%
  as.data.frame()
# Filter for negative changes in SPT area
negative_changes_SPT <- Bus_SIMD %>% 
  filter(SPT=="Y") %>%
  filter(Change < -1) # Less than -1 to remove 'noise' of small changes
# Count negative changes by SIMD quintile
negative_changes_SPT_count <- negative_changes_SPT %>%
  group_by(Quintilev2) %>%
  summarise(Count_negative = n())
# Join the total count with the negative changes count
negative_changes_SPT_count <- left_join(negative_changes_SPT_count, total_datazones_SPT, by = "Quintilev2") %>%
  mutate(Percent = round(Count_negative/TotalCount*100),0)

# Create bar chart for SPT area
ggplot(negative_changes_SPT_count, aes(x = Quintilev2, y = Count_negative)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(Count_negative, " (", Percent, "%)")), 
            vjust = -0.5, size = 5) +  # Add count and percentage labels
  labs(title = "Reduced Bus Accessibility by SIMD Quintile \n in SPT Area",
       x = "SIMD Quintile",
       y = "Number of Datazones with Reduced Bus Service") +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 28,          # Change the title text size
      face = "bold",      # Make the title bold
      hjust = 0.5,        # Center the title horizontally
      vjust = 1           # Adjust the vertical position
    ),
       axis.title.x = element_text(
      size = 20           # Change the x-axis title text size
    ),
    axis.title.y = element_text(
      size = 20           # Change the y-axis title text size
    ),
    axis.text.x = element_text(
      size = 20           # Change the x-axis text size
    ),
    axis.text.y = element_text(
      size = 20           # Change the y-axis text size
    )
  )

ggsave("data/Bus_Access_SIMD_SPT.jpeg", width = 10, height = 10, dpi = 300)

## GCC Area analysis
# Calculate total count of datazones by SIMD quintile
total_datazones_GCC <- Bus_SIMD %>%
  filter(GCC == "Y") %>%
  group_by(Quintilev2) %>%
  summarise(TotalCount = n()) %>%
  as.data.frame()
# Filter for negative changes in GCC area
negative_changes_GCC <- Bus_SIMD %>% 
  filter(GCC=="Y") %>%
  filter(Change < -1)
# Count negative changes by SIMD quintile
negative_changes_GCC_count <- negative_changes_GCC %>%
  group_by(Quintilev2) %>%
  summarise(Count_negative = n())

  # Join the total count with the negative changes count
negative_changes_GCC_count <- left_join(negative_changes_GCC_count, total_datazones_GCC, by = "Quintilev2") %>%
  mutate(Percent = round(Count_negative/TotalCount*100),0)

# Create bar chart for GCC area
ggplot(negative_changes_GCC_count, aes(x = Quintilev2, y = Count_negative)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(Count_negative, " (", Percent, "%)")), 
          vjust = -0.5, size = 5) +  # Add count and percentage labels
  labs(title = "Reduced Bus Accessibility by SIMD Quintile \n in GCC Area",
       x = "SIMD Quintile",
       y = "Number of Datazones with Reduced Bus Service") +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 28,          # Change the title text size
      face = "bold",      # Make the title bold
      hjust = 0.5,        # Center the title horizontally
      vjust = 1           # Adjust the vertical position
    ),
       axis.title.x = element_text(
      size = 20           # Change the x-axis title text size
    ),
    axis.title.y = element_text(
      size = 20           # Change the y-axis title text size
    ),
    axis.text.x = element_text(
      size = 20           # Change the x-axis text size
    ),
    axis.text.y = element_text(
      size = 20           # Change the y-axis text size
    ),
    
  )

ggsave("data/Bus_Access_SIMD_GCC.jpeg", width = 10, height = 10, dpi = 300)