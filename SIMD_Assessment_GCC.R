## Script to assess bus accessibility in Glasgow City Council area
## against SIMD deprivation data by local quintiles

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
  select(DataZone, Rankv2) 
# Join datasets
Bus_SIMD <- left_join(Scot_buses, SIMD, by = c("DataZone" = "DataZone"))

#
# GCC Area Analysis
# Calculate quintile rank of datazones by SIMD rank
GCC_Datazones <- Bus_SIMD %>%
  filter(GCC == "Y")%>%
  mutate(Quintile = ntile(Rankv2, 5)) %>%
  as.data.frame()

# Calculate the number of datazones in each quintile
GCC_Quintile <- GCC_Datazones %>%
  group_by(Quintile) %>%
  summarise(Count = n()) %>%
  as.data.frame()
# Good.

# Filter for negative changes in GCC area
negative_changes_GCC_local <- GCC_Datazones %>% 
  filter(Change < -1) # Less than -1 to remove 'noise' of small changes
# Count negative changes by SIMD quintile
negative_changes_GCC_local_count <- negative_changes_GCC_local %>%
  group_by(Quintile) %>%
  summarise(Count_negative = n())
# Join the total count with the negative changes count
negative_changes_GCC_local_count <- left_join(negative_changes_GCC_local_count, GCC_Quintile, by = "Quintile") %>%
  mutate(Percent = round(Count_negative/Count*100),0)

# Plot Bus Accessibility Change in Buckets of SIMD Quintiles
Access_SIMD_GCC_Buckets <- ggplot(negative_changes_GCC_local_count, aes(x = Quintile, y = Count_negative)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(Count_negative, " (", Percent, "%)")), 
            vjust = -0.5, size = 5) +  # Add count and percentage labels
  labs(title = "Reduced Bus Accessibility by SIMD \n Local Quintiles in GCC Area",
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

# Save the plot
ggsave("data/Bus_Access_SIMD_GCC_local.jpeg", plot = Access_SIMD_GCC_Buckets, width = 10, height = 10, dpi = 300)
