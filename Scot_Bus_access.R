# Script to analyse the change in bus accessibility ratings in the Strathclyde area
# Setup -------------------------------------------------------------------

# Load the necessary libraries
library(tidyverse)
library(sf)
library(mapview)
library(classInt)

# Set working directory
setwd("~/Google Drive/My Drive/Consulting/Scotland_Bus_Access")

# Load the accessibility data
access_ratings <- read_csv("Data/bus-accessibility.csv",
                           skip = 7) %>%
 rename(code = "http://purl.org/linked-data/sdmx/2009/dimension#refArea",
        name = "Reference Area",
        Rating_2017 = "2017",
        Rating_2019 = "2019") %>%
 mutate(datazone = substr(code, 53, 69),
        Change = Rating_2019 - Rating_2017) %>%
  select(-code)

# Load the datazone geography
datazone_geog <- read_sf("Data/Datazones/SG_DataZone_Bdry_2011.shp") %>%
  st_transform(4326) %>%
  select(DataZone, geometry, TotPop2011)

# Merge data
data <- left_join(datazone_geog, access_ratings, by = c("DataZone" = "datazone")) %>%
  st_make_valid()

# Load the local authority boundaries
boundaries <- read_sf("Data/Counties_and_Unitary_Authorities_December_2023_Boundaries_UK_BFC/CTYUA_DEC_2023_UK_BFC.shp") %>%
  st_transform(4326)

# Filter the boundaries to SPT jurisdiction
# note; SPT jurisdiction is only the Helensburgh and Lomond area
# of Argyll and Bute, and that local authority is enormous and includes
# many islands, so is excluded from the analysis here. 
spt_boundaries <- boundaries %>%
  filter(CTYUA23NM %in% c("East Dunbartonshire", 
                          "East Ayrshire", 
                          "East Renfrewshire", 
                          "Glasgow City", 
                          "Inverclyde", 
                          "North Ayrshire", 
                          "North Lanarkshire", 
                          "Renfrewshire", 
                          "South Ayrshire", 
                          "South Lanarkshire", 
                          "West Dunbartonshire")) 
                        #  "Argyll and Bute")) 
boundaries_GCC <- boundaries %>%
  filter(CTYUA23NM == "Glasgow City")
# merge counties together into SPT area
spt_area <- st_union(spt_boundaries) %>%
  st_transform(4326) %>%
  st_as_sf() %>%
  st_make_valid() %>%
  st_buffer(50)

# Filter the datazones to the SPT area
datazones_spt <- st_intersection(data, spt_area) %>%
  mutate("SPT" = "Y")
datazones_GCC <- st_intersection(data, boundaries_GCC) %>%
  mutate("GCC" = "Y")

Scot_Bus_Access <- left_join(data, (datazones_spt %>% as.data.frame() %>% select(DataZone, SPT)), by = "DataZone") %>%
  left_join((datazones_GCC %>% as.data.frame() %>% select(DataZone, GCC)), by = "DataZone") %>%
  replace_na(list(SPT = "N", GCC = "N"))
  
# Calculate the change in accessibility ratings
reduction_pc = sum(datazones_spt$Change)/sum(datazones_spt$Rating_2019) *100
reduced_pc <- nrow(filter(datazones_spt, Change < -1)) / nrow(datazones_spt) * 100
count(filter(datazones_spt, Change > -1))
count(filter(datazones_spt, Change == -1))

reduction_pc_GCC = sum(datazones_GCC$Change)/sum(datazones_GCC$Rating_2019) *100
reduced_pc_GCC <- nrow(filter(datazones_GCC, Change < -1)) / nrow(datazones_GCC) * 100
count(filter(datazones_GCC, Change > -1))
count(filter(datazones_GCC, Change == -1))

# calculate reduced accessibility by population in SPT
average_spt_rating <- mean(datazones_spt$Rating_2019)

reduced_access_pop <- datazones_spt %>% filter(Change < -1) %>%  
  summarise(TotalPopulation = sum(TotPop2011, na.rm = TRUE))
total_pop <- sum(datazones_spt$TotPop2011, na.rm = TRUE)
reduced_access_pc <- reduced_access_pop$TotalPopulation / total_pop * 100

# calculate reduced accessibility by population in GCC
average_GCC_rating <- mean(datazones_GCC$Rating_2019)
reduced_access_pop_GCC <- datazones_GCC %>% filter(Change < -1) %>%  
  summarise(TotalPopulation = sum(TotPop2011, na.rm = TRUE))
total_pop_GCC <- sum(datazones_GCC$TotPop2011, na.rm = TRUE)
reduced_access_pc_GCC <- reduced_access_pop_GCC$TotalPopulation / total_pop_GCC * 100


# Plot data across SPT area ----------------------------------------------
# Create a factor variable for PTJA coloring
breaks_change <- classIntervals(datazones_spt$Change, 
                                n = 5, style = "quantile")$brks
breaks_change[3] <- -1
breaks_change[4] <- 1
datazones_spt$color_change <- cut(datazones_spt$Change, 
                                       breaks = breaks_change, 
                                       include.lowest = TRUE, 
                                       labels = FALSE) %>%
  as.factor()

labels_change <- c("Large negative change","","No change","","Positive change")

ggplot() +
  geom_sf(data = datazones_spt, aes(fill = color_change), color = NA) +
  scale_fill_brewer(palette = "Spectral", 
                    direction = 1,
                    labels = labels_change) +
  theme_void() +
  theme(
    legend.text = element_text(
      size = 14,          # Change the legend text size
    ),
    legend.title = element_text(
      size = 18,          # Change the legend title text size
    ),
    plot.title = element_text(
      size = 28,          # Change the title text size
      face = "bold",      # Make the title bold
      hjust = 0.5,        # Center the title horizontally
      vjust = 1           # Adjust the vertical position
    ),
    plot.subtitle = element_text(
      size = 18          # Change the title text size
        )) +
  labs(title = "Change in Bus Accessibility Ratings \n in SPT Area 2017 - 2019",
       fill = "Change in \n Accessibility Rating",
       subtitle = paste("Datazones with reduced accessibility: ", 
                        round(reduced_pc, 2), "%\n",  
                        "Population with reduced accessibility: ", 
                        round(reduced_access_pc, 2), "%"),
       caption = "Data from Statistics.GOV.scot for Bus Accessibility \n
                      https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fbus-accessibility")

ggsave("Data/Bus_Access_SPT.jpeg", width = 10, height = 10, dpi = 300)
# Plot data across GCC area ----------------------------------------------
# Create a factor variable for PTJA coloring
breaks_change_GCC <- classIntervals(datazones_GCC$Change, 
                                n = 5, style = "quantile")$brks
breaks_change_GCC[3] <- -1
breaks_change_GCC[4] <- 1

datazones_GCC$color_change <- cut(datazones_GCC$Change, 
                                  breaks = breaks_change, 
                                  include.lowest = TRUE, 
                                  labels = FALSE) %>%
  as.factor()

GCC_pc <- nrow(filter(datazones_GCC, Change < (-1)))/nrow(datazones_GCC) *100

labels_change_GCC <- c("Large negative change","","Neutral","","Positive Change")

ggplot() +
  geom_sf(data = datazones_GCC, aes(fill = color_change), color = NA) +
  scale_fill_brewer(palette = "Spectral", 
                    direction = 1,
                    labels = labels_change_GCC) +
  theme_void() +  
  theme(
      legend.text = element_text(
      size = 14,          # Change the legend text size
    ),
    legend.title = element_text(
      size = 18,          # Change the legend title text size
    ),
    plot.title = element_text(
      size = 28,          # Change the title text size
      face = "bold",      # Make the title bold
      hjust = 0.5,        # Center the title horizontally
      vjust = 1           # Adjust the vertical position
    ),
    plot.subtitle = element_text(
      size = 18          # Change the title text size
        )) +
  labs(title = "Change in Bus Accessibility Ratings 
       in GCC Area 2017 - 2019",
       fill = "Change in \n Accessibility Rating",
       subtitle = paste("Datazones with reduced accessibility: ", 
                        round(GCC_pc, 2), 
                        "%\n",
                        "Population with reduced accessibility: ", 
                        round(reduced_access_pc_GCC, 2), "%"),
       caption = "Data from Statistics.GOV.scot for Bus Accessibility \n
                      https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fbus-accessibility")

ggsave("Data/Bus_Access_GCC.jpeg", width = 10, height = 10, dpi = 300)

write_csv(Scot_Bus_Access, "Data/Scot_Bus_Access.csv")