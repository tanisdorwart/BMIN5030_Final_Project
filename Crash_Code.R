# Colors: Redon, Categorical 12, Classic cyclic, Prism, 


# Install packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("RColorBrewer")
install.packages("dplyr")
install.packages("stringr")
install.packages("readr")
install.packages("tidymodels")
install.packages("dotwhisker")
install.packages("broom")
install.packages("RColorBrewer")
install.packages("paletteer")
install.packages("maps")
install.packages("sf")
install.packages("osmdata")


# Load packages
library("tidyverse")
library("ggplot2")
library("RColorBrewer")
library("dplyr")
library("stringr")
library("readr")
library("tidymodels")
library("dotwhisker")
library("broom")
library("RColorBrewer")
library("paletteer")
library("maps")
library("sf")
library("osmdata")

# Import data
crash_data <- read.csv("collision_crash_2020_2024.csv")


# ----- Clean data -----
crash_clean <- crash_data %>%
  select(
    automobile_count,
    bicycle_count,
    crash_month,
    crash_year,
    day_of_week,
    fatal,
    fatal_count,
    hour_of_day,
    motorcycle_count,
    ped_count,
    susp_minor_inj_count,
    susp_serious_inj_count,
    time_of_day,
    total_units,
    unbelted_occ_count,
    unb_death_count,
    unb_susp_serious_inj_count,
    unbelted,
    vehicle_count,
    weather1,
    weather2,
    alcohol_related,
    cell_phone,
    distracted,
    drugged_driver,
    drug_related,
    impaired_driver,
    municipality,
    county,
    dec_longitude,
    dec_latitude
  )


# Maybe throw in county instead of municipality?

crash_clean <- mutate(crash_clean, crash_month = factor(crash_month, levels = c(1:12),
                    labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")))

crash_clean <- mutate(crash_clean, day_of_week = factor(day_of_week, levels = c(1:7),
                                                        labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))

crash_clean <- crash_clean %>%
  slice(-8108)

crash_clean <- mutate(crash_clean, hour_of_day = factor(hour_of_day, levels = c(0:23),
                                                        labels = c("Midnight", "1am", "2am", "3am", "4am", "5am", "6am", "7am", "8am", "9am", "10am", "11am", 
                                                                   "Noon", "1pm", "2pm", "3pm", "4pm", "5pm", "6pm", "7pm", "8pm", "9pm", "10pm", "11pm")))

crash_clean <- mutate(crash_clean, weather1 = factor(weather1, levels = c(1:7),
                                                     labels = c("Blowing Sand, Soil, Dirt", "Blowing Snow", "Clear", "Cloudy", "Fog, Smog, Smoke", "Freezing Rain or Freezing Drizzle", "Rain")))


crash_clean <- mutate(crash_clean, weather2 = factor(weather2, levels = c(1:10, 98),
                                                     labels = c("Blowing Sand, Soil, Dirt", "Blowing Snow", "Clear", "Cloudy", "Fog, Smog, Smoke", "Freezing Rain or Freezing Drizzle", "Rain", "Severe Crosswinds", "Sleet or Hail", "Snow", "Other")))

crash_clean$fatal <- factor(crash_clean$fatal,
                             levels = c(0, 1),
                             labels = c("No", "Yes"))

crash_clean$municipality <- factor(crash_clean$municipality)


# ----- Vehicles, Bikes, Peds Involved, Fatalities ----- 

# How many accidents occurred each year from 2020-2024?
count(crash_clean, crash_year)


# How many accidents resulted in fatalities for each year?
count(crash_clean, fatal_count)

ggplot(data = crash_clean, aes(x = crash_year, fill = fatal)) +
  geom_bar(position = "stack") +
  labs(title = "Accidents Resultsing in Fatalies per Year",
       x = "Year",
       y = "Number of Accidents",
       fill = "Fatalities") +
  theme_classic()




# How many vehicles were involved in each accident?
count(crash_clean, automobile_count)

ggplot(data = crash_clean, aes(automobile_count)) +
  geom_bar(position = "stack") +
  labs(title = "Number of Vehicles Involved in Each Crash",
       x = "Number of Vehicles",
       y = "Number of Accidents") +
  theme_classic()


# How many crashes involved bicycles?

bikes <- ggplot(data = crash_clean, aes(bicycle_count)) +
  geom_bar(position = "stack", stat = "count") +
  labs(title = "Number of Bicycles Involved in Each Crash",
       x = "Number of Bicycles",
       y = "Number of Accidents") +
  theme_classic() +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4
  )
  

# ----- Day, Month, Time, Year Statistics -----
# Which month did the most crashes occur?

ggplot(crash_clean, aes(x = crash_month, fill = crash_month)) + 
  geom_bar() +
  facet_grid(rows =  vars(crash_year)) +
  theme_bw() +
  scale_fill_paletteer_d("MetBrewer::Redon") +
  labs(title = "Number of Accidents Each Month",
    x = "Month of the Year", 
    y = "Number of Accidents",
    fill = "Month") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 30, vjust = .6, hjust = 0.6))




# Which day of the week did the most crashes occur?
#DAY_OF_WEEK

ggplot(crash_clean, aes(x = day_of_week, fill = day_of_week)) + 
  geom_bar() +
  theme_bw() +
  scale_fill_paletteer_d("MetBrewer::Redon") +
  labs(title = "Number of Accidents Per Day of the Week",
       x = "Day of the Week", 
       y = "Number of Accidents",
       fill = "Day of the Week") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 30, vjust = .6, hjust = 0.6))




mutate(sex = factor(sex, levels = c(1, 2),
                    labels = c("male", "female")))

#What time of day are crashes most likely to occur?
crash_clean %>%
  filter(!is.na(hour_of_day)) %>%
  ggplot(aes(x = hour_of_day, fill = hour_of_day)) + 
  geom_bar() +
  theme_classic() +
  labs(title = "Number of Accidents Each Hour of the Day",
       x = "Hour of the Day", 
       y = "Number of Accidents",
       fill = "Hour of the Day") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 30, vjust = .6, hjust = 0.6))

# ~~~~~~~~~~~~~~~~~ Can I layer on the average number of drivers on the road in PA per hour???????????????????????/


# Most common type of vehicle involved in accidents
#MAKE_CD

# Age of driveres involved
#DRIVER_COUNT_16YR ... 65YR

# ----- Impairment -----

count(crash_clean, alcohol_related)

count(crash_clean, drug_related)

count(crash_clean, impaired_driver)


# ----- Weather ------
crash_clean %>%
  filter(!is.na(weather1)) %>%
  ggplot(aes(x = weather1, fill = weather1)) + 
  geom_bar() +
  theme_classic() +
  labs(title = "Number of Accidents per Each Weather Condition",
       x = "Weather", 
       y = "Number of Accidents",
       fill = "Weather") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 30, vjust = .6, hjust = 0.6))


# Are more accidents more likely to be caused by severe weather in the winter months?

winter_weather <- crash_clean %>%
  filter(!is.na(weather1)) %>%
  group_by(crash_month)


ggplot(winter_weather, aes(x = crash_month, fill = weather1)) + 
  geom_bar() +
  theme_classic() +
  labs(title = "Number of Accidents per Each Weather Condition",
       x = "Weather", 
       y = "Number of Accidents",
       fill = "Weather") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 30, vjust = .6, hjust = 0.6))

ggplot(winter_weather, aes(x = crash_month, fill = weather1)) + 
  geom_bar(position = "fill") +
  theme_classic() +
  labs(title = "Percentage of Accidents per Each Weather Condition",
       x = "Weather", 
       y = "Percent of Accidents",
       fill = "Weather") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 30, vjust = .6, hjust = 0.6))



### RUN SOME STATS HERE????????????????????????????????

# ----- Location -----

ggplot(crash_clean, aes(x = municipality, fill = municipality)) + 
  geom_bar() +
  theme_bw() +
  labs(title = "Number of Accidents in each Municipality",
       x = "Municipality Code", 
       y = "Number of Accidents",
       fill = "Municipality") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 30, vjust = .6, hjust = 0.6))

#~~~~~~~~~~~~~~~~
# We will also need the .geojson Crash Data 2020-2024 file from the original Metadata catalog found here: https://metadata.phila.gov/#home/datasetdetails/5543865420583086178c4eba/

#Here we can read in the .geojson file which will give us the boundaries for a map
Philly_sf <- read_sf("City_Limits.geojson")

# By plotting the longitude and latitude of each crash, we can visualize where these crashes occurred on a map of the city.
ggplot() +
  geom_sf(data = Philly_sf, fill = "grey", alpha = 0.3) +
  geom_point(
    data = crash_clean,
    aes(x = dec_longitude, y = dec_latitude),
    size = .05
  ) +
  theme_void() +
  coord_sf(xlim = c(-75.3, -74.95), ylim = c(39.7, 40.2))


# Plotting this way gives us a single data point for each crash, which is messy and doesn't allow us to identify regions which are more likely to have a crash occur.

# Cluster crashes by location
crash_cluster <- crash_clean %>%
  count(dec_longitude, dec_latitude) %>%
  arrange(n)  # low → high count so high plot last (on top)


# Get ALL local streets + main roads
philly_streets <- opq(bbox = c(-75.3, 39.7, -74.95, 40.2)) %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "secondary", "tertiary",
                            "residential", "unclassified", "service")) %>%
  osmdata_sf()

neighborhoods_url <- "https://raw.githubusercontent.com/opendataphilly/open-geo-data/master/philadelphia-neighborhoods/philadelphia-neighborhoods.geojson"
philly_neighborhoods <- sf::st_read(neighborhoods_url)





####### new?

ggplot() +
  geom_sf(data = Philly_sf, fill = "white", color = NA) + #Philly map
  geom_sf(data = philly_streets$osm_lines, color = "grey70", size = 0.2, alpha = 0.5) + #Streets
  geom_sf(data = philly_neighborhoods, fill = NA, color = "black", size = 0.2, alpha = 0.4) + #Neighborhoods
  geom_sf_text(data = philly_neighborhoods,
               aes(label = MAPNAME),
               size = 3, color = "black", alpha = 1,
               check_overlap = TRUE) + # Labels for neighborhoods
  geom_point(
    data = crash_cluster,
    aes(x = dec_longitude, y = dec_latitude, color = n),
    alpha = 0.5, size = .5
  ) + #Bubble map
  scale_color_viridis_c(option = "viridis", name = "Crash Count") +
  coord_sf(xlim = c(-75.3, -74.95), ylim = c(39.7, 40.2)) +
  theme_void() +
  labs(
    title = "Philadelphia Vehicle Crash Hotspots",
    subtitle = "High-density crash areas with neighborhood labels and streets",
    color = "Crash Count"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13),
    legend.position = "right"
  )



# This is still too much data to look at at once. What if we just look at the top 20 hotspots?

library(dplyr)
library(sf)
install.packages("lwgeom")
library(lwgeom)

# Make sure crash data is sf
crash_sf <- st_as_sf(crash_cluster, coords = c("dec_longitude", "dec_latitude"), crs = 4326)
philly_neighborhoods <- st_make_valid(philly_neighborhoods)

# Spatial join crashes to neighborhoods
crashes_by_nhood <- st_join(crash_sf, philly_neighborhoods, join = st_within)

# Count crashes per neighborhood
top20_nhoods <- crashes_by_nhood %>%
  st_drop_geometry() %>%
  group_by(NAME) %>%                           # Field name may be NAME or name — check with names(philly_neighborhoods)
  summarize(crash_count = n()) %>%
  arrange(desc(crash_count)) %>%
  slice(1:20)

# Filter neighborhoods to label only top 20
nhoods_to_label <- philly_neighborhoods %>%
  filter(NAME %in% top20_nhoods$NAME)

nhoods_to_label

ggplot() +
  geom_sf(data = Philly_sf, fill = "white", color = NA) + #Philly map
  geom_sf(data = philly_streets$osm_lines, color = "grey70", size = 0.2, alpha = 0.5) + #Streets
  geom_sf(data = philly_neighborhoods, fill = NA, color = "black", size = 0.2, alpha = 0.4) + #Neighborhoods
  geom_sf_text(data = nhoods_to_label, aes(label = NAME),
               size = 3, fontface = "bold", color = "black") + # Labels for neighborhoods
  geom_point(
    data = crash_cluster,
    aes(x = dec_longitude, y = dec_latitude, color = n),
    alpha = 0.5, size = .5
  ) + #Bubble map
  scale_color_viridis_c(option = "viridis", name = "Crash Count") +
  coord_sf(xlim = c(-75.3, -74.95), ylim = c(39.7, 40.2)) +
  theme_void() +
  labs(
    title = "Philadelphia Vehicle Crash Hotspots",
    subtitle = "High-density crash areas with neighborhood labels and streets",
    color = "Crash Count"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13),
    legend.position = "right"
  )


# ----- Other location plots and whatnot -----
# Load libraries
library(ggplot2)
library(dplyr)
library(sf)
library(lwgeom)
library(ggspatial)

install.packages("ggspatial")


# ---- Convert crash points to sf
crash_sf <- st_as_sf(crash_clean, coords = c("dec_longitude", "dec_latitude"), crs = 4326)

# ---- Fix geometry issues in neighborhoods
philly_neighborhoods <- st_make_valid(philly_neighborhoods)
st_crs(philly_streets) <- 4326   

# ---- Count crashes by neighborhood
crashes_by_nhood <- st_join(crash_sf, philly_neighborhoods, join = st_within)

top10_nhoods <- crashes_by_nhood %>%
  st_drop_geometry() %>%
  group_by(NEIGHBORHOOD = NAME) %>%
  summarize(crash_count = n()) %>%
  arrange(desc(crash_count)) %>%
  slice(1:10)

# ---- Filter neighborhoods to only those top 10 for labeling
nhoods_to_label <- philly_neighborhoods %>%
  filter(NAME %in% top10_nhoods$NEIGHBORHOOD)

# ---- Prepare crash density layer
crash_density <- crash_clean %>%
  mutate(long_bin = round(dec_longitude, 3),
         lat_bin = round(dec_latitude, 3)) %>%
  count(long_bin, lat_bin) %>%
  arrange(n)  # smallest first, biggest plotted last (on top!)

# ---- Plot
ggplot() +
  geom_sf(data = philly_streets$osm_lines, color = "grey70", size = 0.2, alpha = 0.5) +
  geom_sf(data = philly_neighborhoods, fill = NA, color = "grey60", linewidth = 0.3) +
  geom_point(
    data = crash_density,
    aes(x = long_bin, y = lat_bin, color = n),
    alpha = 0.7, size = 1
  ) +
  scale_color_viridis_c(option = "viridis", name = "Crash Count") +
  geom_sf_text(
    data = nhoods_to_label,
    aes(label = NAME),
    size = 3,
    fontface = "bold",
    color = "white"
  ) +
  labs(
    title = "Crash Density Map of Philadelphia",
    subtitle = "Top 10 Most Dangerous Neighborhoods Labeled",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  coord_sf(xlim = c(-75.3, -74.95), ylim = c(39.88, 40.15)) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )


 


