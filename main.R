# Installation
# Uncomment if you have not installed
# If commented, the RShiny functions crash
# install.packages("qdapRegex")
# install.packages("leaflet")
# install.packages("leaflet.minicharts")
# install.packages("geosphere")
library(qdapRegex)
library(leaflet)
library(shiny)
library(leaflet.minicharts)
library(sf)
library(geosphere)
library(rlist)
library(ggplot2)
library(tidyr)
library(dplyr)

#######################################################################
#######################################################################
########################### Read CSV Files ############################
#######################################################################
#######################################################################
# Read zipcodes.csv for borough grouped zipcodes (Manhattan, Brooklyn, Bronx, Queens, Staten Island)
zipcodes <- read.csv("~/Documents/AbbuKyraAlyssa_Project/zipcodes.csv")

# Read Health_Facility.csv for information on every health facility in New York State
# Add columns to be used with zipcodes.csv assignment (Exists, Facility.Borough); set to default
health_facility_orig <- read.csv("~/Documents/AbbuKyraAlyssa_Project/Health_Facility.csv")
health_facility <- health_facility_orig %>% 
  mutate(Exists = FALSE, Facility.Borough = NA)

# Read Bus_Stop_Shelter.csv for information on every bus stop shelter in New York City
bus_stop_shelter <- read.csv("~/Documents/AbbuKyraAlyssa_Project/Bus_Stop_Shelter.csv")

# Read Subway_Station.csv for information on every subway station in New York City
subway_station <- read.csv("~/Documents/AbbuKyraAlyssa_Project/Subway_Station.csv")

# Read NYC Population Data
nyc_population <- read.csv("~/Documents/AbbuKyraAlyssa_Project/nyc_population.csv") %>%
  mutate(Exists = FALSE, Boro = "")
  
#######################################################################
#######################################################################
################# Data Cleaning: Health Facility ######################
#######################################################################
#######################################################################
# (a) Assign the zipcode of each facility with a borough

# If a zipcode is in zipcodes$Manhattan, assign health_facility$Facility.Borough as Manhattan
health_facility$Exists = health_facility$Facility.Zip.Code %in% zipcodes$Manhattan
for (i in 1:length(health_facility$Facility.Zip.Code)) {
  if (health_facility$Exists[i] == TRUE)
    health_facility$Facility.Borough[i] = "Manhattan"
  health_facility$Exists[i] = FALSE
}

# If a zipcode is in zipcodes$Brooklyn, assign health_facility$Facility.Borough as Brooklyn
health_facility$Exists = health_facility$Facility.Zip.Code %in% zipcodes$Brooklyn
for (i in 1:length(health_facility$Facility.Zip.Code)) {
  if (health_facility$Exists[i] == TRUE)
    health_facility$Facility.Borough[i] = "Brooklyn"
  health_facility$Exists[i] = FALSE
}  
  
# If a zipcode is in zipcodes$Bronx, assign health_facility$Facility.Borough as Bronx
health_facility$Exists = health_facility$Facility.Zip.Code %in% zipcodes$Bronx
for (i in 1:length(health_facility$Facility.Zip.Code)) {
  if (health_facility$Exists[i] == TRUE)
    health_facility$Facility.Borough[i] = "Bronx"
  health_facility$Exists[i] = FALSE
}  

# If a zipcode is in zipcodes$Queens, assign health_facility$Facility.Borough as Queens  
health_facility$Exists = health_facility$Facility.Zip.Code %in% zipcodes$Queens
for (i in 1:length(health_facility$Facility.Zip.Code)) {
  if (health_facility$Exists[i] == TRUE)
    health_facility$Facility.Borough[i] = "Queens"
  health_facility$Exists[i] = FALSE
}  
  
# If a zipcode is in zipcodes$Staten.Island, assign health_facility$Facility.Borough as Staten Island  
health_facility$Exists = health_facility$Facility.Zip.Code %in% zipcodes$Staten.Island
for (i in 1:length(health_facility$Facility.Zip.Code)) {
  if (health_facility$Exists[i] == TRUE)
    health_facility$Facility.Borough[i] = "Staten Island"
  health_facility$Exists[i] = FALSE
}  

### Finalized Data ##
# (b) Exclude health facilities with zipcodes outside of New York City's 5 Boroughs
health_facility = health_facility %>% 
  mutate(Nearest.Subway.Station = "", Distance.Subway.Station = Inf) %>% 
  filter(health_facility$Facility.Borough == "Manhattan" | 
           health_facility$Facility.Borough == "Brooklyn" |
           health_facility$Facility.Borough == "Bronx" |
           health_facility$Facility.Borough == "Queens" |
           health_facility$Facility.Borough == "Staten Island") %>%
  select(Facility.ID, Facility.Zip.Code, Facility.Name, Facility.Longitude,Facility.Latitude, Facility.Borough,
         Nearest.Subway.Station, Distance.Subway.Station)

#######################################################################
#######################################################################
################# Data Cleaning: Bus Stop Shelter #####################
#######################################################################
#######################################################################
### Finalized Data ##
# Exclude columns except location (string), longitude (numeric), latitude (numeric)
bus_stop_shelter_modified <- bus_stop_shelter %>% 
  select(LOCATION, LONGITUDE, LATITUDE)

#######################################################################
#######################################################################
#################  Data Cleaning: Subway Station  #####################
#######################################################################
#######################################################################
### Finalized Data ##
# The subway_station does not have longitude and latitude columns,
# Modify coordinate column into two columns for longitude and latitude
subway_station_modified <- subway_station %>% 
  mutate(Subway.Latitude = paste0("40.", rm_between(the_geom, "40.", ")", extract=TRUE)),
         Subway.Longitude = rm_between(the_geom, "(", "40.", extract=TRUE)) %>%
  mutate(Subway.Latitude = as.numeric(Subway.Latitude), Subway.Longitude = as.numeric(Subway.Longitude)) %>%
  select(OBJECTID, NAME, Subway.Longitude, Subway.Latitude)

#######################################################################
#######################################################################
################# Data Cleaning: NYC Population #######################
#######################################################################
#######################################################################
nyc_population$Exists = nyc_population$Zip.Code %in% zipcodes$Manhattan
for (i in 1:length(nyc_population$Zip.Code)) {
  if (nyc_population$Exists[i] == TRUE)
    nyc_population$Boro[i] = "Manhattan"
  nyc_population$Exists[i] = FALSE
}  

nyc_population$Exists = nyc_population$Zip.Code %in% zipcodes$Brooklyn
for (i in 1:length(nyc_population$Zip.Code)) {
  if (nyc_population$Exists[i] == TRUE)
    nyc_population$Boro[i] = "Brooklyn"
  nyc_population$Exists[i] = FALSE
}  

nyc_population$Exists = nyc_population$Zip.Code %in% zipcodes$Queens
for (i in 1:length(nyc_population$Zip.Code)) {
  if (nyc_population$Exists[i] == TRUE)
    nyc_population$Boro[i] = "Queens"
  nyc_population$Exists[i] = FALSE
}  

nyc_population$Exists = nyc_population$Zip.Code %in% zipcodes$Bronx
for (i in 1:length(nyc_population$Zip.Code)) {
  if (nyc_population$Exists[i] == TRUE)
    nyc_population$Boro[i] = "Bronx"
  nyc_population$Exists[i] = FALSE
} 

nyc_population$Exists = nyc_population$Zip.Code %in% zipcodes$Staten.Island
for (i in 1:length(nyc_population$Zip.Code)) {
  if (nyc_population$Exists[i] == TRUE)
    nyc_population$Boro[i] = "Staten Island"
  nyc_population$Exists[i] = FALSE
}  

nyc_population <- nyc_population %>% filter(nyc_population$Boro != "")

#######################################################################
#######################################################################
########################  Group by Zipcode  ###########################
#######################################################################
#######################################################################
# Data Cleaning: Some zipcodes are written in full.
zipcode_group <- health_facility %>% 
  mutate(Zip.Code = as.character(health_facility$Facility.Zip.Code)) 
zipcode_group$Zip.Code = substr(zipcode_group$Facility.Zip.Code, 1, 5)

# Change the dataset into just tallies by zipcodes
zipcode_group <- zipcode_group %>% 
  group_by(Zip.Code) %>% tally()

# Prints a summary of the number of health facilities in NYC
summary(zipcode_group)

# Prints the mode of x
mode <- function(x) { 
  ux <- unique(x) 
  ux[which.max(tabulate(match(x, ux)))] 
  }

mode(zipcode_group$n)

#######################################################################
#######################################################################
############################  Scatter Plot  ###########################
#######################################################################
#######################################################################
zip_pop <- zipcode_group %>% 
  left_join(nyc_population)  %>% 
  na.omit(zip_pop) # Removing NA

ggplot(zip_pop, aes(Population,n, color = Boro)) + 
  scale_x_discrete(breaks = seq(50000, 200000, by = 10000)) + 
  geom_point()

#######################################################################
#######################################################################
############################  Barplot  ################################
#######################################################################
#######################################################################
# Health Facility Count
health_facility_manhattan <- health_facility %>% filter(health_facility$Facility.Borough == "Manhattan")
health_facility_brooklyn <- health_facility %>% filter(health_facility$Facility.Borough == "Brooklyn")
health_facility_queens <- health_facility %>% filter(health_facility$Facility.Borough == "Queens")
health_facility_bronx <- health_facility %>% filter(health_facility$Facility.Borough == "Bronx")
health_facility_statenisland <- health_facility %>% filter(health_facility$Facility.Borough == "Staten Island")
borough_counts <- c(length(health_facility_manhattan$Facility.ID), length(health_facility_brooklyn$Facility.ID), 
                    length(health_facility_queens$Facility.ID), length(health_facility_bronx$Facility.ID), 
                    length(health_facility_statenisland$Facility.ID))
borough_names <- c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island")
hf_byborough <- data.frame(borough_names, borough_counts)

# Bus Stop Shelter Count
bus_manhattan <- bus_stop_shelter %>% filter(bus_stop_shelter$BoroName == "Manhattan") 
bus_brooklyn <- bus_stop_shelter %>% filter(bus_stop_shelter$BoroName == "Brooklyn") 
bus_queens <- bus_stop_shelter %>% filter(bus_stop_shelter$BoroName == "Queens") 
bus_bronx <- bus_stop_shelter %>% filter(bus_stop_shelter$BoroName == "Bronx") 
bus_statenisland <- bus_stop_shelter %>% filter(bus_stop_shelter$BoroName == "Staten Island") 

bus_borough_counts <- c(length(bus_manhattan$SHELTER_ID), length(bus_brooklyn$SHELTER_ID),
                       length(bus_queens$SHELTER_ID), length(bus_bronx$SHELTER_ID),
                       length(bus_statenisland$SHELTER_ID))
bus_borough_names <- c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island")
bus_byborough <- data.frame(bus_borough_names, bus_borough_counts)

#######################################################################
#######################################################################
######################## Determine Distance ###########################
### This is a failed attempt since the nearest distance does        ###
### not account for what areas are subway lines or roads.           ###
### Therefore, the items it returned are not valid. It also does    ###
### not account for the river space between certain boroughs.       ###
#######################################################################
#######################################################################
#######################################################################
#######################################################################

# get_geo_distance <- function(long1, lat1, long2, lat2, units = "miles") {
#   loadNamespace("purrr")
#   loadNamespace("geosphere")
#   longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
#   longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
#   distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
#   distance_m = list.extract(distance_list, position = 1)
#   return(distance_m)
# }
# 
# 
# for (i in 1:length(health_facility$Facility.ID)) {
#   for (j in 1:length(subway_station_modified$NAME)) {
#     new_distance = get_geo_distance(health_facility$Facility.Longitude[i],
#                                  health_facility$Facility.Latitude[i],
#                                  subway_station_modified$Subway.Longitude[j],
#                                  subway_station_modified$Subway.Latitude[j])
#     smaller = new_distance < health_facility$Distance.Subway.Station[i]
#     if (is.na(smaller)) {
#       # do nothing
#     }
#     else if (smaller == TRUE) {
#       health_facility$Nearest.Subway.Station[i] = levels(subway_station_modified$NAME)[j]
#       health_facility$Distance.Subway.Station[i] = new_distance
#     }
#   }
# }

#######################################################################
#######################################################################
########################  Hypothesis Test  ############################
#######################################################################
#######################################################################

# A researcher believes that Midtown Manhattan (10001) reflects the median
# number of health facilities in the five boroughs of New York because it 
# has the most inflow and outflow of tourists and commuters from different
# states on a daily basis. The number of health facilities is 7. Is this
# significantly different from the true location?

wilcox.test(zipcode_group$n, mu = 7, alternative = "two.sided")

# We are using the Wilcoxon signed-rank test because we are avoiding 
# the assumption of our data being normally distributed. 

# The p-value is 0.4638, which is greater than 0.05. This means
# it is not statistically different/significant and indicates 
# a strong evidence for the null hypothesis.
#######################################################################
#######################################################################
############################# RShiny App ##############################
#######################################################################
#######################################################################
# User Interface
ui <- fluidPage(
  mainPanel(
    tabsetPanel(type = "pills",
                tabPanel("Interactive Map", leafletOutput("pinnedmap")), # Displays map (refer to server)
                tabPanel("Borough Bar Graph", plotOutput("bar"))
    )
  )
)

# Server
server <- function(input, output, session) {
  output$pinnedmap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(-74.00, 40.71, zoom = 12) %>% # New York City view of the map 
      ### Overlay Groups ###
      addProviderTiles("CartoDB.Positron") %>% # Creates a black and white map
      # Adds health facility points
      addMarkers(health_facility$Facility.Longitude, health_facility$Facility.Latitude,
                 # Hovering over each point gives you its name
                 label = paste0("Health Facility: ", health_facility$Facility.Name),
                 icon = list(
                   iconUrl = 'http://icons.iconarchive.com/icons/graphicloads/medical-health/96/capsule-icon.png',
                   iconSize = c(20, 20)),
                 group = "Health Facility") %>%
      # Adds bus stop shelter points
      addMarkers(bus_stop_shelter_modified$LONGITUDE, bus_stop_shelter_modified$LATITUDE,
                 # Hovering over each point gives you its name
                 label = paste0("Bus Stop Shelter: ", bus_stop_shelter_modified$LOCATION),
               icon = list(
                 iconUrl = 'http://icons.iconarchive.com/icons/google/noto-emoji-travel-places/72/42541-bus-icon.png',
                 iconSize = c(20, 20)),
               group = "Bus Stop Shelter") %>% # Group name for overlay control
      # Adds subway shelter points
      addMarkers(subway_station_modified$Subway.Longitude, subway_station_modified$Subway.Latitude,
                 # Hovering over each point gives you its name
                 label = paste0("Subway Station: ", subway_station_modified$NAME),
                 icon = list(
                   iconUrl = 'http://icons.iconarchive.com/icons/ncrow/new-york-subway/72/Subway-Car-icon.png',
                   iconSize = c(20, 20)),
                 group = "Subway Station") %>% # Group name for overlay control
      # Layers Control
      addLayersControl(
        overlayGroups = c("Health Facility", "Bus Stop Shelter", "Subway Station"),
        options = layersControlOptions(collapsed = FALSE), # No points plotted in the beginning
      ) %>%
      # Mini Map
      addMiniMap() # A mini map to see a bigger picture
  })
  
  output$bar <- renderPlot({
    twoplot <- rbind(hf_byborough$borough_counts, bus_byborough$bus_borough_counts)
    barplot(twoplot, beside = T, col = c("darkblue", "darkred"), 
            names.arg = hf_byborough$borough_names,
            xlab = "*Excludes subway station since borough cannot be extracted from data source",
            ylab = "Count",
            legend = c("Health Facilities", "Bus Stop Shelters"))
  })
  
}

shinyApp(ui, server)

