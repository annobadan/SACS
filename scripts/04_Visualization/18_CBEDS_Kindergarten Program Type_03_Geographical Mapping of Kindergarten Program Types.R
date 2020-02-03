
###'######################################################################
###'
###' Task    : Geographical Mapping of Kingergarten Program Types
###'           
###' Category: Visualization
###' 
###' Data    : CBEDS Data about Schools & Districts
###'           2015-16, 2016-17, 2017-18
###' 
###' Date    : 2019-02-18
###' 
###' Author  : JoonHo Lee (joonho@berkeley.edu)
###' 
###' 

###'######################################################################
###'
###' Basic settings
###'
###'

### Start with a clean slate
gc()            # force R to release memory it is no longer using
rm(list=ls())   # delete all the objects in the workspace


### Set working directory 
work_dir <- c("~/SACS")
setwd(work_dir)


### Set a directory containing large data files
data_folder <- c("C:/Users/joonh/OneDrive/Data")  
data_dir <- file.path(data_folder, 
                      "LCFF", 
                      "Staff_Data", 
                      "Certificated_Staff", 
                      "CBEDS Data")


### Call libraries
library(tidyverse)
library(readxl)
library(Hmisc)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Load the managed dataset
###'
###'

setwd(data_dir)
load(file = "CBEDS_School_Data_01_Kindergarten_Program_Type_3-year-long_managed.rda")
df_elem <- df_to_save



###'######################################################################
###'
###' Load the required packages for the geospatial data analysis 
###' 
###'

### Packages to encode spatial vector / raster data
library(sf)
library(raster) 


###' A package to load Census TIGER/Line Shapefiles
###' Set options to cache data from tigris and load the data as sf objects.
library(tigris)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
library(USAboundaries) # STATES/counties data


### Packages to load and manipulate American Community Survey (ACS) data
library(acs)
library(tidycensus)


### A package to create thematic map
library(tmap)
# library(tmaptools)
# library(OpenStreetMap)


### Classes and method for spatial data
library(sp)


### Install a Census API Key
acs::api.key.install(key = "b6eb1ab42bceaa4ddd3f6d8979b73dab7f95acb5")
tidycensus::census_api_key("b6eb1ab42bceaa4ddd3f6d8979b73dab7f95acb5")


### Packages for leaflet mapping
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(RColorBrewer)



###'######################################################################
###'
###' Call dataset with FIPS codes for US states and counties
###'
###'

### Assign FIPS for whole US states
fips <- tidycensus::fips_codes


### Subset only CA counties
fips_CA <- fips %>%
  filter(state == "CA")



###'######################################################################
###'
###' Fetch shape files
###' 
###' Census tracts for Whole California
###' 
###'

###' Download a Census tracts shapefile
###' Get a generalized (1:500k) tracts file, 
###' rather than the most detailed TIGER/Line file
CA_tracts <- tracts(state = "CA", cb = TRUE)


### Check the imported shape file
summary(CA_tracts)
# ggplot(CA_tracts) + geom_sf()



###'######################################################################
###'
###' Obtain data and feature geometry for 
###' the five-year American Community Survey (ACS)
###'
###' (1) B06011_001: Median income in the past 12 months. TOTAL
###' 
###' (2) B14006_001: Income in the past 12 months below the poverty level
###' 
###' + add more variables  
###'
###' See: https://api.census.gov/data/2016/acs/acs1/variables.html
###' 
###'

### Get data for every Census tract in California
CA_income <- map_df("CA", function(x) {
  get_acs(geography = "tract", variables = c("B06011_001", "B14006_001"), 
          state = x)
})


### Spread the imported long dataframe
CA_income_wide <- CA_income %>% 
  gather(key, value, -(1:3))  %>% 
  unite(temp, variable, key) %>%
  spread(temp, value)

names(CA_income_wide) <- c("GEOID", "tract_name",
                           "median_inc_est", "median_inc_err", 
                           "poverty_est", "poverty_err")


### Merge with the feature geometry
CA_tracts_merged <- sp::merge(CA_tracts, CA_income_wide, by = "GEOID")



###'######################################################################
###'
###' Merge school district feature geometry with attributes
###' 
###' (1) CA_unified/elementary/secondary: Spatial vectors
###' (2) df_dists: mapping GEOID => NCESDist => CDS code
###' (3) df_SACS: Attributes 
###' 
###' 

### Import df_dists
setwd(work_dir)
load(file = "processed_data/IDs_district.rda")


Dtype_vec <- c("unified", "elementary", "secondary")

Dist_label_vec <- c(" Unified", "Elementary", "High")


for (i in seq_along(Dtype_vec)){
  
  ### Fetch shape file
  df <- tigris::school_districts("CA", type = Dtype_vec[i], year = 2016)
  
  ### Convert string GEOID to numeric
  df$GEOID <- as.numeric(df$GEOID)
  
  ### Merge (1) + (2) 
  df <- sp::merge(df, df_dists, by.x = "GEOID", by.y = "NCESDist")
  
  # ### Merge {(1) + (2)} + (3)
  # df <- sp::merge(df, df_SACS, 
  #                 by.x = c("CountyCode", "DistrictCode"), by.y = c("Ccode", "Dcode"))
  
  ### Generate Label
  df$Dist_label <- gsub(Dist_label_vec[i], "", df$DistrictName)
  
  ### Assign object name
  assign(paste0("CA_", Dtype_vec[i]), df)
}



###'######################################################################
###'
###' Prepare Leaflet mapping
###'
###'

### Set display options
options("viewer" = function(url, ...) utils::browseURL(url))


### Filter out schools not serving kindergarten graders
df_elem <- df_elem %>%
  filter(Kinder_Served == "Served")


### Check missing values in Latitude and Longitude
summary(df_elem$Latitude)
summary(df_elem$Longitude)
nrow(df_elem)


### Define layer dataframes by year
tabdf(df_elem, AcademicYear)

df_elem_2015 <- df_elem %>%
  filter(AcademicYear == 2015) 

df_elem_2016 <- df_elem %>%
  filter(AcademicYear == 2016)

df_elem_2017 <- df_elem %>%
  filter(AcademicYear == 2017)


### Extract shape file from the tidycensus
CA_tracts_shp <- CA_tracts_merged %>%
  st_transform(crs = "+init=epsg:4326")

CA_unified_shp <- CA_unified %>%
  st_transform(crs = "+init=epsg:4326")

CA_elementary_shp <- CA_elementary %>%
  st_transform(crs = "+init=epsg:4326")

CA_secondary_shp <- CA_secondary %>%
  st_transform(crs = "+init=epsg:4326")


### Save the resulting objects for later use
setwd(data_dir)
save(df_elem, file = "df_elem.rda")
save(CA_tracts_shp, file = "CA_tracts_shp.rda")
save(CA_unified_shp, file = "CA_unified_shp.rda")
save(CA_elementary_shp, file = "CA_elementary_shp.rda")
save(CA_secondary_shp, file = "CA_secondary_shp.rda")

  
### Set color palette
tabdf(df_elem, Kindergarten3)

pal <- colorFactor(palette = c("red", "blue", "green"), 
                   levels = c("Full day", "Part day", "None"), 
                   ordered = FALSE)

inc_pal <- colorQuantile(palette = "YlGn", n = 5,  
                         domain = log(CA_tracts_merged$median_inc_est))

summary(CA_tracts_merged$median_inc_est)




###'######################################################################
###'
###' Leaflet Plot (1) Kindergarten Maps
###'
###'

### Build a leaflet map
Kindergarten_Map <- df_elem %>% 
  
  ### (1) Initiate leaflet map and basic settings
  leaflet() %>%
  addSearchOSM() %>%
  # addReverseSearchOSM() %>%
  addResetMapButton() %>%
  
  
  ### (2) Set tiles
  # addTiles() %>%
  # addProviderTiles(provider = "CartoDB") %>%
  
  addMapPane(name = "polygons", zIndex = 410) %>% 
  addMapPane(name = "maplabels", zIndex = 420) %>%  # higher zIndex rendered on top
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  addProviderTiles("CartoDB.PositronOnlyLabels", 
                   options = leafletOptions(pane = "maplabels"),
                   group = "map labels") %>%
  
  
  ### (3) Add Census tract and school district polygons
  addPolygons(data = CA_tracts_shp,
              weight = 1, 
              stroke = TRUE,
              color = "darkgray", 
              fillColor = ~inc_pal(log(median_inc_est)), 
              fillOpacity = 0.5) %>%
  
  addPolygons(data = CA_unified_shp, 
              label = ~NAME, 
              weight = 1, 
              stroke = TRUE, 
              color = "black", 
              fillOpacity = 0) %>%
  
  addPolygons(data = CA_elementary_shp, 
              label = ~NAME, 
              weight = 1, 
              stroke = TRUE, 
              color = "black", 
              fillOpacity = 0) %>%
  
  addPolygons(data = CA_secondary_shp, 
              label = ~NAME, 
              weight = 1, 
              stroke = TRUE, 
              color = "black", 
              fillOpacity = 0) %>%
  
  
  ### (4) Add Circle Markers
  addCircleMarkers(data = df_elem_2015, 
                   lng = ~Longitude, lat = ~Latitude, 
                   radius = 1, 
                   opacity = 1, 
                   color = ~pal(Kindergarten3), 
                   # label = ~paste0(SchoolName, 
                   #                 " (FRPM = ", round(MN_PCT_FRPM, 2), "%)"), 
                   popup = ~paste0("<b>", SchoolName, "</b>", 
                                   "<br/>", "School Type: ", SOC, 
                                   "<br/>", "Total Enrollment: ", Total_Enroll, 
                                   "<br/>", "Kindergarten Enrollment: ", 
                                   N_KDGN, " (", round(PCT_KDGN, 1), "%", ")",   
                                   "<br/>", "Kindergarten Program: ", 
                                   Kindergarten3, 
                                   "<br/>", "Transitional Kindergarten Program: ", 
                                   Transitional_K3,
                                   "<br/>", "White: ", 
                                   round(PCT_White, 1), "%", 
                                   "<br/>", "Hispanic or Latino: ",   
                                   round(PCT_Latino, 1), "%",
                                   "<br/>", "FRPM: ", 
                                   round(PCT_FRPM_CALPADS_Undup, 1), "%", 
                                   "<br/>", quartile),
                   group = "2015-16") %>%
  
  addCircleMarkers(data = df_elem_2016, 
                   lng = ~Longitude, lat = ~Latitude, 
                   radius = 1, 
                   opacity = 1,
                   color = ~pal(Kindergarten3), 
                   # label = ~paste0(SchoolName, 
                   #                 " (FRPM = ", round(MN_PCT_FRPM, 2), "%)"), 
                   popup = ~paste0("<b>", SchoolName, "</b>", 
                                   "<br/>", "School Type: ", SOC, 
                                   "<br/>", "Total Enrollment: ", Total_Enroll, 
                                   "<br/>", "Kindergarten Enrollment: ", 
                                   N_KDGN, " (", round(PCT_KDGN, 1), "%", ")",   
                                   "<br/>", "Kindergarten Program: ", 
                                   Kindergarten3, 
                                   "<br/>", "Transitional Kindergarten Program: ", 
                                   Transitional_K3,
                                   "<br/>", "White: ", 
                                   round(PCT_White, 1), "%", 
                                   "<br/>", "Hispanic or Latino: ",   
                                   round(PCT_Latino, 1), "%",
                                   "<br/>", "FRPM: ", 
                                   round(PCT_FRPM_CALPADS_Undup, 1), "%", 
                                   "<br/>", quartile),
                   group = "2016-17") %>%
  
  addCircleMarkers(data = df_elem_2017, 
                   lng = ~Longitude, lat = ~Latitude, 
                   radius = 1, 
                   opacity = 1,
                   color = ~pal(Kindergarten3), 
                   # label = ~paste0(SchoolName, 
                   #                 " (FRPM = ", round(MN_PCT_FRPM, 2), "%)"), 
                   popup = ~paste0("<b>", SchoolName, "</b>", 
                                   "<br/>", "School Type: ", SOC, 
                                   "<br/>", "Total Enrollment: ", Total_Enroll, 
                                   "<br/>", "Kindergarten Enrollment: ", 
                                   N_KDGN, " (", round(PCT_KDGN, 1), "%", ")",   
                                   "<br/>", "Kindergarten Program: ", 
                                   Kindergarten3, 
                                   "<br/>", "Transitional Kindergarten Program: ", 
                                   Transitional_K3,
                                   "<br/>", "White: ", 
                                   round(PCT_White, 1), "%", 
                                   "<br/>", "Hispanic or Latino: ",   
                                   round(PCT_Latino, 1), "%",
                                   "<br/>", "FRPM: ", 
                                   round(PCT_FRPM_CALPADS_Undup, 1), "%", 
                                   "<br/>", quartile),
                   group = "2017-18") %>%
  

  ### (5) Add Layer Control
  addLayersControl(baseGroups = c("2015-16", "2016-17", "2017-18"), 
                   options = layersControlOptions(collapsed = FALSE), 
                   position = "topleft") %>%
  
  
  ### (6) Add legend
  addLegend(title = "Kindergarten Program Type", 
            pal = pal, 
            values = c("Full day", "Part day", "None"), 
            position = "topleft", 
            opacity = 10) %>%

  addLegend(title = "Census Tract Median Income Quintile", 
            pal = inc_pal, 
            values = log(CA_tracts_merged$median_inc_est), 
            position = "topleft", 
            opacity = 10)
  

# ### (7) Adding a piece of flair
# addSearchFeatures(
#   targetGroups = c("2015-16", "2016-17", "2017-18"), 
#   options = searchFeaturesOptions(zoom = 18)
# )

Kindergarten_Map


### Save leaflet object as html file
setwd(work_dir)
saveWidget(Kindergarten_Map, 
           file = "Map01_Kindergarten Program Type 2015-17.html")



###'######################################################################
###'
###' Leaflet Plot (2) Transitional Kindergarten Maps
###'
###'

### Build a leaflet map
Transitional_Kindergarten_Map <- df_elem %>% 
  
  ### (1) Initiate leaflet map and basic settings
  leaflet() %>%
  addSearchOSM() %>%
  # addReverseSearchOSM() %>%
  addResetMapButton() %>%
  
  
  ### (2) Set tiles
  # addTiles() %>%
  # addProviderTiles(provider = "CartoDB") %>%
  
  addMapPane(name = "polygons", zIndex = 410) %>% 
  addMapPane(name = "maplabels", zIndex = 420) %>%  # higher zIndex rendered on top
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  addProviderTiles("CartoDB.PositronOnlyLabels", 
                   options = leafletOptions(pane = "maplabels"),
                   group = "map labels") %>%
  
  
  ### (3) Add Census tract and school district polygons
  addPolygons(data = CA_tracts_shp,
              weight = 1, 
              stroke = TRUE,
              color = "darkgray", 
              fillColor = ~inc_pal(log(median_inc_est)), 
              fillOpacity = 0.5) %>%
  
  addPolygons(data = CA_unified_shp, 
              label = ~NAME, 
              weight = 1, 
              stroke = TRUE, 
              color = "black", 
              fillOpacity = 0) %>%
  
  addPolygons(data = CA_elementary_shp, 
              label = ~NAME, 
              weight = 1, 
              stroke = TRUE, 
              color = "black", 
              fillOpacity = 0) %>%
  
  addPolygons(data = CA_secondary_shp, 
              label = ~NAME, 
              weight = 1, 
              stroke = TRUE, 
              color = "black", 
              fillOpacity = 0) %>%
  
  
  ### (4) Add Circle Markers
  addCircleMarkers(data = df_elem_2015, 
                   lng = ~Longitude, lat = ~Latitude, 
                   radius = 1, 
                   opacity = 1, 
                   color = ~pal(Transitional_K3), 
                   # label = ~paste0(SchoolName, 
                   #                 " (FRPM = ", round(MN_PCT_FRPM, 2), "%)"), 
                   popup = ~paste0("<b>", SchoolName, "</b>", 
                                   "<br/>", "School Type: ", SOC, 
                                   "<br/>", "Total Enrollment: ", Total_Enroll, 
                                   "<br/>", "Kindergarten Enrollment: ", 
                                   N_KDGN, " (", round(PCT_KDGN, 1), "%", ")",   
                                   "<br/>", "Kindergarten Program: ", 
                                   Kindergarten3, 
                                   "<br/>", "Transitional Kindergarten Program: ", 
                                   Transitional_K3,
                                   "<br/>", "White: ", 
                                   round(PCT_White, 1), "%", 
                                   "<br/>", "Hispanic or Latino: ",   
                                   round(PCT_Latino, 1), "%",
                                   "<br/>", "FRPM: ", 
                                   round(PCT_FRPM_CALPADS_Undup, 1), "%", 
                                   "<br/>", quartile),
                   group = "2015-16") %>%
  
  addCircleMarkers(data = df_elem_2016, 
                   lng = ~Longitude, lat = ~Latitude, 
                   radius = 1, 
                   opacity = 1,
                   color = ~pal(Transitional_K3), 
                   # label = ~paste0(SchoolName, 
                   #                 " (FRPM = ", round(MN_PCT_FRPM, 2), "%)"), 
                   popup = ~paste0("<b>", SchoolName, "</b>", 
                                   "<br/>", "School Type: ", SOC, 
                                   "<br/>", "Total Enrollment: ", Total_Enroll, 
                                   "<br/>", "Kindergarten Enrollment: ", 
                                   N_KDGN, " (", round(PCT_KDGN, 1), "%", ")",   
                                   "<br/>", "Kindergarten Program: ", 
                                   Kindergarten3, 
                                   "<br/>", "Transitional Kindergarten Program: ", 
                                   Transitional_K3,
                                   "<br/>", "White: ", 
                                   round(PCT_White, 1), "%", 
                                   "<br/>", "Hispanic or Latino: ",   
                                   round(PCT_Latino, 1), "%",
                                   "<br/>", "FRPM: ", 
                                   round(PCT_FRPM_CALPADS_Undup, 1), "%", 
                                   "<br/>", quartile),
                   group = "2016-17") %>%
  
  addCircleMarkers(data = df_elem_2017, 
                   lng = ~Longitude, lat = ~Latitude, 
                   radius = 1, 
                   opacity = 1,
                   color = ~pal(Transitional_K3), 
                   # label = ~paste0(SchoolName, 
                   #                 " (FRPM = ", round(MN_PCT_FRPM, 2), "%)"), 
                   popup = ~paste0("<b>", SchoolName, "</b>", 
                                   "<br/>", "School Type: ", SOC, 
                                   "<br/>", "Total Enrollment: ", Total_Enroll, 
                                   "<br/>", "Kindergarten Enrollment: ", 
                                   N_KDGN, " (", round(PCT_KDGN, 1), "%", ")",   
                                   "<br/>", "Kindergarten Program: ", 
                                   Kindergarten3, 
                                   "<br/>", "Transitional Kindergarten Program: ", 
                                   Transitional_K3,
                                   "<br/>", "White: ", 
                                   round(PCT_White, 1), "%", 
                                   "<br/>", "Hispanic or Latino: ",   
                                   round(PCT_Latino, 1), "%",
                                   "<br/>", "FRPM: ", 
                                   round(PCT_FRPM_CALPADS_Undup, 1), "%", 
                                   "<br/>", quartile),
                   group = "2017-18") %>%
  
  
  ### (5) Add Layer Control
  addLayersControl(baseGroups = c("2015-16", "2016-17", "2017-18"), 
                   options = layersControlOptions(collapsed = FALSE), 
                   position = "topleft") %>%
  
  
  ### (6) Add legend
  addLegend(title = "Transitional Kindergarten Program Type", 
            pal = pal, 
            values = c("Full day", "Part day", "None"), 
            position = "topleft", 
            opacity = 10) %>%
  
  addLegend(title = "Census Tract Median Income Quintile", 
            pal = inc_pal, 
            values = log(CA_tracts_merged$median_inc_est), 
            position = "topleft", 
            opacity = 10)


# ### (7) Adding a piece of flair
# addSearchFeatures(
#   targetGroups = c("2015-16", "2016-17", "2017-18"), 
#   options = searchFeaturesOptions(zoom = 18)
# )

Transitional_Kindergarten_Map


### Save leaflet object as html file
setwd(work_dir)
saveWidget(Transitional_Kindergarten_Map, 
           file = "Map02_Transitional Kindergarten Program Type 2015-17.html")








