
###'######################################################################
###'
###' Data Visualization
###' 
###' (6) The Geospatial Patterns of School District Suspension/Expulsion Rate
###' 
###'     The relationship between the two factors:
###'     
###'     - Suspension/Expulsion Rate
###'     - Child family and neighborhood socioeconomic factors    
###' 
###' 
###' 20181210 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Spatial")


### Call libraries
library(tidyverse)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



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
library(purrr)


### A package to create thematic map
library(tmap)
# library(tmaptools)
# library(OpenStreetMap)


### Classes and method for spatial data
library(sp)


### Install a Census API Key
acs::api.key.install(key = "b6eb1ab42bceaa4ddd3f6d8979b73dab7f95acb5")
tidycensus::census_api_key("b6eb1ab42bceaa4ddd3f6d8979b73dab7f95acb5")



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


### (1) Subset CA counties around Bay Area
BA <- c(
  "San Francisco County", 
  "San Mateo County", 
  "Contra Costa County", 
  "Alameda County", 
  "Santa Clara County",
  "Santa Cruz County", 
  "Marin County", 
  "Sonoma County", 
  "Napa County", 
  "Solano County"
)

fips_BA <- fips_CA %>% filter(county %in% BA)


### (2) Subset CA counties around Los Angeles
LA <- c(
  "Los Angeles County", 
  # "Ventura County", 
  "Orange County"
)

fips_LA <- fips_CA %>% filter(county %in% LA)



###'######################################################################
###'
###' Fetch shape files
###' (1) Census tracts for Whole California
###' (2) Census tracts for Bay Area
###' (3) Census tracts for Los Angeles Area 
###' 
###'

###' Download a Census tracts shapefile
###' Get a generalized (1:500k) tracts file, 
###' rather than the most detailed TIGER/Line file
CA_tracts <- tracts(state = "CA", cb = TRUE)
BA_tracts <- tracts(state = "CA", county = fips_BA$county_code, cb = TRUE)
LA_tracts <- tracts(state = "CA", county = fips_LA$county_code, cb = TRUE)


### Check the imported shape file
summary(CA_tracts)
ggplot(CA_tracts) + geom_sf()
ggplot(BA_tracts) + geom_sf()
ggplot(LA_tracts) + geom_sf()



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
BA_tracts_merged <- sp::merge(BA_tracts, CA_income_wide, by = "GEOID")
LA_tracts_merged <- sp::merge(LA_tracts, CA_income_wide, by = "GEOID")



###'######################################################################
###'
###' Import the prepared dataset
###'
###'

### Suspension and Expulsion Data
setwd(work_dir)

load(file = "processed_data/susp_managed_wide_2011_2016.rda")
df_susp <- df_to_save

load(file = "processed_data/Exps_managed_wide_2011_2016.rda")
df_exps <- df_to_save


### Aggregate to district-level
df_susp_temp <- df_susp %>%
  filter(AcademicYear == 2016) %>%
  group_by(CountyCode, DistrictCode) %>%
  summarise(Susp_Defiance = sum(TA_N_Susp_Defiance, na.rm = TRUE), 
            Enrollment_Cum = sum(TA_Enrollment_Cum, na.rm = TRUE)) %>%
  filter(!is.na(DistrictCode))




###'######################################################################
###'
###' Merge school district feature geometry with attributes
###' 
###' (1) CA_unified/elementary/secondary: Spatial vectors
###' (2) df_dists: mapping GEOID => NCESDist => CDS code
###' (3) df_susp_temp: Attributes 
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
  
  ### Merge {(1) + (2)} + (3)
  df <- sp::merge(df, df_susp_temp, 
                  by = c("CountyCode", "DistrictCode"))
  
  ### Generate Label
  df$Dist_label <- gsub(Dist_label_vec[i], "", df$DistrictName)
  
  ### Assign object name
  assign(paste0("CA_", Dtype_vec[i]), df)
}



###'######################################################################
###'
###' Spatial subsetting for the 
###' 
###' CA_unified, CA_elementary, and CA_secondary
###'
###'

### Define a function to grab school districts in metropolitan area

metro_districts <- function(metro_name, sch_dists) {
  
  # (1) identify which states intersect the metro area using the
  # `states` function in tigris
  st <- states(cb = TRUE)
  cb <- core_based_statistical_areas(cb = TRUE)
  metro <- filter(cb, grepl(metro_name, NAME))
  
  stcodes <- st[metro,]$STATEFP
  
  
  # (2) find out which school districts are intersect the metro area
  intersect <- st_intersects(sch_dists, metro)
  
  intersect_lgl <- map_lgl(intersect, function(x) {
    if (length(x) == 1) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  
  # (3) subset and return the output
  output <- sch_dists[intersect_lgl,]
  
  return(output)
}


### Loop over 3 District Types and 2 metropolitan areas

Dtype_vec <- c("unified", "elementary", "secondary")

metro_vec <- c("BA", "LA")

metro_name_vec <- c("San Francisco", "Los Angeles")


for (i in seq_along(Dtype_vec)){
  
  for (j in seq_along(metro_vec)){
    
    sch_dists <- get(paste0("CA_", Dtype_vec[i]))
    
    assign(paste0("CA_", Dtype_vec[i], "_", metro_vec[j]), 
           metro_districts(metro_name_vec[j], sch_dists))
  }
}



###'######################################################################
###'
###' Build a thematic map: Whole California
###'
###'

Dtype_vec <- c("unified", "elementary", "secondary")

Dist_label_vec <- c(" Unified", "Elementary", "High")

setwd(work_dir)


for (i in seq_along(Dtype_vec)){
  
  # Census tract 
  tracts <- get("CA_tracts_merged")
  
  # School districts
  sch_dists <- get(paste0("CA_", Dtype_vec[i]))
  
  # Thematic plot
  # (1) Census tract layer
  tm_shape(tracts) +
    tm_fill(col = "median_inc_est", 
            # convert2density = TRUE,
            # style= "kmeans",
            title = "Median Income", 
            palette = "Blues") +
    tm_borders(alpha = 0.2, lwd = 0.05) + 
    
    # (2) School district layers
    tm_shape(sch_dists) +
    tm_borders(lwd = 0.3, alpha = 0.5, col = "grey20") + 
    tm_bubbles(col = "Susp_Defiance", 
               size = "Enrollment_Cum", 
               border.col = "gray40", 
               border.lwd = 0.1, 
               border.alpha = 0.2, 
               scale = 1, 
               alpha = 1, 
               size.lim = c(0, 30000),
               n = 5, 
               style = "quantile", 
               palette = "-RdYlBu", 
               title.size = "K-12 Enrollment", 
               title.col = "Suspension Count (Defiance-Only)", 
               just = c("center", "center")) + 
    tm_text("Dist_label", size = 0.05, col = "grey30", just = "bottom") + 
    tm_layout(
      title = paste0(Dist_label_vec[i], " School Districts"),
      title.size = 0.5, 
      legend.title.size = 1,
      legend.text.size = 1,
      legend.outside = FALSE, 
      legend.position = c("left","bottom"),
      legend.bg.alpha = 1
    )
  
  tmap_save(filename = paste0("figures/", "CA_", Dtype_vec[i], "_2016", ".pdf"))
}



###'######################################################################
###'
###' Build a thematic map: Two metropolitan areas
###' (1) Bay Area
###' (2) Los Angeles
###'
###'

Dtype_vec <- c("unified", "elementary", "secondary")

Dist_label_vec <- c(" Unified", "Elementary", "High")

metro_vec <- c("BA", "LA")

metro_name_vec <- c("San Francisco", "Los Angeles")

setwd(work_dir)


for (i in seq_along(Dtype_vec)){
  
  for (j in seq_along(metro_vec)){
    
    # Census tract 
    tracts <- get(paste0(metro_vec[j], "_tracts_merged"))
    
    # School districts
    sch_dists <- get(paste0("CA_", Dtype_vec[i], "_", metro_vec[j]))
    
    # Thematic plot
    # (1) Census tract layer
    tm_shape(tracts) +
      tm_fill(col = "median_inc_est", 
              # convert2density = TRUE,
              # style= "kmeans",
              title = "Median Income", 
              palette = "Blues") +
      tm_borders(alpha = 0.2, lwd = 0.05) + 
      
      # (2) School district layers
      tm_shape(sch_dists) +
      tm_borders(lwd = 0.3, alpha = 0.5, col = "grey20") + 
      tm_bubbles(col = "Susp_Defiance", 
                 size = "Enrollment_Cum", 
                 border.col = "gray40", 
                 border.lwd = 0.1, 
                 border.alpha = 0.2, 
                 scale = 0.5, 
                 alpha = 1, 
                 size.lim = c(0, 30000), 
                 n = 5, 
                 style = "quantile", 
                 palette = "-RdYlBu", 
                 title.size = "K-12 Enrollment", 
                 title.col = "Suspension Count (Defiance-Only)", 
                 just = c("center", "center")) + 
      tm_text("Dist_label", size = 0.2, col = "grey30", just = "bottom") + 
      tm_layout(
        title = paste0(Dist_label_vec[i], " School Districts"),
        title.size = 0.5, 
        legend.title.size = 0.7,
        legend.text.size = 0.5,
        legend.outside = FALSE, 
        legend.position = c("left","bottom"),
        legend.bg.alpha = 1
      )
    
    tmap_save(filename = paste0("figures/", metro_vec[j], "_", Dtype_vec[i], "_2016", ".pdf"))
  }
}
