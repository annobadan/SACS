
###'######################################################################
###'
###' Data Visualization
###' 
###' (6) The Geospatial Patterns of School District Spending
###' 
###'     The relationship between the two factors:
###'     
###'     - Per-pupil Spending / Quality of schools
###'     - Child family and neighborhood socioeconomic factors 
###' 
###' 
###' <Rationale>
###' 
###' The primary empirical challenge in estimating the effects of school spending on student outcomes 
###' is that spending and the quality of schools tend to be highly correlated with 
###' child family and neighborhood socioeconomic factors, 
###' due to the combination of parental choices and residential location constraints 
###' (e.g., zoning policies and availability of affordable housing) that 
###' sort more advantaged children into better quality schools. 
###' 
###' Compensatory spending reforms may understate the effects of increased funding 
###' on student outcomes if the pre-existing student disadvantage 
###' that funding is targeted toward is not fully taken into account.
###'   
###' 
###' 20180811 JoonHo Lee
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
list.files("code/functions", full.names = TRUE) %>% walk(source)



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

fips <- tidycensus::fips_codes



###'######################################################################
###'
###' Fetch shape files
###' (1) Census tracts for Whole California 
###' 
###'

###' Download a Census tracts shapefile
###' Get a generalized (1:500k) tracts file, 
###' rather than the most detailed TIGER/Line file
CA_tracts <- tracts("CA", cb = TRUE)


### Check the imported shape file
summary(CA_tracts)
# plot(CA_tracts)
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
###' Import the prepared dataset
###'
###'

### Expenditures per ADA
setwd(work_dir)
load(file = "processed_data/list_expenditures_def_1_2.RData")


### Extract the dataframe from the list
datalist <- list_expenditures_def_1
idx <- which(names(datalist) == "total_exp")
df <- datalist[[idx]]


### Sort out districts with continuous operation
df <- operation14(df)


### Subset by Fiscalyear
df_SACS <- df %>%
  filter(Fiscalyear == 2016)



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
  
  ### Merge {(1) + (2)} + (3)
  df <- sp::merge(df, df_SACS, 
                  by.x = c("CountyCode", "DistrictCode"), by.y = c("Ccode", "Dcode"))
  
  ### Generate Label
  df$Dist_label <- gsub(Dist_label_vec[i], "", df$DistrictName)
  
  ### Assign object name
  assign(paste0("CA_", Dtype_vec[i]), df)
}



###'######################################################################
###'
###' Build a thematic map: Unified School Districts
###'
###'

# (1) Census tract layer
tm_shape(CA_tracts_merged) +
  tm_fill(col = "median_inc_est", 
          # convert2density = TRUE,
          # style= "kmeans",
          title = "Median Income", 
          palette = "Blues") +
  tm_borders(alpha = 0.2, lwd = 0.05) + 
  
# (2) School district layers - Unified
  tm_shape(CA_unified) +
  tm_borders(lwd = 0.3, alpha = 0.5, col = "grey60") + 
  tm_text("Dist_label", size = 0.05, col = "grey30", just = "bottom") + 
  tm_bubbles(col = "sum_value_PP_16", 
             # size = "K12ADA", 
             border.col = "gray40", 
             border.lwd = 0.1, 
             border.alpha = 0.2, 
             scale = 0.05, 
             alpha = 1, 
             size.lim = c(0, 30000), 
             n = 5, 
             style = "quantile", 
             palette = "-RdYlBu", 
             title.size = "K-12 Enrollment", 
             title.col = "Per-pupil Expenditure", 
             just = c("center", "center")) 

tmap_save(filename = "unified_2016.pdf")
             
             


###'######################################################################
###'
###' Build a thematic map: Elementary School Districts
###'
###'

# (1) Census tract layer
tm_shape(CA_tracts_merged) +
  tm_fill(col = "median_inc_est", 
          # convert2density = TRUE,
          # style= "kmeans",
          title = "Median Income", 
          palette = "Blues") +
  tm_borders(alpha = 0.2, lwd = 0.05) + 
  
  # (2) School district layers - Unified
  tm_shape(CA_elementary) +
  tm_borders(lwd = 0.3, alpha = 0.5, col = "grey60") + 
  tm_text("Dist_label", size = 0.1, col = "grey30", just = "bottom") + 
  tm_bubbles(col = "sum_value_PP_16", 
             # size = "K12ADA", 
             border.col = "gray40", 
             border.lwd = 0.1, 
             border.alpha = 0.2, 
             scale = 0.1, 
             alpha = 1, 
             size.lim = c(0, 30000), 
             n = 5, 
             style = "cont", 
             palette = "-RdYlBu", 
             title.size = "K-12 Enrollment", 
             title.col = "Per-pupil Expenditure", 
             just = c("center", "center")) 

tmap_save(filename = "elementary_2016.pdf")



###'######################################################################
###'
###' Build a thematic map: Secondary School Districts
###'
###'

# (1) Census tract layer
tm_shape(CA_tracts_merged) +
  tm_fill(col = "median_inc_est", 
          # convert2density = TRUE,
          # style= "kmeans",
          title = "Median Income", 
          palette = "Blues") +
  tm_borders(alpha = 0.2, lwd = 0.05) + 
  
  # (2) School district layers - Unified
  tm_shape(CA_secondary) +
  tm_borders(lwd = 0.3, alpha = 0.5, col = "grey60") + 
  tm_text("Dist_label", size = 0.1, col = "grey30", just = "bottom") + 
  tm_bubbles(col = "sum_value_PP_16", 
             # size = "K12ADA", 
             border.col = "gray40", 
             border.lwd = 0.1, 
             border.alpha = 0.2, 
             scale = 0.1, 
             alpha = 1, 
             size.lim = c(0, 30000), 
             n = 5, 
             style = "cont", 
             palette = "-RdYlBu", 
             title.size = "K-12 Enrollment", 
             title.col = "Per-pupil Expenditure", 
             just = c("center", "center")) 

tmap_save(filename = "secondary_2016.pdf")
             
     
  
  





