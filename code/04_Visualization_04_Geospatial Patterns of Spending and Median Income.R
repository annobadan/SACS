
###'######################################################################
###'
###' Data Visualization
###' 
###' (6) The Geospatial Patterns of School District Spending
###' 
###'     The relationship between the two factors:
###'     
###'     - Spending / Quality of schools
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


### Call functions
list.files("code/functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Load the required packages and 
###' set options to cache data from tigris and load the data as sf objects.
###'
###'

library(tigris)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

library(sf)
library(tidyverse)
library(raster) 
library(acs)
library(tidycensus)
library(purrr)
library(tmap)
library(tmap)

### Getting and installing a Census API Key
api.key.install(key = "b6eb1ab42bceaa4ddd3f6d8979b73dab7f95acb5")
census_api_key("b6eb1ab42bceaa4ddd3f6d8979b73dab7f95acb5")



###'######################################################################
###'
###' Fetch Census tracts for Whole California 
###'
###'

CA_tracts <- tracts("CA", cb = TRUE)

ggplot(CA_tracts) + geom_sf()

plot(CA_tracts)



###'######################################################################
###'
###' Attributes data from the American Community Survey (ACS)
###' 
###' fetches & merge tract feature geometry from the tigris package
###' 
###' See:
###' https://api.census.gov/data/2016/acs/acs1/variables.html
###'
###'

###' (1) Median income: B06011_001
###' Median income in the past 12 months (in 2016 inflation-adjusted dollars) 
###' by place of birth in the united states

CA_income <- map_df("CA", function(x) {
  get_acs(geography = "tract", variables = "B06011_001", 
          state = x)
})

CA_income_sf <- reduce(
  map("CA", function(x) {
    get_acs(geography = "tract", variables = "B06011_001", 
            state = x, geometry = TRUE)
  }), 
  rbind
)



###'######################################################################
###'
###' SChool districts data
###' 
###' (1) Total School Enrollment: B14001_001
###' 
###' See:
###' 
###' https://walkerke.github.io/tidycensus/articles/basic-usage.html#geography-in-tidycensus
###'
###'


CA_unified <- school_districts("CA", type = "unified", year = 2016)

CA_elementary <- school_districts("CA", type = "elementary", year = 2016)

CA_secondary <- school_districts("CA", type = "secondary", year = 2016)




###'######################################################################
###'
###' A first plot
###'
###'

tm_shape(CA_income_sf) +
  tm_fill(col = "estimate") +
   #tm_borders(lwd = 0.1) + 
  tm_shape(CA_unified) + 
  tm_borders(lwd = 0.1, col = "red") + 
  tm_bubbles("ALAND") +
  
  tm_shape(CA_elementary) + 
  tm_borders(lwd = 0.1, col = "blue") + 
  tm_shape(CA_secondary) + 
  tm_borders(lwd = 0.1, col = "green")





tmap_save(filename = "trial.pdf")
















