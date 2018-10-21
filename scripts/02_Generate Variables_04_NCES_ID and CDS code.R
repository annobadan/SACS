
###'######################################################################
###'
###' Data for matching three different IDs of school districts:
###' 
###' (1) CDS code from data from California Department of Education (CDE)
###' (2) NCESID from National Center for Educational Statistics (NCESID)
###' (3) GEOID from TIGER/LINE shape files
###' 
###'  
###' 20180812 JoonHo Lee
###' 
###' 

### Package dependency
library(tidyverse)



###'######################################################################
###'
###' Import datasets
###'
###'

###' Public Schools and Districts 
###' https://www.cde.ca.gov/ds/si/ds/fspubschls.asp

data_dir <- "D:/Data/LCFF/School_Listing/Public_Schools_and_Districts"
setwd(data_dir)
load(file = "pubschls_cleaned_20171231.rda")



###'######################################################################
###'
###' Summarize at district-level & Save as processed data
###'
###'

### Generate data
df_dists <- pubschls %>%
  group_by(CountyCode, DistrictCode) %>%
  summarise_all(first) %>%
  select(CountyCode, DistrictCode, CountyName, DistrictName, 
         NCESDist, Latitude, Longitude)


### Save as processed data
work_dir <- "~/SACS"
setwd(work_dir)
save(df_dists, file = "processed_data/IDs_district.rda")



###'######################################################################
###'
###' Verify whether NCESDist = GEOID
###' 
###' => Yes, NCESDist = GEOID
###'   
###'   

### Import TIGER/LINE shape file using tigris package

library(tigris)

CA_unified <- tigris::school_districts("CA", type = "unified", year = 2016)

CA_elementary <- tigris::school_districts("CA", type = "elementary", year = 2016)

CA_secondary <- tigris::school_districts("CA", type = "secondary", year = 2016)


### Merge with NCESDist

CA_unified <- CA_unified %>%
  mutate_at(vars(GEOID), as.numeric) %>%
  left_join(df_dists, by = c("GEOID" = "NCESDist")) 

verify <- CA_unified %>%
  select(GEOID, NAME, DistrictName, CountyCode, DistrictCode)

