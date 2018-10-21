
###'######################################################################
###'
###' Data Management 
###' 
###' Staff Demography + Staff School FTE 2012-2017
###' 
###' Because the "SchoolCode" is absent in Staff Demography data
###' 
###' 
###' 20181001 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Staff_Data/Certificated_Staff/Staff_Demographic")


### Call libraries
library(tidyverse)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Loop over years
###'
###'

years <- sprintf("%02d",seq(12, 17))


for (i in seq_along(years)) {
  
  ###'######################################################################
  ###'
  ###' Assign year number
  ###' 
  ###' 
  
  year_num <- years[i]
  
  
  
  ###'######################################################################
  ###'
  ###' Import cleaned datafiles
  ###'
  ###'
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Staff Demographics
  load(file = paste0("StaffDemo", year_num, "_cleaned", ".rda"))
  df_demo <- df; rm(df)
  classmode(df_demo, everything())
  
  
  ### Staff School FTE
  load(file = paste0("StaffSchoolFTE", year_num, "_cleaned", ".rda"))
  df_fte <- df; rm(df)
  classmode(df_fte, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Check duplicates in the two datasets 
  ###'
  ###'
  
  ###' Staff Demography
  ###' duplicated records for each teacher within districts
  df_demo_duplicates <- df_demo %>%
    group_by(CountyCode, DistrictCode, RecID) %>%
    summarise(N = n())
  
  tabdf(df_demo_duplicates, N)  
  
  
  ###' Staff School FTE
  ###' There are some duplicates within schools & districts
  df_fte_duplicates_school <- df_fte %>%
    group_by(CountyCode, DistrictCode, SchoolCode, RecID) %>%
    summarise(N = n()) 
  
  tabdf(df_fte_duplicates_school, N) # duplicates within school
  
  
  df_fte_duplicates_district <- df_fte %>%
    group_by(CountyCode, DistrictCode, RecID) %>%
    summarise(N = n()) 
  
  tabdf(df_fte_duplicates_district, N) # duplicates within school
  
  
  
  ###'######################################################################
  ###'
  ###' Merge Staff Demography & Staff School FTE
  ###' 
  ###' key = c("CountyCode", "DistrictCode", "RecID")
  ###' 
  ###' This is possible because "Staff Demography" has no duplicated 
  ###' within the combination of keys
  ###'
  ###'
  
  ### Delete duplicated columns (other than keys) before merge
  names(df_demo)
  names(df_fte)
  
  col_dups <- names(df_demo)[names(df_demo) %in% names(df_fte)]
  keys <- c("CountyCode", "DistrictCode", "RecID")
  names_to_delete <- col_dups[!col_dups %in% keys]
  
  
  ### Merge two dataset
  df_demo <- df_demo %>%
    select(-names_to_delete)
  
  df_demo_fte <- df_fte %>%
    left_join(df_demo, by = keys)
  
  
  ### Arrange the merged dataset
  df_demo_fte <- df_demo_fte %>% 
    select(AcademicYear, contains("Name"), 
           CD_code, CountyCode, DistrictCode, SchoolCode, RecID,  
           GenderCode, EthnicGroup, EducationLevel, 
           YearsTeaching, YearsInDistrict, EmploymentStatusCode, 
           StaffType, JobClassification, 
           FTE, FTE_Teaching, FTE_administrative, FTE_PupilServices, 
           everything()) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, RecID)
  
  
  
  ###'######################################################################
  ###' 
  ###' Save data objects
  ###' 
  ###' 
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Save the resulting dataset
  save(df_demo_fte, file = paste0("Staff_Demo_FTE", year_num, "_merged", ".rda"))
  write.dta(df_demo_fte, file = paste0("Staff_Demo_FTE", year_num, "_merged", ".dta"))
  
  
  
  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("Year ", year_num, " completed", "\n"))
  
  
  
  ###'######################################################################
  ###' 
  ###' End for loop
  ###' 
  
}
