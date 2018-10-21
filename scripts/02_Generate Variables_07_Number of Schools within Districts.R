
###'######################################################################
###'
###' Generate variables
###' 
###' How many schools are within each district?
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
  

  ### Staff School FTE
  load(file = paste0("StaffSchoolFTE", year_num, "_cleaned", ".rda"))
  df_fte <- df; rm(df)
  classmode(df_fte, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Generate table
  ###'
  ###'
  
  ### Count the number of schools within district
  df_Nschools <- df_fte %>%
    group_by(CountyCode, DistrictCode) %>%
    summarise(N_School = n_distinct(SchoolCode))
  
  tabdf(df_Nschools, N_School)
  
  
  

  ###'######################################################################
  ###' 
  ###' Save data objects
  ###' 
  ###' 
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Save the resulting dataset
  save(df, file = paste0("paif", year_num, "_cleaned", ".rda"))
  write.dta(df, file = paste0("paif", year_num, "_cleaned", ".dta"))
  
  
  
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

