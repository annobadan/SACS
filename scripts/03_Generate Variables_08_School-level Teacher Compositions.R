
###'######################################################################
###'
###' Generate Variables
###' 
###' School-level Teacher Compositions 2012-2017
###' 
###' - with "Staff_Demo_FTE" data 
###' 
###' 
###' 20181002 JoonHo Lee
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
  ###' Import cleaned data files
  ###'
  ###'
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Staff School FTE
  load(file = paste0("Staff_Demo_FTE", year_num, "_merged", ".rda"))
  df <- df_demo_fte; rm(df_demo_fte)
  classmode(df, everything())
  
  
  ### Prepare dataset filtering only "Teacher"
  tabdf(df, StaffType)
  
  df_Teacher <- df %>%
    filter(StaffType == "Teacher")
  
  tabdf(df_Teacher, StaffType)
  
  
  
  ###'######################################################################
  ###' 
  ###' Data frame #1. "df_StaffType" 
  ###' Total Count/Percent of staffs (RecID) within school
  ###' 
  ###' 
  ###' (1) Total (unduplicated) Count of Staffs within school
  ###' 
  ###' (2) Total Count/Percent of "Teachers" within school
  ###' 
  ###' (3) Total Count/Percent of "Administrators" within school
  ###' 
  ###' (4) Total Count/Percent of "Pupil Services" within school
  ###' 
  ###' 
  
  ### Check distribution
  tabdf(df, StaffType)
  
  
  ### Generate school composition table
  df_StaffType <- school_composition(df, 
                                     var_to_count = RecID, 
                                     factor = StaffType, 
                                     table_name = "Staff Type")
  
  
  
  ###'######################################################################
  ###' 
  ###' Teacher Gender composition
  ###' 
  ###'   
  
  ### Check distribution
  tabdf(df_Teacher, GenderCode)
  
  
  ### Generate school composition table
  df_Teacher_Gender <- school_composition(df_Teacher, 
                                          var_to_count = RecID, 
                                          factor = GenderCode, 
                                          table_name = "Teacher Gender")
  
  
  
  ###'######################################################################
  ###' 
  ###' Teacher Race/Ethnicity composition
  ###' 
  ###'   
  
  ### Check distribution
  tabdf(df_Teacher, EthnicGroup)
  
  
  ### Generate school composition table
  df_Teacher_Race <- school_composition(df_Teacher, 
                                        var_to_count = RecID, 
                                        factor = EthnicGroup, 
                                        table_name = "Teacher Race/Ethnicity")
  
  
  
  ###'######################################################################
  ###' 
  ###' Teacher composition of Educational Level
  ###' 
  ###' Dummy for Master / Doctorate
  ###' 
  ###' D = Doctorate
  ###' S = Special
  ###' V = Master's degree plus 30 or more semester hours
  ###' M = Master's degree
  ###' U = Fifth year within bachelor's degree
  ###' Y = Fifth year induction
  ###' F = Fifth year
  ###' C = Baccalaureate plus 30 or more semester hours
  ###' B = Baccalaureate
  ###' A = Associate degree
  ###' N = Not reported
  ###' 
  ###' 
  
  ### Check distribution
  tabdf(df_Teacher, EducationLevel)
  
  
  ###' Generate factor variable
  ###' The percent of certified teachers with no more than a Bachelor's degree
  df_Teacher <- recode

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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
