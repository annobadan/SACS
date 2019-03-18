
###'######################################################################
###'
###' Generate Variables
###' 
###' Course-related Properties: 
###' 
###' Course Enrollment 2012-2017 
###' 
###' - Collapse to the group-unique rows
###' 
###'   < Group by the following six identifiers > :
###'   
###'   CountyCode, DistrictCode, SchoolCode, 
###'   
###'   ClassID, CourseCode, ClassCourseID   
###'         
###' 
###' 20181204 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Staff_Data/Certificated_Staff/Staff_Assignment_and_Course")


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

years <- sprintf("%02d", seq(12, 17))


for (i in 2:6) {

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
  
  setwd(data_dir)
  
  ### Load CourseEnrollment data
  load(file = paste0("CourseEnrollment", year_num, "_cleaned", ".rda"))
  names(df)
  
  classmode(df, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' (1) Gender Composition
  ###'
  ###'
  
  df_Gender <- df %>%
    filter(!is.na(GenderCode)) %>%
    group_by(CountyCode, DistrictCode, SchoolCode, 
             ClassID, CourseCode, ClassCourseID, GenderCode) %>%
    summarise(N = sum(EnrollTotal, na.rm = TRUE)) %>% 
    spread(key = GenderCode, value = N, fill = 0)
  
  
  
  ###'######################################################################
  ###'
  ###' (2) Racial + EL Composition
  ###'
  ###'
  
  df_RaceEL <- df %>% 
    gather(key = key, value = value, starts_with("Enroll")) %>%
    group_by(CountyCode, DistrictCode, SchoolCode, 
             ClassID, CourseCode, ClassCourseID, key) %>% 
    summarise(N = sum(value, na.rm = TRUE)) %>%
    spread(key = key, value = N)
  
  
  
  ###'######################################################################
  ###'
  ###' (3) Grade Composition
  ###'
  ###'

  df_Grade <- df %>%
    filter(!is.na(GradeLevelCode)) %>%
    group_by(CountyCode, DistrictCode, SchoolCode, 
             ClassID, CourseCode, ClassCourseID, GradeLevelCode) %>%
    summarise(N = sum(EnrollTotal, na.rm = TRUE)) %>% 
    spread(key = GradeLevelCode, value = N, fill = 0)
  
  
  
  ###'######################################################################
  ###'
  ###' Rename variables before merge
  ###'
  ###'
  
  ### Rename variables 1. df_Gender
  names(df_Gender)
  oldname <- c("female", "male")
  newname <- paste0("N_", oldname)
  
  df_Gender <- df_Gender %>%
    rename_at(.vars = oldname, .funs = ~ newname)
  
  
  ### Rename variables 2. df_RaceEL
  names(df_RaceEL)
  names(df_RaceEL) <- gsub("Enroll", "N_", names(df_RaceEL))
  
  
  ### Rename variables 3. df_Grade
  names(df_Grade)
  oldname <- levels(df$GradeLevelCode)
  newname <- paste0("N_GR_", oldname)
  
  df_Grade <- df_Grade %>%
    rename_at(.vars = oldname, .funs = ~ newname)
  
  
  
  ###'######################################################################
  ###'
  ###' Merge the generated three dataframes
  ###'
  ###'
  
  ### Keys for matching
  match_key <- c("CountyCode", "DistrictCode", "SchoolCode", 
                 "ClassID", "CourseCode", "ClassCourseID")
  
  
  ### Collect as a list
  list_temp <- list(df_Gender, df_RaceEL, df_Grade)
  
  
  ### Merge!
  df_merge <- reduce(list_temp, full_join, by = match_key)
  

  
  ###'######################################################################
  ###'
  ###' Generate percentage variables
  ###'
  ###'
  
  ### Set variable order
  vars <- c("male", "female", "EL", 
            "White", "Hispanic", "AfrAm", "Asian", 
            "Filipino", "PacIsl", "AmInd", "TwoOrMore", "NoEthRptd", 
            paste0("GR_", levels(df$GradeLevelCode)))
  
  levels_vec <- c(paste0("N_", vars), paste0("PCT_", vars)) 
  
  
  df_wide <- df_merge %>% 
    gather(key = key, value = N, paste0("N_", vars)) %>%
    mutate(key = gsub("N_", "", key), 
           PCT = 100*(N/N_Total)) %>%
    gather(key = quantity, value = value, N, PCT) %>%
    unite(variable, quantity, key) %>% 
    mutate(variable = factor(variable, levels = levels_vec)) %>%
    spread(key = variable, value = value)
  
  
  
  ###'######################################################################
  ###'
  ###' Save the resulting dataframe 
  ###'
  ###'
  
  setwd(data_dir)
  
  dualsave(df_wide, paste0("CourseEnrollment", year_num, "_managed"))
  

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
