
###'######################################################################
###'
###' Data Management 
###' 
###' School-level Student Compositions 1993-2017
###' 
###' Generate Longitudinal Datasets (bind_rows)
###' 
###' 
###' 20181007 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character/Enrollment/school-level_student_composition_01_Enrollment")


### Call libraries
library(tidyverse)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import the cleaned lists
###'
###'

### Set data containing working directory
setwd(data_dir)


### Loop over years
years <- c(sprintf("%02d",seq(93, 99)), sprintf("%02d",seq(00, 17)))

for (i in seq_along(years)){
  
  ### Assign year
  year_num <- years[i]
  
  ### Load the cleaned list
  load(file = paste0("list_school-level_student_composition_01_Enrollment", 
                     year_num, ".rda"))
  
  ### Assign year-specified list name
  assign(paste0("list", year_num), list_collect_df)
  
}



###'######################################################################
###'
###' Append data frames over years
###'
###'

years <- c(sprintf("%02d",seq(93, 99)), sprintf("%02d",seq(00, 17)))
df_names <- names(list17)

for (j in seq_along(df_names)){
  
  ### Prepare empty list to collect data frames
  meta_list <- list()
  
  
  ### Collect variable specific dataframes over years
  for (i in seq_along(years)){
    
    # Extract list by year
    year_num <- years[i]
    list_temp <- get(paste0("list", year_num))
    
    # Extract dataframe by df_name
    df_name <- df_names[j]
    df <- list_temp[[df_name]]
    
    # Append to pre-defined dataframe
    meta_list[[i]] <- df
  }
  
  
  ### Bind rows!
  df_bind <- bind_rows(meta_list)
  
  ### Arrange by school
  df_bind <- df_bind %>%
    mutate(AcademicYear = if_else(AcademicYear > 90, 
                                  AcademicYear + 1900, 
                                  AcademicYear + 2000)) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, 
            table, AcademicYear)
  
  ### Assign name
  assign(paste0(df_name, "_9317"), df_bind)
  
  ### Save the resulting dataframe
  setwd(data_dir)
  save(df_bind, file = paste0(df_name, "_9317", ".rda"))
  write.dta(df_bind, file = paste0(df_name, "_9317", ".dta"))
}

