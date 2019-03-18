
###'######################################################################
###'
###' Data Management 
###' 
###' School-level Student Poverty FRPM 2004-2017
###' 
###' Generate Longitudinal Datasets (bind_rows)
###' 
###' 
###' 20181008 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character/Student_Poverty_FRPM")


### Call libraries
library(tidyverse)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' (1) Import cleaned datasets
###' (2) Generate uniformly formatted datasets over years
###' (3) Collect data frames to list
###'
###'

### Prepare for loops
years <- sprintf("%02d", seq(04, 17))
list_collect_df <- list()


for (i in seq_along(years)) {
  
  ###'######################################################################
  ###'
  ###' Assign year number
  ###' 
  ###' 
  
  year_num <- years[i]
  
  
  ###'######################################################################
  ###'
  ###' (1) Import the cleaned lists
  ###'
  ###'
  
  setwd(data_dir)
  load(file = paste0("frpm", year_num, "_cleaned", ".rda"))
  
  
  
  ###'######################################################################
  ###'
  ###' (2) Generate uniformly formatted datasets over years
  ###' 
  ###' => Choose only "Adjusted", "Age 5-17" Stats. 
  ###'    These are measures consistent across years
  ###' 
  ###' - CDS Codes
  ###' - AcademicYear
  ###' 
  ###' - Enrollment_5-17
  ###' - N_Free_5-17
  ###' - PCT_Free_5-17
  ###' - N_FRPM_5-17
  ###' _ PCT_FRPM_5-17
  ###'
  ###'
  
  names(df)
  
  if (year_num %in% sprintf("%02d", seq(4, 11))){
    
    df_temp <- df %>%
      mutate(PCT_Free = 100*(Free_Meals/Enrollment)) %>%
      rename(N_Free = Free_Meals, 
             N_FRPM = Total_FRPM) %>%
      select(ends_with("Code"), AcademicYear,  
             Enrollment, N_Free, PCT_Free, N_FRPM, PCT_FRPM)
    
  } else if (year_num %in% c("12")){
    
    names(df) <- gsub("_5-17", "", names(df))
    
    df_temp <- df %>%
      rename(N_FRPM = N_FRPM_Undup) %>%
      select(ends_with("Code"), AcademicYear, 
             Enrollment, N_Free, PCT_Free, N_FRPM, PCT_FRPM)
    
  } else if (year_num %in% c("13")){
    
    names(df) <- gsub("_5-17", "", names(df))
    names(df) <- gsub("_adj", "", names(df))
    
    df_temp <- df %>%
      select(ends_with("Code"), AcademicYear, 
             Enrollment, N_Free, PCT_Free, N_FRPM, PCT_FRPM)
    
  } else if (year_num %in% sprintf("%02d", seq(14, 17))){
    
    names(df) <- gsub("_5-17", "", names(df))
    
    df_temp <- df %>%
      select(ends_with("Code"), AcademicYear, 
             Enrollment, N_Free, PCT_Free, N_FRPM, PCT_FRPM)
    
  }

  
  
  ###'######################################################################
  ###'
  ###' (3) Collect data frames into list
  ###'
  ###'
  
  list_collect_df[[i]] <- df_temp
  
  
  
  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("Year ", year_num, " completed", "\n"))
  
}


###'######################################################################
###' 
###' Append datasets & Save data objects
###' 
###' 

### Bind rows!
df_bind <- bind_rows(list_collect_df)


### Arrange by school
df_bind <- df_bind %>%
  arrange(CountyCode, DistrictCode, SchoolCode, 
          AcademicYear)


### Save the resulting dataset
setwd(data_dir)
save(df_bind, file = paste0("frpm_appended_", "0417", ".rda"))
write.dta(df_bind, file = paste0("frpm_appended_", "0417", ".dta"))
