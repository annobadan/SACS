
###'######################################################################
###'
###' Generate Variables
###' 
###' Course-related Properties 
###' 
###' StaffAssign 2003-2011 data
###' 
###' => Generate Teacher-level variables 
###' 
###' 
###' 20181127 JoonHo Lee
###' 20181227 JoonHo Lee - Update with Unified Subject Name
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

years <- c(sprintf("%02d", seq(3, 8)), sprintf("%02d", seq(10, 11)))

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
  ###' StaffAssign 2003-2011 enhanced with AssignmentCodes
  ###' 
  ###'
  
  setwd(data_dir)

  load(file = paste0("assign", year_num, "_enhanced_with_AssignmentCodes.rda"))
  
  df_origin <- df_to_save; rm(df_to_save)
  
  classmode(df_origin, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Filter only Teachers 
  ###' 
  ###' => Remove Administrator, Pupil Services
  ###'
  ###'
  
  df <- df_origin %>%
    filter(Assign_Type == "Teacher")
  
  
  
  ###'######################################################################
  ###'
  ###' (1) df_counts
  ###' 
  ###'  1-1. The number of subjects
  ###'  1-2. The number of courses (Course_Code)
  ###'  1-3. The number of class periods 
  ###' 
  ###' 
  
  df_counts <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, RecID) %>%
    summarise(N_Subject = n_distinct(Unified_Subject), 
              N_Course = n_distinct(Assign_Name), 
              N_Period = n())
  
  ### Check distributions
  tabdf(df_counts, N_Subject)
  tabdf(df_counts, N_Course)
  tabdf(df_counts, N_Period)
  
  
  
  ###'######################################################################
  ###'
  ###' (2) df_subjects: 
  ###' 
  ###'   => squashing multiple "subject categories" per group into one
  ###'      Qualitative information
  ###'
  ###'
  
  df_subjects <- df %>% 
    group_by(CountyCode, DistrictCode, SchoolCode, RecID, Subject_Category) %>%
    summarise(N = n()) %>%
    unite(Subject, Subject_Category, N) %>%
    ungroup() %>%
    group_by(CountyCode, DistrictCode, SchoolCode, RecID) %>%
    summarise(Subjects = paste(Subject, collapse = ", "))
  
  
  
  ###'######################################################################
  ###'
  ###' (3) df_classsize
  ###' 
  ###'   => Average classsize teachers teach
  ###'
  ###'
  
  df_classsize <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, RecID) %>%
    summarise(MNT_classsize = mean(Enroll_total, na.rm = TRUE))
  
  
  
  ###'######################################################################
  ###'
  ###' (4) df_UC_CSU_Approved
  ###' 
  ###'   => Percent of UC_CSU_Approved classes teachers teach
  ###'
  ###'
  
  df_UC_CSU_Approved <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, RecID, UC_CSU_Approved) %>%
    summarise(N = n()) %>%
    mutate(UC_CSU_Approved = recode(UC_CSU_Approved, 
                                    '1' = "Apprvd", 
                                    '0' = "Not_Apprvd")) %>% 
    ungroup() %>%
    group_by(CountyCode, DistrictCode, SchoolCode, RecID) %>%
    mutate(N_Period = sum(N, na.rm = TRUE)) %>%
    spread(key = UC_CSU_Approved, value = N, fill = 0) %>%
    select(-8) %>%
    gather(key = key, value = N, Apprvd, Not_Apprvd) %>%
    mutate(PCT = 100*(N/N_Period)) %>%
    gather(key = quantity, value = value, N, PCT) %>%
    unite(variable, quantity, key, sep = "_") %>%
    spread(key = variable, value = value) %>%
    select(-N_Period)
  
  
  
  ###'######################################################################
  ###'
  ###' (5) df_UCCSU_Requirements
  ###' 
  ###'   => Percent of UCCSU_Requirement classes teachers teach
  ###'
  ###'
  
  df_UCCSU_Requirements <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, RecID, UCCSU_Requirements) %>%
    summarise(N = n()) %>%
    ungroup() %>%
    group_by(CountyCode, DistrictCode, SchoolCode, RecID) %>%
    mutate(N_Period = sum(N, na.rm = TRUE)) %>%
    spread(key = UCCSU_Requirements, value = N, fill = 0) %>%
    gather(key = key, value = N, contains("A-G")) %>%
    mutate(PCT = 100*(N/N_Period)) %>%
    gather(key = quantity, value = value, N, PCT) %>%
    unite(variable, quantity, key, sep = "_") %>%
    spread(key = variable, value = value) %>%
    select(-N_Period)
     
  names(df_UCCSU_Requirements) <- gsub("A-G", "AG", names(df_UCCSU_Requirements))
  
  
  
  ###'######################################################################
  ###'
  ###' (6) df_AP
  ###' 
  ###'    => Percentage of AP classes teachers teach
  ###'
  ###'
  
  ### Check distribution
  tabdf(df, Topic_Name)
  
  
  ### Generate AP dummy indicator
  df <- df %>%
    mutate(AP = if_else(Topic_Name == "AP", 1, 0, missing = NA_real_))
  
  tabdf(df, AP)
  
  
  ### Generate variables
  df_AP <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, RecID, AP) %>%
    summarise(N = n()) %>%
    mutate(AP = recode(AP, '1' = "AP", '0' = "Non_AP")) %>% 
    ungroup() %>%
    group_by(CountyCode, DistrictCode, SchoolCode, RecID) %>%
    mutate(N_Period = sum(N, na.rm = TRUE)) %>%
    spread(key = AP, value = N, fill = 0) %>%
    select(-8) %>%
    gather(key = key, value = N, AP, Non_AP) %>%
    mutate(PCT = 100*(N/N_Period)) %>%
    gather(key = quantity, value = value, N, PCT) %>%
    unite(variable, quantity, key, sep = "_") %>%
    spread(key = variable, value = value) %>%
    select(-N_Period)
  
  
  
  ###'######################################################################
  ###'
  ###' Merge all generated teacher-level summaries
  ###'
  ###'
  
  ### Collect as a list
  list_temp <- list(df_subjects, 
                    df_counts, 
                    df_classsize, 
                    df_UC_CSU_Approved, 
                    df_UCCSU_Requirements, 
                    df_AP)
  
  
  ### Merge!
  key_match <- c("CountyCode", "DistrictCode", "SchoolCode", "RecID")
  df_bind <- reduce(list_temp, left_join, by = key_match)
  
  
  ### Tag AcademicYear
  df_bind <- df_bind %>%
    mutate(AcademicYear = as.numeric(year_num)) %>%
    select(AcademicYear, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Save the resulting dataframe
  ###'
  ###'

  setwd(data_dir)
  
  dualsave(df_bind, paste0("assign", year_num, "_Teacher-level variables"))
  


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

