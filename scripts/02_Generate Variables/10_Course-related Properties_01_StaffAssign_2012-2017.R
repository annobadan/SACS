
###'######################################################################
###'
###' Generate Variables
###' 
###' Course-related Properties 2012-2017
###' 
###' - with "StaffAssign" data
###' 
###' 
###' 20181127 JoonHo Lee
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

meta_list <- list()


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
  
  setwd(data_dir)
  
  ### Load StaffAssign data
  load(file = paste0("StaffAssign", year_num, "_cleaned", ".rda"))
  names(df)
  
  classmode(df, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Filter only Teachers 
  ###' 
  ###' => Remove Administrator, Pupil Services
  ###'
  ###'
  
  tabdf(df, Assign_Type)
  
  df_temp_teacher <- df %>%
    filter(Assign_Type == "Teacher") %>%
    arrange(CountyCode, DistrictCode, SchoolCode, RecID)
  
  tabdf(df_temp_teacher, Assign_Type)
  
  
  
  ###'######################################################################
  ###'
  ###' Teacher- & School-level summaries:
  ###'
  ###' (1) Number of class periods assigned to teachers
  ###'  
  ###'  1.1. Not by subject: 
  ###'  - Reflect teachers cross-subject assignment
  ###'  - Individual teacher's working conditions
  ###'  
  ###'  1.2. By subject
  ###'  - working conditions by each subject division within school
  ###'  - ELA, Math, and others 
  ###'
  ###'
  
  ### Not by subject
  df_periods_teacher <- df_temp_teacher %>%
    group_by(CountyCode, DistrictCode, SchoolCode, RecID) %>%
    summarise(N_periods = n())
  
  tabdf(df_periods_teacher, N_periods)
  
  
  ### By subject
  df_periods_teacher_subj <- df_temp_teacher %>%
    group_by(CountyCode, DistrictCode, SchoolCode, Assign_Subject, RecID) %>%
    summarise(N_periods = n())
  
  tabdf(df_periods_teacher_subj, N_periods)
  
  
  ### School-level summary
  df_periods_school <- df_periods_teacher %>%
    group_by(CountyCode, DistrictCode, SchoolCode) %>%
    summarise(MN_periods = mean(N_periods, na.rm = TRUE)) %>%
    mutate(Assign_Subject = "Total")
  
  df_periods_school_subj <- df_periods_teacher_subj %>%
    group_by(CountyCode, DistrictCode, SchoolCode, Assign_Subject) %>%
    summarise(MN_periods = mean(N_periods, na.rm = TRUE))
  
  df_periods_school_bind <- bind_rows(df_periods_school_subj, df_periods_school) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, Assign_Subject)
  
  

  ###'######################################################################
  ###'
  ###' School-level summaries:
  ###'
  ###' (3) Percentage of A-G Classes: UCCSU_Requirements
  ###'  
  ###'  3.1. Not by subject: 
  ###'  
  ###'  3.2. By subject
  ###'  - ELA, Math, and others 
  ###'
  ###'
  
  ### UCCSU_Requirements: Not by subject
  df_AG_school_require <- df_temp_teacher %>%
    group_by(CountyCode, DistrictCode, SchoolCode, UCCSU_Requirements) %>%
    summarise(N_classes = n())
  
  df_AG_school_require$UCCSU_Requirements <- 
    gsub("A-G", "AG", df_AG_school_require$UCCSU_Requirements)
  
  df_AG_school_require_wide <- df_AG_school_require %>%
    group_by(CountyCode, DistrictCode, SchoolCode) %>%
    mutate(N_classes_total = sum(N_classes, na.rm = TRUE), 
           PCT = 100*(N_classes/N_classes_total)) %>%
    select(-N_classes_total) %>% 
    rename(N = N_classes) %>%
    gather(key = key, value = value, N, PCT) %>%
    unite(AG_require, key, UCCSU_Requirements) %>%
    spread(key = AG_require, value = value, fill = 0) %>%
    mutate(Assign_Subject = "Total")
  
  
  ### UCCSU_Requirements: By subject
  df_AG_school_require_subj <- df_temp_teacher %>%
    group_by(CountyCode, DistrictCode, SchoolCode, Assign_Subject, UCCSU_Requirements) %>%
    summarise(N_classes = n())
  
  df_AG_school_require_subj$UCCSU_Requirements <- 
    gsub("A-G", "AG", df_AG_school_require_subj$UCCSU_Requirements)
  
  df_AG_school_require_subj_wide <- df_AG_school_require_subj %>%
    group_by(CountyCode, DistrictCode, SchoolCode, Assign_Subject) %>%
    mutate(N_classes_total = sum(N_classes, na.rm = TRUE), 
           PCT = 100*(N_classes/N_classes_total)) %>%
    select(-N_classes_total) %>% 
    rename(N = N_classes) %>%
    gather(key = key, value = value, N, PCT) %>%
    unite(AG_require, key, UCCSU_Requirements) %>%
    spread(key = AG_require, value = value, fill = 0)
  
  
  ### School-level summary: bind rows
  df_AG_school_require_wide_bind <- bind_rows(df_AG_school_require_subj_wide, 
                                              df_AG_school_require_wide) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, Assign_Subject)
  
  
  
  ###'######################################################################
  ###'
  ###' School-level summaries:
  ###'
  ###' (4) Percentage of AP Classes
  ###'  
  ###'  3.1. Not by subject: 
  ###'  
  ###'  3.2. By subject
  ###'  - ELA, Math, and others 
  ###'
  ###'
  
  ### Check distributions
  tabdf(df_temp_teacher, AP)
  
  
  ### Percentage of AP Classes: Not by subject
  df_AP_school <- df_temp_teacher %>% 
    group_by(CountyCode, DistrictCode, SchoolCode) %>%
    summarise(N_classes = n(), 
              N_AP = sum(AP, na.rm = TRUE),
              PCT_AP = 100*(N_AP/N_classes)) %>%
    mutate(Assign_Subject = "Total")
  
  
  ### Percentage of AP Classes: By subject
  df_AP_school_subj <- df_temp_teacher %>% 
    group_by(CountyCode, DistrictCode, SchoolCode, Assign_Subject) %>%
    summarise(N_classes = n(), 
              N_AP = sum(AP, na.rm = TRUE),
              PCT_AP = 100*(N_AP/N_classes))
  
  
  ### School-level summaries
  df_AP_school_bind <- bind_rows(df_AP_school_subj, df_AP_school) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, Assign_Subject)
  
  
  
  ###'######################################################################
  ###'
  ###' Merge all generated school-level summaries
  ###'
  ###'
  
  ### Collect as a list
  list_temp <- list(df_periods_school_bind, 
                    df_AG_school_require_wide_bind, 
                    df_AP_school_bind)
  
  
  ### Merge!
  key_match <- c("CountyCode", "DistrictCode", "SchoolCode", "Assign_Subject")
  df_bind <- reduce(list_temp, left_join, by = key_match)
  
  
  ### Tag AcademicYear
  df_bind <- df_bind %>%
    mutate(AcademicYear = as.numeric(year_num)) %>%
    select(AcademicYear, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Embed into the meta list
  ###'
  ###'
  
  meta_list[[i]] <- df_bind
  
  
  
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



###'######################################################################
###'
###' Bind rows
###'
###'

df_meta_bind <- bind_rows(meta_list) %>%
  arrange(CountyCode, DistrictCode, SchoolCode, Assign_Subject, AcademicYear) %>%
  select(CountyCode, DistrictCode, SchoolCode, Assign_Subject, AcademicYear, everything())

setwd(data_dir)
dualsave(df_meta_bind, file = "course_properties_2012_2017")






