
###'######################################################################
###'
###' Generate Variables
###' 
###' School-level Student Compositions 
###' 
###' (5) English Learner
###' 
###' - 5.1. English Learner by Grade
###' 
###' 
###' 20181104 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character/04_English_Learner/EL_by_Grade_Language")
data_dir_enr <- c("D:/Data/LCFF/Public_K-12_Character/01_Enrollment")


### Call libraries
library(tidyverse)
library(foreign)
library(rlang)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import student composition longitudinal data
###'
###'

setwd(data_dir_enr)

load(file = "df_Enroll_Joined_9317.rda")



###'######################################################################
###'
###' Loop over years
###'
###' 

setwd(data_dir)

years <- c(sprintf("%02d", c(99)), sprintf("%02d", seq(0, 17)))

meta_list <- list()


for (i in seq_along(years)){
  
  
  ###'######################################################################
  ###'
  ###' Load dataset: EL by Grade and Language
  ###'
  ###'
  
  year_num <- years[i]
  
  load(file = paste0("elsch", year_num, "_cleaned", ".rda"))
  
  
  
  ###'######################################################################
  ###'
  ###' Generate dataset: Enrollment by Grade
  ###'
  ###'
  
  ### Subset the long dataset
  df_enr_grade <- df_Enroll_Joined %>%
    filter(AcademicYear == if_else(year_num == "99", 
                                   1900 + as.numeric(year_num), 
                                   2000 + as.numeric(year_num))) %>%
    select(AcademicYear, CountyCode, DistrictCode, SchoolCode, 
           "N_KDGN", paste0("N_GR_", seq(1, 12)), 
           starts_with("N_uNGR"), Total_Enroll) %>%
    mutate(N_UNGR = N_UNGR_ELM + N_UNGR_SEC) %>%
    select(-N_UNGR_ELM, -N_UNGR_SEC)
  
  
  ### Reshape to long data format
  names(df_enr_grade) <- gsub("N_", "", names(df_enr_grade))
  df_enr_grade <- df_enr_grade %>%
    rename(Total = Total_Enroll)
  
  df_enr_grade_long <- df_enr_grade %>% 
    gather(key = "Grade", value = "Enroll", 
           KDGN, starts_with("GR"), UNGR, 
           Total) %>%
    mutate(Grade = factor(Grade, 
                          levels = c("KDGN", 
                                     paste0("GR_", seq(1, 12)), 
                                     "UNGR", "Total"))) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, Grade)

  
  
  ###'######################################################################
  ###'
  ###' Dataframe #1. EL by grade
  ###'
  ###'
  
  ### Select only necessary variables
  df_temp <- df %>%
    select(ends_with("Code"), 
           KDGN, starts_with("GR"), UNGR, 
           TOTAL_EL)
  
  
  ### Reshape to long data format
  df_temp <- df_temp %>%
    rename(Total = TOTAL_EL)
  
  df_temp_long <- df_temp %>% 
    gather(key = "Grade", value = "N_EL", 
           KDGN, starts_with("GR"), UNGR, 
           Total) %>% 
    mutate(Grade = factor(Grade, 
                          levels = c("KDGN", 
                                     paste0("GR_", seq(1, 12)), 
                                     "UNGR", "Total"))) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, Grade)
  
  
  ### Summarize to school-level by grade
  df_temp_long_sum <- df_temp_long %>%
    group_by(CountyCode, DistrictCode, SchoolCode, Grade) %>%
    summarise(N_EL = sum(N_EL, na.rm = TRUE))
  
  
  ### Merge with Enrollment by Grade
  df_temp_long_sum <- df_temp_long_sum %>%
    left_join(df_enr_grade_long, 
              by = c("CountyCode", "DistrictCode", "SchoolCode", "Grade")) %>%
    select(AcademicYear, everything())
  
  
  ### Calculate PCT_EL by Grade
  df_EL_Grade_long <- df_temp_long_sum %>%
    mutate(PCT_EL = 100*(N_EL/Enroll)) %>% 
    select(AcademicYear:Grade, Enroll, N_EL, PCT_EL)
  
  
  ### Convert long to wide format
  df_EL_Grade_wide <- df_EL_Grade_long %>%
    gather(key = "quantity", value = "value", Enroll, N_EL, PCT_EL) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, Grade, quantity) %>%
    unite(col = "key", quantity, Grade) %>%
    spread(key, value, fill = NA)
  
  
  ### Save the resulting dataframe
  setwd(data_dir)
  dualsave(df_EL_Grade_long, paste0("EL_by_grade", year_num, "_long"))
  dualsave(df_EL_Grade_wide, paste0("EL_by_grade", year_num, "_wide"))
  
  
  ### Embed the dataframe in the list
  meta_list[[i]] <- df_EL_Grade_wide
  
  
  
  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("Year ", year_num, " completed", "\n"))
  
}



###'######################################################################
###'
###' Append to the longitudinal dataset
###'
###'

df_bind <- bind_rows(meta_list)

df_bind <- df_bind %>%
  select(CountyCode, DistrictCode, SchoolCode, AcademicYear, everything()) %>% 
  arrange(CountyCode, DistrictCode, SchoolCode, AcademicYear)

### Save the resulting dataframe
setwd(data_dir)
dualsave(df_bind, paste0("EL_by_grade", "_1999_2017" ))

