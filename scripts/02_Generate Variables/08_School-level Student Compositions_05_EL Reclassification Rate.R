
###'######################################################################
###'
###' Generate Variables
###' 
###' School-level Student Compositions 
###' 
###' (5) English Learner
###' 
###' - 5.3. EL Reclassification Rate by School
###' 
###' The total number of EL students redesignated as 
###' fluent-English-proficient (FEP) since the last census. 
###' Includes those who are no longer enrolled at the school 
###' (i.e., graduated or moved).
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character/04_English_Learner/EL_Reclassification")



### Call libraries
library(tidyverse)
library(foreign)
library(rlang)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Loop over years: 2005-06 ~ 2017-18
###'
###' 

setwd(data_dir)

years <- c(sprintf("%02d", seq(5, 17)))

meta_list <- list()


for (i in seq_along(years)){
  
  
  ###'######################################################################
  ###'
  ###' Load dataset: EL by Grade and Language
  ###'
  ###'
  
  year_num <- years[i]
  
  load(file = paste0("reclass", year_num, "_cleaned", ".rda"))
  
  
  
  ###'######################################################################
  ###'
  ###' Dataframe #1. CDS code + EL, FEP, Reclass
  ###'
  ###'
  
  names(df)
  
  ### Select variables
  vars <- names(df)[names(df) %in% c("TOTAL_EL", "RECLASS", 
                                     "EL", "FEP", "Reclass")]
  
  df_temp <- df %>%
    select(ends_with("Code"), vars)

  
  ### Rename variable => for binding rows
  idx <- which(names(df_temp) %in% c("TOTAL_EL"))
  names(df_temp)[idx] <- "EL"
  
  idx <- which(names(df_temp) %in% c("RECLASS"))
  names(df_temp)[idx] <- "Reclass"
  
  
  ### Add Academic Year
  df_Reclass <- df_temp %>%
    mutate(AcademicYear = 2000 + as.numeric(year_num)) %>%
    select(ends_with("Code"), AcademicYear, everything())
  
  
  ### Embed the dataframe in the list
  meta_list[[i]] <- df_Reclass


  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("Year ", year_num, " completed", "\n"))
  
}



###'######################################################################
###'
###' Generate the longitudinal dataset
###'
###'

### Append to the longitudinal dataset
df_bind <- bind_rows(meta_list) %>%
  select(CountyCode, DistrictCode, SchoolCode, AcademicYear, 
         EL, FEP, Reclass) %>%
  arrange(CountyCode, DistrictCode, SchoolCode, AcademicYear)


### Rename variables
df_bind <- df_bind %>%
  rename(N_EL = EL, 
         N_FEP = FEP, 
         N_Reclass = Reclass)



###'######################################################################
###'
###' Merge the prepared EL and FEP data
###'
###'

### Set the data containing parent folder as working directory
setwd("..")


### Import the prepared longitudinal dataset for EL and FEP
load(file = "EL_by_grade_1999_2017.rda")
df_EL <- df_to_save

load(file = "FEP_by_grade_1999_2017.rda")
df_FEP <- df_to_save


### Select variables to merge
match_key <- c("CountyCode", "DistrictCode", "SchoolCode", "AcademicYear")

df_EL_sub <- df_EL %>%
  select(ends_with("Code"), AcademicYear, ends_with("_Total")) %>% 
  group_by(CountyCode, DistrictCode, SchoolCode) %>%
  mutate(N_EL_Total_lag = lag(N_EL_Total))

df_FEP_sub <- df_FEP %>%
  select(ends_with("Code"), AcademicYear, ends_with("_Total"), -Enroll_Total)

df_EL_FEP_sub <- df_EL_sub %>%
  left_join(df_FEP_sub, by = match_key)


### Merge to Reclass dataset
df_merged <- df_EL_FEP_sub %>%
  left_join(df_bind, by = match_key) %>%
  select(match_key, Enroll_Total, 
         N_EL_Total, N_EL, N_EL_Total_lag,  
         N_FEP_Total, N_FEP, 
         N_Reclass, 
         contains("PCT"), everything())



###'######################################################################
###'
###' Calculate EL Reclassification Rate
###'
###'

df <- df_merged %>%
  mutate(PCT_Reclass = 100*(N_Reclass/N_EL_Total), 
         PCT_Reclass_lag = 100*(N_Reclass/N_EL_Total_lag))



###'######################################################################
###'
###' Save the resulting dataframe
###'
###'

dualsave(df, "Reclass_Rate_by_School_2005-2017")

