
###'######################################################################
###'
###' Generate Variables
###' 
###' School-level Student Compositions 
###' 
###' (7) Truancy or Chronic Absenteeism 
###' 
###' - 6.1. Truancy
###' 
###' 
###' 20181108 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character/06_Truancy_and_Chronic_Absenteeism")


### Call libraries
library(tidyverse)
library(foreign)
library(rlang)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Loop over years
###'
###' 

setwd(data_dir)

years <- c(sprintf("%02d", seq(12, 15)))

meta_list <- list()


for (i in seq_along(years)){
  
  
  ###'######################################################################
  ###'
  ###' Load dataset: FEP by Grade and Language
  ###'
  ###'
  
  year_num <- years[i]
  
  load(file = paste0("enr", year_num, "_cleaned", ".rda"))
  
  
  
  ###'######################################################################
  ###'
  ###' Academic Year
  ###'
  ###'
  
  df$AcademicYear <- 2000 + as.numeric(year_num)
  
  
  
  ###'######################################################################
  ###'
  ###' Arrange row and column orders
  ###'
  ###'
  
  names(df)
  
  df <- df %>%
    select(-Year, -CDS, -Name) %>% 
    select(AggLevel, ends_with("Code"), AcademicYear, 
           starts_with("Enrollment"),  
           N_Truant, Truancy_Rate) %>%
    arrange(AggLevel, CountyCode, DistrictCode, SchoolCode)
  
  
  
  ###'######################################################################
  ###'
  ###' Embed into the list
  ###'
  ###'
  
  meta_list[[i]] <- df
  
  
  
  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("Year ", year_num, " completed", "\n"))
  
}



###'######################################################################
###'
###' Append to longitudinal dataset
###'
###'

df_bind <- bind_rows(meta_list)

classmode(df_bind, everything())

df_bind <- df_bind %>%
  select(AggLevel, CountyCode, DistrictCode, SchoolCode, AcademicYear, 
         everything()) %>% 
  arrange(AggLevel, CountyCode, DistrictCode, SchoolCode, AcademicYear)


### Save the resulting dataframe
setwd(data_dir)
dualsave(df_bind, paste0("Truancy", "_2012_2015" ))

