
###'######################################################################
###'
###' Task    : Merge CBEDS Kindergarten Program Type data with School poverty measures
###'           
###' Category: Visualization
###' 
###' Data    : CBEDS Data about Schools & Districts
###'           2015-16, 2016-17, 2017-18
###'           Focus only on 2017-18
###' 
###' Date    : 2019-02-10
###' 
###' Author  : JoonHo Lee (joonho@berkeley.edu)
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
data_dir <- c("D:/Data/LCFF/Staff_Data/Certificated_Staff/CBEDS Data")


### Call libraries
library(tidyverse)
library(readxl)
library(Hmisc)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Load the cleaned datasets
###'
###'

### (1) Kindergarten Program Type dataset
setwd(data_dir)
load("CBEDS_School_Data_01_Kindergarten_Program_Type_3-year-long.rda")
df_kinder <- df_to_save; rm(df_to_save)


### (2) Ultimate Wide dataset
load("D:/Data/LCFF/df_Ultimate_Merged.rda")
df_wide <- df_to_save; rm(df_to_save)



###'######################################################################
###'
###' Issues with 0 SchoolCode for AcademicYear 2016-17 Data
###' 
###' => Nicely Done!
###'
###' 

### Arrange by "SchoolName", not by "SchoolCode"
df_kinder <- df_kinder %>%
  arrange(CountyName, DistrictName, SchoolName, AcademicYear)


### Replace SchoolCode in AcademicYear 2016-17 with SchoolCode in other years
df_kinder <- df_kinder %>%
  group_by(CountyName, DistrictName, SchoolName) %>%
  mutate(CountyCode = first(CountyCode), 
         DistrictCode = first(DistrictCode), 
         SchoolCode = first(SchoolCode))



###'######################################################################
###'
###' Merge with Ultimate Wide data
###'
###'

### Subset Ultimate Wide dataset
tabdf(df_wide, AcademicYear)

df_wide_to_merge <- df_wide %>%
  filter(AcademicYear >= 2015) %>%
  select(CDSCode:PCT_CALPADS_UPC)

tabdf(df_wide_to_merge, AcademicYear)


### Merge with Kindergarten dataset
df_kinder_merged <- df_kinder %>%
  ungroup() %>%
  select(-contains("Name"), -AcademicYear) %>%
  rename(AcademicYear = Year) %>%
  left_join(df_wide_to_merge, by = c("CountyCode", 
                                     "DistrictCode", 
                                     "SchoolCode", 
                                     "AcademicYear"))



###'######################################################################
###'
###' Save the resulting dataset
###'
###'

setwd(data_dir)

dualsave(df_kinder_merged, 
         "CBEDS_School_Data_01_Kindergarten_Program_Type_3-year-long_merged")


