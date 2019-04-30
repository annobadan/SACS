
###'######################################################################
###'
###' Task    : Postsecondary Preparation 04. Merge SAT, ACT, and AP
###'           
###' Category: Generate Variables 
###' 
###' Data    : Postsecondary Preparation 
###'           Managed SAT, ACT, AP data 1998-1999 ~ 2017-2018
###' 
###' Date    : 2019-04-28
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
data_dir <- c("D:/Data/LCFF/School_Performance/Postsecondary_Preparation")


### Call libraries
library(tidyverse)
library(readxl)
library(Hmisc)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Load the managed longitudinal datasets
###'
###'

setwd(data_dir)

load(file = "SAT_Longitudinal_98-17.rda")
df_SAT <- df_to_save


load(file = "ACT_Longitudinal_98-17.rda")
df_ACT <- df_to_save


load(file = "AP_Longitudinal_98-17.rda")
df_AP <- df_to_save



###'######################################################################
###'
###' Check variable names for merge
###'
###'

### Check variable names
names(df_SAT)
names(df_ACT)
names(df_AP)


### Check intersection
intersect(names(df_SAT), names(df_ACT))
intersect(names(df_SAT), names(df_AP))
intersect(names(df_ACT), names(df_AP))



###'######################################################################
###'
###' Prepare df_SAT for merge
###'
###'

### Filter only school-level data
tabdf(df_SAT, ReportType)

df_SAT <- df_SAT %>%
  filter(!(ReportType %in% c("X", "C", "D"))) %>%
  dplyr::select(-ReportType) %>%
  filter(SchoolCode != 0)


### Add "SAT_" prefix to variable names
names(df_SAT)

match_key <- c("CountyCode", "DistrictCode", "SchoolCode", "Year")

match_key_name <- c(match_key, "CountyName", "DistrictName", "SchoolName")

idx <- !names(df_SAT) %in% match_key_name
  
names(df_SAT)[idx]   

names(df_SAT)[idx] <- paste0("SAT_", names(df_SAT)[idx])   



###'######################################################################
###'
###' Prepare df_ACT for merge
###'
###'

### Filter only school-level data
tabdf(df_ACT, ReportType)

df_ACT <- df_ACT %>%
  filter(!(ReportType %in% c("X", "C", "D"))) %>%
  dplyr::select(-ReportType) %>%
  filter(SchoolCode != 0)


### Add "ACT_" prefix to variable names
names(df_ACT)

match_key <- c("CountyCode", "DistrictCode", "SchoolCode", "Year")

match_key_name <- c(match_key, "CountyName", "DistrictName", "SchoolName")

idx <- !names(df_ACT) %in% match_key_name

names(df_ACT)[idx]   

names(df_ACT)[idx] <- paste0("ACT_", names(df_ACT)[idx])   


### Drop CDS names
df_ACT <- df_ACT %>%
  select(-contains("Name"))




###'######################################################################
###'
###' Prepare df_AP for merge
###'
###'

### Filter only school-level data
tabdf(df_AP, ReportType)

df_AP <- df_AP %>%
  filter(!(ReportType %in% c("X", "C", "D"))) %>%
  dplyr::select(-ReportType) %>%
  filter(SchoolCode != 0)


### Add "AP_" prefix to variable names
names(df_AP)

match_key <- c("CountyCode", "DistrictCode", "SchoolCode", "Year")

match_key_name <- c(match_key, "CountyName", "DistrictName", "SchoolName")

idx <- !names(df_AP) %in% match_key_name

names(df_AP)[idx]   

names(df_AP)[idx] <- paste0("AP_", names(df_AP)[idx])   


### Drop CDS names
df_AP <- df_AP %>%
  select(-contains("Name"))



###'######################################################################
###'
###' Merge df_SAT and df_ACT
###'
###'

### Collect the dataframes as a list
meta_list <- list(df_SAT, df_ACT, df_AP)

df_merge <- reduce(meta_list, full_join_track, 
                   by = match_key)



### Save the resulting dataframe
setwd(data_dir)
dualsave(df_merge, "SAT_ACT_AP_Longitudinal_merged_98-17")
