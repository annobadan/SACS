
###'######################################################################
###'
###' Data Management 
###' 
###' (1) df_Teacher_Student: Student compositions + Teacher compositions 2003-2017
###' 
###' (2) Public K-12 Characteristics 
###' 
###' 
###' Merge and Generate Longitudinal Datasets: 2003-2017
###' 
###' 
###' 20181118 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF")


### Call libraries
library(tidyverse)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import datasets
###'
###'

### Student compositions + Teacher compositions 2003-2017
setwd(work_dir)
load(file = "processed_data/df_Teacher_Student_compositions_0317.rda")
df_compositions <- df_Teacher_Student


### Public K-12 Characteristics datasets
setwd(work_dir)

load(file = "processed_data/Public_K-12_Character/Reclass_Rate_by_School_2005-2017.rda")
df_Reclass <- df_to_save

load(file = "processed_data/Public_K-12_Character/dropouts_managed_wide_1991_2016.rda")
df_Dropouts <- df_to_save

load(file = "processed_data/Public_K-12_Character/graduates_managed_PCT_UC_GRADS_1992_2016.rda")
df_Graduates <- df_to_save

load(file = "processed_data/Public_K-12_Character/susp_managed_wide_2011_2016.rda")
df_Susps <- df_to_save

load(file = "processed_data/Public_K-12_Character/Exps_managed_wide_2011_2016.rda")
df_Exps <- df_to_save

list_K12_character <- list(df_Reclass, 
                           df_Dropouts, 
                           df_Graduates, 
                           df_Susps, 
                           df_Exps)



###'######################################################################
###'
###' Check names
###'
###'

names(df_Teacher_Student)

lapply(list_K12_character, names)



###'######################################################################
###'
###' Subset datasets before merging
###'
###'

match_key <- c("CountyCode", "DistrictCode", "SchoolCode", "AcademicYear")


### (1) EL Reclassification Rate
names(df_Reclass)
head(df_Reclass)

df_Reclass_sub <- df_Reclass %>%
  select(match_key, PCT_Reclass, PCT_Reclass_lag) %>%
  mutate(AcademicYear = AcademicYear - 2000)


### (2) Dropout Rates
names(df_Dropouts)
head(df_Dropouts)
tabdf(df_Dropouts, Category)

df_Dropouts_sub <- df_Dropouts %>%
  filter(Category == "school aggregate") %>%
  select(match_key, PCT_Dropout_Total, PCT_Dropout_TOT) %>%
  mutate(AcademicYear = AcademicYear - 2000)


### (3) A-G Graduates Rates
names(df_Graduates)
head(df_Graduates)

df_Graduates_sub <- df_Graduates %>%
  filter(Category == "school aggregate") %>%
  select(match_key, PCT_UC_GRADS) %>%
  mutate(AcademicYear = AcademicYear - 2000)


### (4) Suspension Rates
names(df_Susps)
head(df_Susps)

df_Susps_sub <- df_Susps %>%
  select(match_key, TA_Rate_Susp) %>%
  mutate(AcademicYear = AcademicYear - 2000)


### (5) Expulsion Rates
names(df_Exps)
head(df_Exps)

df_Exps_sub <- df_Exps %>%
  select(match_key, TA_Rate_Exps) %>%
  mutate(AcademicYear = AcademicYear - 2000)



###'######################################################################
###'
###' Merge to df_Teacher_Students
###'
###'

df_temp <- df_Teacher_Student %>%
  left_join(df_Reclass_sub, by = match_key) %>%
  left_join(df_Dropouts_sub, by = match_key) %>%
  left_join(df_Graduates_sub, by = match_key) %>%
  left_join(df_Susps_sub, by = match_key) %>%
  left_join(df_Exps_sub, by = match_key)


df_K12_character <- df_temp

setwd(work_dir)
dualsave(df_K12_character, paste0("processed_data/df_Teacher_Student_K12_character"))















