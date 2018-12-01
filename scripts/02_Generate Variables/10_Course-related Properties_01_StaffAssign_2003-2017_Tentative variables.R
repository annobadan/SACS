
###'######################################################################
###'
###' Generate Variables
###' 
###' Course-related Properties 2003-2017
###' 
###' - Extract necessary variables: focusing on ELA and Math
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
###' Import datasets
###'
###'

setwd(data_dir)


### Variables from 2003-2011
load(file = "course_properties_2003_2011.rda")
df1 <- df_to_save


### Variables from 2012-2017
load(file = "course_properties_2012_2017.rda")
df2 <- df_to_save



###'######################################################################
###'
###' Extract only necessary categories: ELA, Math
###'
###'

### Check distribution of subject name
tabdf(df1, Assign_Subject)
tabdf(df2, Assign_Subject)


### Filter only English and Math
df1_sub <- df1 %>%
  filter(Assign_Subject %in% c("english", "math"))

df2_sub <- df2 %>%
  filter(Assign_Subject %in% c("English Language Arts", "Mathematics"))


### Assign common subject name
df1_sub$Assign_Subject[df1_sub$Assign_Subject == "english"] <- "ELA"
df1_sub$Assign_Subject[df1_sub$Assign_Subject == "math"] <- "Math"

df2_sub$Assign_Subject[df2_sub$Assign_Subject == "English Language Arts"] <- "ELA"
df2_sub$Assign_Subject[df2_sub$Assign_Subject == "Mathematics"] <- "Math"


### Append the two datasets
df_bind <- bind_rows(df1_sub, df2_sub) %>%
  arrange(CountyCode, DistrictCode, SchoolCode, Assign_Subject, AcademicYear)

df_bind_wide <- df_bind %>%
  gather(key = key, value = value, MN_periods:PCT_NA) %>%
  unite(united_key, key, Assign_Subject) %>%
  spread(key = united_key, value = value)


### Save the dataset
setwd(data_dir)
dualsave(df_bind, "course_properties_2003-2017_ELA_Math.rda")



###'######################################################################
###'
###' Merge with the analysis data
###'
###'

### Student compositions + Teacher compositions 2003-2017
setwd(work_dir)
load(file = "processed_data/df_Teacher_Student_K12_character.rda")
df <- df_to_save


### Merge df_bind_wide
setwd(data_dir)
df <- df %>%
  left_join(df_bind_wide, by = c("CountyCode", "DistrictCode", 
                                 "SchoolCode", "AcademicYear"))


### Save the resulting data
setwd(work_dir)
dualsave(df, "processed_data/df_Teacher_Student_K12_character")


