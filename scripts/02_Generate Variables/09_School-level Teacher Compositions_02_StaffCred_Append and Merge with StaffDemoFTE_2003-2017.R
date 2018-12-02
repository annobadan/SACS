
###'######################################################################
###'
###' Generate Variables
###' 
###' School-level Teacher Compositions 2003-2017
###' 
###' - Merge df_teacher_compositions_0317 with "Staff_Cred_0317" data
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
data_dir <- c("D:/Data/LCFF/Staff_Data/Certificated_Staff/Staff_Demographic")


### Call libraries
library(tidyverse)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import datasets to merge
###'
###'

setwd(work_dir)

### df_student_compositions 2003-2017
load(file = "processed_data/df_student_composition_0317.rda")
names(df_student_composition)


### df_teacher_compositions 2003-2017 all merged one
load(file = "processed_data/df_teacher_composition_0317.rda")
df_teacher_composition <- df_to_save


### df_StaffCred => contains only 2003-2008 and 2012-2017 Staff Credential data
load(file = "processed_data/df_StaffCred_2003-2008 and 2012-2017.rda")
df_Cred <- df_to_save



###'######################################################################
###'
###' Merge the imported datasets
###'
###'

### df_teacher_composition + df_StaffCred
df_teacher_composition_augmented <- full_join(df_teacher_composition, 
                                              df_Cred, 
                                              by = c("CountyCode", "DistrictCode", 
                                                     "SchoolCode", "AcademicYear"))


### df_student_composition + df_teacher_composition_augmented
df_student_teacher_composition <- full_join(df_student_composition, 
                                            df_teacher_composition_augmented, 
                                            by = c("CountyCode", "DistrictCode", 
                                                   "SchoolCode", "AcademicYear"))


### Save the merged datasets
setwd(data_dir)
dualsave(df_teacher_composition_augmented, "df_teacher_composition_Cred_2003_2017")
dualsave(df_student_teacher_composition, "df_student_teacher_composition_2003_2017")


