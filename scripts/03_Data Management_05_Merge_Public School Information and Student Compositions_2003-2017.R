
###'######################################################################
###'
###' Data Management 
###' 
###' Public School Information + School-level Student Compositions
###' 
###' 
###' Generate Longitudinal Datasets: 2003-2017
###' 
###' 
###' 20181007 JoonHo Lee
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
###' Import the cleaned datasets
###'
###'

### Set data containing working directory
setwd(data_dir)


### (1) Public Schools and Districts
setwd(paste0(data_dir, "/", "School_Listing/Public_Schools_and_Districts"))
load(file = "pubschls_cleaned_20180923.rda")
df_pubschls <- pubschls


### (2) Student School-level Compositions: 01_Enrollment
setwd(paste0(data_dir, "/", "Public_K-12_Character/Enrollment"))

load(file = "school-level_student_composition_01_Enrollment/df_Gender_9317.rda")
df_Gender <- df_bind; rm(df_bind)

load(file = "school-level_student_composition_01_Enrollment/df_Ethnic_9317.rda")
df_Ethnic <- df_bind; rm(df_bind)

load(file = "school-level_student_composition_01_Enrollment/df_Grade_9317.rda")
df_Grade <- df_bind; rm(df_bind)


### (3) Student School-level Compositions: 02_FRPM - Student Poverty
setwd(paste0(data_dir, "/", "Public_K-12_Character/Student_Poverty_FRPM"))
load(file = "frpm_appended_0417.rda")
df_FRPM <- df_bind; rm(df_bind)



###'######################################################################
###'
###' Merge School Panel datasets:
###' 
###' (1) Enrollment 
###' 
###' df_Gender + df_Ethnic + df_Grade
###'
###'

### Check variable names
names(df_Gender)
names(df_Ethnic)
names(df_Grade)


### Collect data frames to merge as a list
list_Enroll <- list(df_Gender, df_Ethnic, df_Grade)


### Merge multiple data frames
df_Enroll_Joined <- list_Enroll %>%
  Reduce(function(df1, df2) full_join(df1, df2, by = c("CountyCode", 
                                                       "DistrictCode", 
                                                       "SchoolCode", 
                                                       "AcademicYear")), .)

names(df_Enroll_Joined)

df_Enroll_Joined <- df_Enroll_Joined %>%
  select(-starts_with("table"), 
         -starts_with("subtotal.")) %>% 
  rename(Total_Enroll = subtotal) %>%
  select(ends_with("Code"), AcademicYear, Total_Enroll, 
         everything())


### Save the resulting data object
setwd(paste0(data_dir, "/", "Public_K-12_Character/Enrollment"))
save(df_Enroll_Joined, file = paste0("df_Enroll_Joined_9317", ".rda"))
write.dta(df_Enroll_Joined, file = paste0("df_Enroll_Joined_9317", ".dta"))



###'######################################################################
###'
###' (2) Merge Student Poverty FRPM to df_Enroll_Joined
###' 
###' (df_Gender + df_Ethnic + df_Grade) + df_FRPM
###' 
###' => Restrict sample to 2003-2017
###' 
###'

### Restrict df_Enroll_Joined: from 1993-2017 => 2003-2017
df_Enroll_Joined_Res <- df_Enroll_Joined %>%
  filter(AcademicYear >= 2003) %>%
  mutate(AcademicYear = AcademicYear - 2000)

tabdf(df_Enroll_Joined_Res, AcademicYear)


### Merge FRPM data
df_student_composition <- df_Enroll_Joined_Res %>%
  full_join(df_FRPM, by = c("CountyCode", "DistrictCode", 
                            "SchoolCode", "AcademicYear"))


###'######################################################################
###'
###' (3) Merge school information
###'
###' (df_Gender + df_Ethnic + df_Grade) + df_FRPM + pubschls
###'
###'

### Merge public school information data
df_student_composition <- df_student_composition %>%
  left_join(pubschls, by = c("CountyCode", "DistrictCode", "SchoolCode"))


### Arrange variables
names(df_student_composition)

df_student_composition <- df_student_composition %>%
  select(CDSCode, CountyCode:SchoolCode, AcademicYear, 
         CountyName:Longitude, everything())


### Save data object
setwd(work_dir)
save(df_student_composition, file = "processed_data/df_student_composition_0317.rda")  
write.dta(df_student_composition, file = "processed_data/df_student_composition_0317.dta") 

