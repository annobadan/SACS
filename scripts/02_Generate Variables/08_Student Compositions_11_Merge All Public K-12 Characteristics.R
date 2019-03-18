
###'######################################################################
###'
###' Generate Variables
###' 
###' School-level student compositions
###' 
###' (11) Merge them all!
###' 
###' 
###' 20181202 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character")


### Call libraries
library(tidyverse)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import all appended datasets
###'
###'

### Public School Listing
setwd("D:/Data/LCFF/School_Listing/Public_Schools_and_Districts")
load(file = "pubschls_cleaned_20180923.rda")
df_public <- pubschls


### Public K-12 Characteristics
setwd(data_dir)

load(file = "01_Enrollment/df_Enroll_Joined_9317.rda")
df_Enroll <- df_Enroll_Joined

load(file = "01_Enrollment_Primary_and_Short_Term/enr_short_term_0917.rda")
df_Enroll_Short <- df_bind

load(file = "02_Student_Poverty_FRPM/frpm_appended_0417.rda")
df_FRPM <- df_bind

load(file = "03_CALPADS_Unduplicated_Pupil_Count/CALPADS_UPC_2013_2017.rda")
df_CALPADS_UPC <- df_to_save

load(file = "04_English_Learner/EL_by_grade_1999_2017.rda")
df_EL <- df_to_save

load(file = "04_English_Learner/FEP_by_grade_1999_2017.rda")
df_FEP <- df_to_save

load(file = "04_English_Learner/Reclass_Rate_by_School_2005-2017.rda")
df_Reclass <- df_to_save

load(file = "05_Expulsion_Suspension/Expulsion/Exps_managed_wide_2011_2016.rda")
df_Exp <- df_to_save

load(file = "05_Expulsion_Suspension/Suspension/susp_managed_wide_2011_2016.rda")
df_Susp <- df_to_save

load(file = "06_Truancy_and_Chronic_Absenteeism/Truancy_2012_2015.rda")
df_Truancy <- df_to_save

load(file = "07_Dropout/dropouts_managed_wide_1991_2016.rda")
df_Dropout <- df_to_save

load(file = "08_Graduate/graduates_managed_PCT_UC_GRADS_1992_2016.rda")
df_UCGrads <- df_to_save



###'######################################################################
###'
###' Prepare merge
###' 
###' (1) Restrict AcademicYear to 2003-2017 
###'     - Year format as original year: "2003", not "3"
###' 
###' (2) Rename if duplicated
###'
###'

### (1) Enrollment
names(df_Enroll)
tabdf(df_Enroll, AcademicYear)

df_Enroll_to_merge <- df_Enroll %>%
  filter(AcademicYear >= 2003)
  
tabdf(df_Enroll_to_merge, AcademicYear)


### (2) Enrollment primary and short-term
names(df_Enroll_Short)
tabdf(df_Enroll_Short, AcademicYear)

tabdf(df_Enroll_Short, table)

df_Enroll_Short_to_merge <- df_Enroll_Short %>%
  dplyr::select(-table, -subtotal)


### (3) Student Poverty FRPM
names(df_FRPM)
tabdf(df_FRPM, AcademicYear)

df_FRPM_to_merge <- df_FRPM %>%
  rename(Enrollment_in_FRPM = Enrollment) %>%
  mutate(AcademicYear = 2000 + AcademicYear)


### (4) CALPADS_Unduplicated_Pupil_Count
names(df_CALPADS_UPC)
tabdf(df_CALPADS_UPC, AcademicYear)


### (5) English Learner
names(df_EL)
tabdf(df_EL, AcademicYear)

idx <- grepl("Enroll_", names(df_EL))
names(df_EL) <- gsub("Enroll_", "N_", names(df_EL))
names(df_EL)[idx] <- paste0(names(df_EL)[idx], "_in_EL")

df_EL_to_merge <- df_EL %>%
  filter(AcademicYear >= 2003)


### (6) FEP 
names(df_FEP)
tabdf(df_FEP, AcademicYear)

idx <- grepl("Enroll_", names(df_FEP))
names(df_FEP) <- gsub("Enroll_", "N_", names(df_FEP))
names(df_FEP)[idx] <- paste0(names(df_FEP)[idx], "_in_FEP")

df_FEP_to_merge <- df_FEP %>%
  filter(AcademicYear >= 2003)


### (7) EL Reclassification Rate
names(df_Reclass)  

idx <- grepl("Total", names(df_Reclass))
names(df_Reclass) <- gsub("Total", "Total_in_Rcls", names(df_Reclass))

df_Reclass_to_merge <- df_Reclass %>%
  filter(AcademicYear >= 2003) %>%
  dplyr::select(-N_EL, -N_FEP)


### (8) Expulsion
names(df_Exp)
tabdf(df_Exp, AcademicYear)
tabdf(df_Exp, Aggregate_Level)
tabdf(df_Exp, Charter)

df_Exp_to_merge <- df_Exp %>%
  filter(Aggregate_Level == "School") %>% 
  rename(Charter_in_Exp = Charter) %>%
  dplyr::select(-Aggregate_Level, 
                -CountyName, -DistrictName, -SchoolName)


### (9) Suspension
names(df_Susp)
tabdf(df_Susp, AcademicYear)
tabdf(df_Susp, Aggregate_Level)
tabdf(df_Susp, Charter)

df_Susp_to_merge <- df_Susp %>%
  filter(Aggregate_Level == "School") %>% 
  rename(Charter_in_Susp = Charter) %>%
  dplyr::select(-Aggregate_Level, 
                -CountyName, -DistrictName, -SchoolName)
  

### (10) Truancy
names(df_Truancy)
tabdf(df_Truancy, AcademicYear)
tabdf(df_Truancy, AggLevel)

df_Truancy_to_merge <- df_Truancy %>% 
  filter(AggLevel == "School") %>%
  dplyr::select(-AggLevel) %>%
  rename(N_Census_in_Trncy = Enrollment_Census, 
         N_Cum_in_Trncy = Enrollment_Cum)


### (11) Dropout: from 7 - 12 grades
names(df_Dropout)
tabdf(df_Dropout, AcademicYear)
tabdf(df_Dropout, Category)

category1 <- as.vector(unique(df_Dropout$Category))
category2 <- c("TA", "GM", "GF", "RW", "RH", "RB", "RA", "RF", 
               "RI", "RP", "RT", "RD", "RM") 

df_category <- tibble(category1, category2)

df_Dropout_to_merge <- df_Dropout %>%
  filter(AcademicYear >= 2003) %>%
  left_join(df_category, by = c("Category" = "category1")) %>%
  dplyr::select(-Category) %>%
  gather(key = key, value = value, E_7:PCT_Dropout_Total) %>%
  unite(newvar, category2, key) %>%
  spread(key = newvar, value = value, fill = NA)


### (12) Graduate
names(df_UCGrads)
tabdf(df_UCGrads, AcademicYear)
tabdf(df_UCGrads, Category)

category1 <- as.vector(unique(df_UCGrads$Category))
category2 <- c("TA", "GM", "GF", "RW", "RH", "RB", "RA", "RF", 
               "RI", "RP", "RD", "RM", "RT") 

df_category <- tibble(category1, category2)

df_UCGrads_to_merge <- df_UCGrads %>%
  filter(AcademicYear >= 2003) %>%
  left_join(df_category, by = c("Category" = "category1")) %>%
  dplyr::select(-Category) %>%
  gather(key = key, value = value, N_GRADS:PCT_UC_GRADS) %>%
  unite(newvar, category2, key) %>%
  spread(key = newvar, value = value, fill = NA)

names(df_UCGrads_to_merge)



###'######################################################################
###'
###' Collect as a list
###'
###'

list_collect <- list(df_Enroll_to_merge, 
                     df_Enroll_Short_to_merge, 
                     df_FRPM_to_merge, 
                     df_CALPADS_UPC, 
                     df_EL_to_merge, 
                     df_FEP_to_merge,
                     df_Reclass_to_merge, 
                     df_Exp_to_merge, 
                     df_Susp_to_merge, 
                     df_Truancy_to_merge, 
                     df_Dropout_to_merge, 
                     df_UCGrads_to_merge)



###'######################################################################
###'
###' Merge the multiple dataframes
###'
###'

df_collect <- reduce(list_collect, full_join, 
                     by = c("CountyCode", "DistrictCode", "SchoolCode", 
                            "AcademicYear"))

tabdf(df_collect, AcademicYear)



###'######################################################################
###'
###' Merge with Public School Listing
###'
###'

df_merge <- df_public %>%
  full_join_track(df_collect, 
                  by = c("CountyCode", "DistrictCode", "SchoolCode"), 
                  .merge = TRUE)


###' Investigate unmerged cases from public school listing
###' They are schools closed before 2003
###' => Delete from merged data
tabdf(df_merge, .merge)
names(df_merge)

school_list_only <- df_merge %>%
  filter(.merge == "left_only") %>%
  dplyr::select(SchoolName, SOC, Charter, OpenDate, ClosedDate)


###' Investigate unmerged cases from Public K-12 characters
###' 72% of unmerged cases have SchoolCode == 1, District Office?
###' Keep them in the dataset

K12_character_only <- df_merge %>%
  filter(.merge == "right_only") %>%
  dplyr::select(SchoolCode, SchoolName, SOC, Charter, OpenDate, ClosedDate, 
                Total_Enroll)

tabdf(K12_character_only, SchoolCode)


### Finalize
df_merge_final <- df_merge %>%
  filter(.merge != "left_only") %>%
  arrange(CountyCode, DistrictCode, SchoolCode, AcademicYear)



###'######################################################################
###'
###' Save the resulting dataframe
###'
###'

setwd(data_dir)

dualsave(df_merge_final, "df_Public_K12_Characters_2003_2017")



