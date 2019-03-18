
###'######################################################################
###'
###' Generate Variables
###' 
###' ULTIMATE MERGE!
###' 
###' (1) Public K-12 Characteristics (including Student Composition)
###' 
###' (2) Teacher Demographic & Credential composition
###' 
###' (3) Course properties: Class periods and class sizes
###' 
###' (4) Within-school teacher assignment measures
###'  
###' 
###' 20181231 JoonHo Lee
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
###' Load all finalized wide datasets
###'
###'

### (1) Public K-12 Characteristics 2003-2017
temp_path <- file.path(data_dir, "Public_K-12_Character")
setwd(temp_path)

load(file = "df_Public_K12_Characters_2003_2017_with_filter.rda")
df_K12 <- df_to_save


### (2) Teacher Demographic & Credential composition 2003-2017   
temp_path <- file.path(data_dir, "Staff_Data", "Certificated_Staff", "Staff_Demographic")
setwd(temp_path)

load(file = "df_Teacher_Composition_DemoFTE_StaffCred_2003_2017.rda")
df_DemoCred <- df_to_save; rm(df_to_save)


### (3) Course properties: Class periods and class sizes 2003-2017
temp_path <- file.path(data_dir, "Staff_Data", "Certificated_Staff", "Staff_Assignment_and_Course")
setwd(temp_path)

load(file = "df_Class Periods and Sizes_2003-2017_wide.rda")
df_Course <- df_to_save; rm(df_to_save)


### (4) Within-school teacher assignment measures 2012-2017
temp_path <- file.path(data_dir, "Staff_Data", "Certificated_Staff", "Staff_Assignment_and_Course")
setwd(temp_path)

load(file = "Within-School teacher sorting regression_04_Estimates_Wide_Final.rda")
df_Within <- df_to_save; rm(df_to_save)



###'######################################################################
###'
###' Prepare merge
###'
###'

### Match Key
match_key <- c("CountyCode", "DistrictCode", "SchoolCode", "AcademicYear")


### Check key variable names
classmode(df_K12, everything())
classmode(df_DemoCred, everything())
classmode(df_Course, everything())
classmode(df_Within, everything())


### Check the values of AcademicYear
tabdf(df_K12, AcademicYear)
tabdf(df_DemoCred, AcademicYear)
tabdf(df_Course, AcademicYear)
tabdf(df_Within, AcademicYear)


### Change AcademicYear values
df_DemoCred <- df_DemoCred %>%
  mutate(AcademicYear = 2000 + AcademicYear)


df_Course <- df_Course %>%
  mutate(AcademicYear = 2000 + AcademicYear)


df_Within <- df_Within %>%
  mutate(AcademicYear = 2000 + as.numeric(AcademicYear))



###'######################################################################
###'
###' Merge at once
###'
###'

# list_collect <- list(df_K12, 
#                      df_DemoCred, 
#                      df_Course, 
#                      df_Within)
# 
# df_collect <- reduce(list_collect, full_join_track, 
#                      by = c("CountyCode", "DistrictCode", "SchoolCode", 
#                             "AcademicYear"))
# 
# tabdf(df_collect, AcademicYear)



###'######################################################################
###'
###'  (1) df_K12 + df_DemoCred
###'
###'

### Delete .merge
df_K12 <- df_K12 %>%
  dplyr::select(-.merge)


### Full join
df_K12_DemoCred <- full_join_track(df_K12, df_DemoCred, by = match_key, .merge = TRUE)

tabdf(df_K12_DemoCred, .merge)


###' Look into unmerged cases (1) df_DemoCred only
###' 96.3% of unmatched schools are SchoolCode == 0, i.e., "District Office"
###' The recent years have more missings
###' OK to remove
df_right_only <- df_K12_DemoCred %>%
  filter(.merge == "right_only")

tabdf(df_right_only, SchoolCode)  

tabdf(df_right_only %>% filter(SchoolCode != 0), AcademicYear)


###' Look into unmerged cases (2) df_K12 only
###' 1% of unmerged cases are "District Office" (SchoolCode == 0)
###' 60% of unmerged cases are "Non-public, Non-Secterian Schools"
###' 50% of unmerged public schools are "Nontraditional schools"
###'   e.g. District Community Day Schools, Opportunity Schools etc. 
###' The rest 40% are school which don't have teacher demographic, credential data

df_left_only <- df_K12_DemoCred %>%
  filter(.merge == "left_only")

tabdf(df_left_only, SchoolCode)
tabdf(df_left_only, AcademicYear)
tabdf(df_left_only, DOC)
tabdf(df_left_only, EIL)
tabdf(df_left_only, SOC)

df_left_only_schools <- df_left_only %>%
  filter(SchoolCode != 0 & SchoolCode != 1)

tabdf(df_left_only_schools, SchoolCode)
tabdf(df_left_only_schools, AcademicYear)
tabdf(df_left_only_schools, DOC)
tabdf(df_left_only_schools, EIL)
tabdf(df_left_only_schools, SOC)


### Remove only df_DemoCred Only cases, i.e., District Office (SchoolCode == 1)
df_K12_DemoCred_to_merge <- df_K12_DemoCred %>%
  filter(.merge != "right_only") %>%
  dplyr::select(-.merge)



###'######################################################################
###'
###' (2) df_K12_DemoCred_to_merge + df_Course
###'
###'

### Full join
df_K12_DemoCred_Course <- full_join_track(df_K12_DemoCred_to_merge, 
                                          df_Course, 
                                          by = match_key, 
                                          .merge = TRUE)

tabdf(df_K12_DemoCred, .merge)


###' Look into unmerged cases (1) df_Course only
###' 96.2% of unmatched schools are SchoolCode == 0, i.e., "District Office"
###' 0.3% are SchoolCode == 1, i.e., "Nonpublic, nonsectarian schools"
###' The recent years have more missings
###' OK to remove
df_right_only <- df_K12_DemoCred_Course %>%
  filter(.merge == "right_only")

tabdf(df_right_only, SchoolCode)  

tabdf(df_right_only %>% filter(SchoolCode != 0), AcademicYear)


###' Look into unmerged cases (2) df_K12_DemoCred only
###' 
###' 1% of unmerged cases are "District Office" (SchoolCode == 0)
###' 26.5% of unmerged cases are "Non-public, Non-Secterian Schools" (SchoolCode == 1)
###' 50% of cases missing in df_Course are 2009 data, which are not available. 
###' 50% of unmerged public schools are "Nontraditional schools"
###'   e.g. District Community Day Schools, Opportunity Schools etc. 
###' The rest 40% are schools which don't have course data

df_left_only <- df_K12_DemoCred_Course %>%
  filter(.merge == "left_only")

tabdf(df_left_only, SchoolCode)
tabdf(df_left_only, AcademicYear)
tabdf(df_left_only, DOC)
tabdf(df_left_only, EIL)
tabdf(df_left_only, SOC)

df_left_only_schools <- df_left_only %>%
  filter(SchoolCode != 0 & SchoolCode != 1 & AcademicYear != 2009)

tabdf(df_left_only_schools, SchoolCode)
tabdf(df_left_only_schools, AcademicYear)
tabdf(df_left_only_schools, DOC)
tabdf(df_left_only_schools, EIL)
tabdf(df_left_only_schools, SOC)


### Remove only df_Course Only cases, i.e., District Office (SchoolCode == 1)
df_K12_DemoCred_Course_to_merge <- df_K12_DemoCred_Course %>%
  filter(.merge != "right_only") %>%
  dplyr::select(-.merge)



###'######################################################################
###'
###' (3) df_K12_DemoCred_Course + Within
###'
###'
###'

### Clear memory
rm(df_Course, 
   df_K12, 
   df_K12_DemoCred, 
   df_K12_DemoCred_to_merge, 
   df_K12_DemoCred_Course, 
   df_left_only, 
   df_left_only_schools, 
   df_right_only)


### Full join
df_K12_DemoCred_Course_Within <- full_join_track(df_K12_DemoCred_Course_to_merge, 
                                                 df_Within, 
                                                 by = match_key, 
                                                 .merge = TRUE)

tabdf(df_K12_DemoCred_Course_Within, .merge)


### Look into unmerged cases: (1) right_only
df_right_only <- df_K12_DemoCred_Course_Within %>%
  filter(.merge == "right_only")


### Remove df_Within Only cases
df <- df_K12_DemoCred_Course_Within %>%
  filter(.merge != "right_only") %>%
  dplyr::select(-.merge)



###'######################################################################
###'
###' Save the resulting dataset
###'
###'

setwd(data_dir)
dualsave(df, "df_Ultimate_Merged")

