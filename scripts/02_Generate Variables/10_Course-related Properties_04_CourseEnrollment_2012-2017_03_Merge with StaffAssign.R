
###'######################################################################
###'
###' Generate Variables
###' 
###' Course-related Properties: 
###' 
###' => Merge (CoursesTaught + CourseEnrollment) with StaffAssign
###'
###' (1) CoursesTaught + CourseEnrollment 2012-2017 
###' (2) StaffAssign
###'  
###'   < Merge by the following five identifiers > :
###'   
###'   CountyCode, DistrictCode, SchoolCode, 
###'   
###'   ClassID, CourseCode
###'   
###'   => ClassCourseID is not available   
###'         
###' 
###' 20181204 JoonHo Lee
###' 20181211 JoonHo Lee - Update code with tag_ID_miss_dups function
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
###' Loop over years
###'
###'

years <- sprintf("%02d", seq(12, 17))


for (i in seq_along(years)) {
  
  ###'######################################################################
  ###'
  ###' Assign year number
  ###' 
  ###' 
  
  year_num <- years[i]
  
  
  
  ###'######################################################################
  ###'
  ###' Import precleaned dataset
  ###'
  ###'
  
  setwd(data_dir)
  
  ### Load the CoursesTaught with CourseEnrollment Data
  load(file = paste0("CoursesTaught", year_num, "_merged with CourseEnrollment", ".rda"))
  df_course <- df_to_save; rm(df_to_save)
  classmode(df_course, everything())
  
  
  ### Load the raw StaffAssign data
  load(file = paste0("StaffAssign", year_num, "_cleaned", ".rda"))
  df_staff <- df; rm(df)
  classmode(df_staff, everything())
  
  
  ### Generate key for matching
  names(df_course)
  names(df_staff)
  
  table(is.na(df_staff$CourseCode))
  table(is.na(df_staff$Assign_Code))

  match_key <- c("CountyCode", "DistrictCode", "SchoolCode", 
                 "ClassID", "CourseCode")
  
  
  
  ###'######################################################################
  ###'
  ###' Filter only Teachers
  ###'
  ###' => Because only teachers have course information
  ###'
  ###'
  
  tabdf(df_staff, Assign_Type)  # About 95% of cases are teachers for 2012 data
  
  df_teacher <- df_staff %>%
    filter(Assign_Type == "Teacher")
  
  
  
  ###'######################################################################
  ###'
  ###' Check missing and duplicated ID
  ###' 
  ###' (1) CoursesTaught + CourseEnrollment
  ###'
  ###' => All unique & valid ID combinations
  ###'
  ###'
  
  ### Generate tag
  df_course <- tag_ID_miss_dups(df_course, match_key = match_key)
  
  
  
  ###'######################################################################
  ###'
  ###' Check missing and duplicated ID
  ###' 
  ###' (2) StaffAssign
  ###'
  ###'
  
  ### Generate tag
  df_teacher <- tag_ID_miss_dups(df_teacher, match_key = match_key)
  
  tabdf(df_teacher, tag)
  
  
  ###' Look into missing IDs
  ###' => Non-instructional Teachers
  ###' => All "Administration" such as Resource teacher (50%)
  ###' Remove when merge with CoursesTaught + CourseEnrollment data
  df_teacher_missing <- df_teacher %>%
    filter(tag == "ID_missing")
  
  tabdf(df_teacher_missing, Assign_Subject)
  tabdf(df_teacher_missing, Assign_Name)
  
  
  ###' Look into duplicated IDs
  ###' => Class & Courses that multiple teachers are assigned
  ###' Do not remove
  df_teacher_dups <- df_teacher %>%
    filter(tag == "ID_duplicated") %>%
    arrange(CountyCode, DistrictCode, SchoolCode, 
            ClassID, CourseCode)
  
  tabdf(df_teacher_dups, Assign_Subject)
  tabdf(df_teacher_dups, Assign_Name)
  
  setwd(data_dir)
  write.csv(df_teacher_dups, 
            file = paste0("StaffAssign", year_num, "_cases with duplicated IDs", ".csv"))
  
  
  
  ###'######################################################################
  ###'
  ###' Prepare StaffAssign data to merge with CoursesTaught + CourseEnrollment
  ###'
  ###'
  
  df_teacher_to_merge <- df_teacher %>%
    filter(tag != "ID_missing") %>%
    dplyr::select(CountyCode, DistrictCode, SchoolCode, ClassID, CourseCode, 
                  RecID, EstimatedFTE, tag)
  
  tabdf(df_teacher_to_merge, tag)
  
  
  
  ###'######################################################################
  ###'
  ###' Merge!
  ###' 
  ###' (1) df_course: CoursesTaught + CourseEnrollment
  ###' (2) df_teacher_to_merge: StaffAssign
  ###'
  ###'
  
  ### Remove tags
  names(df_course)
  names(df_teacher_to_merge)
  
  df_course <- df_course %>%
    dplyr::select(-.merge, -tag.x, -tag.y, -tag)
  
  
  ### Merge!
  df_merged <- df_course %>%
    full_join_track(df_teacher_to_merge, 
                    by = match_key, .merge = TRUE)
  
  tabdf(df_merged, .merge)
  
  ###' Look into unmerged cases (1) Course data only
  ###' Staff Data are not available for about 1.6% cases
  ###' Statewide Educator Identifier (SEID) Indicator == 0
  ###' => Class/Course that staff-level data are not available 
  df_unmerged_courseonly <- df_merged %>%
    filter(.merge == "left_only")
  
  tabdf(df_unmerged_courseonly, SEID_Indicator)
  tabdf(df_unmerged_courseonly %>% filter(SEID_Indicator == 1), Assign_Subject)
  tabdf(df_unmerged_courseonly %>% filter(SEID_Indicator == 1), Assign_Name)
  
  
  ###' Look into unmerged cases (2) Staff data only
  df_unmerged_staffonly <- df_merged %>%
    filter(.merge == "right_only")
  
  tabdf(df_unmerged_courseonly, SEID_Indicator)
  
  
  
  ###'######################################################################
  ###'
  ###' Save the resulting dataframe 
  ###'
  ###'
  
  ### Arrange rows and columns before saving
  names(df_merged)
  
  df_merged <- df_merged %>%
    dplyr::select(CountyCode:SchoolName, ClassID, CourseCode, ClassCourseID, 
                  RecID, EstimatedFTE, everything()) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, ClassID, CourseCode, RecID)
  
  
  setwd(data_dir)
  dualsave(df_merged, 
           paste0("CoursesTaught", year_num, 
                  "_merged with CourseEnrollment and StaffAssign"))
  
  
  
  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("Year ", year_num, " completed", "\n"))
  
  
  
  ###'######################################################################
  ###' 
  ###' End for loop
  ###' 
  
}


