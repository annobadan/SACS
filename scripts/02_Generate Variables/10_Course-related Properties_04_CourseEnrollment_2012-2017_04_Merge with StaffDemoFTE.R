
###'######################################################################
###'
###' Generate Variables
###' 
###' Course-related Properties: 
###' 
###' => Merge (CoursesTaught + CourseEnrollment + StaffAssign) with StaffDemoFTE
###'
###' (1) CoursesTaught + CourseEnrollment + StaffAssign 2012-2017 
###' (2) StaffDemoFTE
###'  
###'   < Merge by the following four identifiers > :
###'   
###'   CountyCode, DistrictCode, SchoolCode, RecID 
###'   
###' 
###' 20181211 JoonHo Lee
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
data_dir1 <- c("D:/Data/LCFF/Staff_Data/Certificated_Staff/Staff_Assignment_and_Course")
data_dir2 <- c("D:/Data/LCFF/Staff_Data/Certificated_Staff/Staff_Demographic")


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

  ### Load the CoursesTaught + CourseEnrollment + StaffAssign Data
  setwd(data_dir1)
  load(file = paste0("CoursesTaught", year_num, 
                     "_merged with CourseEnrollment and StaffAssign", ".rda"))
  df_course <- df_to_save; rm(df_to_save)
  classmode(df_course, everything())
  
  
  ### Load the StaffDemoFTE
  setwd(data_dir2)
  load(file = paste0("Staff_Demo_FTE", year_num, "_merged", ".rda"))
  df_demo <- df_demo_fte; rm(df_demo_fte)
  classmode(df_demo, everything())
  
  
  ### Generate key for matching
  names(df_course)
  names(df_demo)
  match_key <- c("CountyCode", "DistrictCode", "SchoolCode", "RecID")
  
  
  
  ###'######################################################################
  ###'
  ###' Filter only Teachers
  ###'
  ###' => Because only teachers have course information
  ###'
  ###'
  
  tabdf(df_demo, StaffType)  # About 84% of cases are teachers for 2012 data
  
  df_teacher <- df_demo %>%
    filter(StaffType == "Teacher")
  
  tabdf(df_demo, JobClassification)
  tabdf(df_teacher, JobClassification)
  
  
  
  
  ###'######################################################################
  ###' 
  ###' Check missing and duplicated ID
  ###' 
  ###' (1) StaffDemoFTE   
  ###' 
  ###' => ID_duplicates: Teacher + Itinerant or Pull-out/Push-in Teacher
  ###' 
  ###' 
  
  df_teacher <- tag_ID_miss_dups(df_teacher, match_key = match_key)
  
  tabdf(df_teacher, tag)
  
  df_teacher_duplicated <- df_teacher %>% 
    filter(tag == "ID_duplicated")
  
  setwd(data_dir2)
  write.csv(df_teacher_duplicated, 
            file = paste0("Staff_Demo_FTE", year_num, "_duplicated", ".csv"))
  
  
  
  ###'######################################################################
  ###' 
  ###' Process duplicates
  ###' 
  ###' 

  ### Collapse duplicated IDs to one ID
  names(df_teacher_duplicated)
  
  df_teacher_duplicated_collapsed <- df_teacher_duplicated %>%
    group_by(CountyCode, DistrictCode, SchoolCode, RecID) %>% 
    arrange(CountyCode, DistrictCode, SchoolCode, RecID, JobClassification) %>%
    mutate_at(.vars = c("GenderCode", "EthnicGroup", "EducationLevel", 
                        "YearsTeaching", "YearsInDistrict",
                        "EmploymentStatusCode", 
                        "StaffType"), 
              .funs = first) %>%
    mutate_at(.vars = c("JobClassification"), 
              .funs = function(x) paste(x, collapse = ", ")) %>%
    dplyr::select(-FTE) %>%
    distinct()
  
  tabdf(df_teacher_duplicated_collapsed, JobClassification)
  
  
  ### Bind with the original Staff_Demo_FTE dataset
  df_teacher_unduplicated <- df_teacher %>%
    filter(tag == "ID_valid") %>%
    dplyr::select(-FTE)

  df_teacher_to_merge <- df_teacher_unduplicated %>%
    bind_rows(df_teacher_duplicated_collapsed)
  
  
  
  ###'######################################################################
  ###'
  ###' Merge (CoursesTaught + CourseEnrollment + StaffAssign) with StaffDemoFTE 
  ###'
  ###'

  ### Remove columns with the same variable names
  intersect(names(df_course), names(df_teacher_to_merge))
  
  df_teacher_to_merge <- df_teacher_to_merge %>%
    dplyr::select(-CountyName, -DistrictName, -SchoolName, -tag, -FileCreated)
  
  
  ### Prepare df_course dataframe to merge
  tabdf(df_course, .merge)
  
  df_course_to_merge <- df_course %>%
    filter(.merge == "matched") %>% 
    dplyr::select(-.merge)
  
  
  ### Merge!
  df_merged <- df_course_to_merge %>%
    full_join_track(df_teacher_to_merge, by = match_key, .merge = TRUE)
  
  
  ### Look into unmerged cases from Staff_Demo_FTE data (about 2% in 2012)
  tabdf(df_merged, .merge)
  
  df_unmerged <- df_merged %>%
    filter(.merge != "matched")
  
  tabdf(df_unmerged, JobClassification)
  
  setwd(data_dir1)
  write.csv(df_unmerged, 
            file = paste0("CoursesTaught", year_num, 
                          "_merged with CourseEnrollment_StaffAssign_StaffDemoFTE_unmerged cases.csv"))
  
  
  
  ###'######################################################################
  ###'
  ###' Save the resulting dataframe 
  ###'
  ###'
  
  ### Arrange rows and columns before saving
  names(df_merged)
  
  df_merged <- df_merged %>%
    dplyr::select(-AcademicYear, -CD_code) %>% 
    mutate(RecID2 = RecID) %>%
    dplyr::select(CountyCode:SchoolName,
                  RecID2, GenderCode:FTE_PupilServices, 
                  RecID, EstimatedFTE, 
                  ClassID, CourseCode, ClassCourseID, 
                  Assign_Type, Assign_Subject, Assign_Name, 
                  everything()) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, RecID, ClassID, CourseCode)
  
  setwd(data_dir1)
  dualsave(df_merged, 
           paste0("CoursesTaught", year_num, 
                  "_merged with CourseEnrollment_StaffAssign_StaffDemoFTE"))
  


  
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
