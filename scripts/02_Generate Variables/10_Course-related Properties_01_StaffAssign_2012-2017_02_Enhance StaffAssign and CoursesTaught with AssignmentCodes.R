
###'######################################################################
###'
###' Generate Variables
###' 
###' Course-related Properties 
###' 
###' CoursesTaught + AssignmentCodes12On_Unified Subject 2012-2017 data
###' 
###' => (1) Enhance CoursesTaught with AssignmentCodes12On data
###' => (2) Enhance StaffAssign with AssignmentCodes12On data
###' 
###' 
###' 20181230 JoonHo Lee
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
  
  ### (1) CoursesTaught 
  load(file = paste0("CoursesTaught", year_num, "_cleaned", ".rda"))
  df_course <- df; rm(df)
  classmode(df_course, everything())
  
  
  ### (2) StaffAssign cleaned data
  load(file = paste0("StaffAssign", year_num, "_cleaned", ".rda"))
  df_staff <- df; rm(df)
  classmode(df_staff, everything())
  
  
  ### (2) AssignmentCodes with Unified Subject Name 2012-2017
  load(file = "AssignmentCodes12On_Unified Subject.rda")
  df_unified <- df_to_save
  
  
  

  ###'######################################################################
  ###'
  ###' (1) Enhance CoursesTaught with AssignmentCodes12On
  ###'
  ###'
  
  names(df_course)
  
  
  ### Merge!
  df_course_merged <- df_course %>%
    full_join_track(df_unified, 
                    by = c("CourseCode" = "Assign_Code"), 
                    .merge = TRUE)
  
  
  ### Look into unmerged cases
  df_course_only <- df_course_merged %>%
    filter(.merge == "left_only")
  
  df_unified_only <- df_course_merged %>%
    filter(.merge == "right_only")
  
  tabdf(df_unified_only, Subject_Category)
  
  
  ### Remove unmerged cases
  df_course_matched <- df_course_merged %>%
    filter(.merge == "matched") %>%
    select(-.merge)
    
  
  ### Arrange rows and columns
  names(df_course_matched)
  
  df_course_matched <- df_course_matched %>%
    select(AcademicYear, 
           CountyCode, DistrictCode, SchoolCode, 
           CountyName, DistrictName, SchoolName, 
           CourseCode, ClassID, ClassCourseID, 
           Assign_Name, Assign_Subject, Subject_Category, Assign_Type, 
           Enrollment, 
           UCCSU_Requirements, UC_CSU_Approved, 
           NCLB_Core, NCLB_HQT, 
           AP, IB, CTE, CTE_FundingProvider, 
           DistanceLearning, IndependentStudy, MultipleTeacherCode, 
           SEID_Indicator) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, ClassID, CourseCode)
  
  
  
  ###'######################################################################
  ###'
  ###' (2) Enhance CoursesTaught with AssignmentCodes12On
  ###'
  ###'
  
  names(df_staff)
  names(df_unified)
  
  
  ### Prepare merge
  df_unified_to_merge <- df_unified %>%
    select(Assign_Code, Assign_Name, Assign_Subject, Subject_Category)
  
  
  ### Merge!
  df_staff_merged <- df_staff %>%
    full_join_track(df_unified_to_merge, 
                    by = c("Assign_Code", "Assign_Name", "Assign_Subject"), 
                    .merge = TRUE)
  
  
  df_staff_matched <- df_staff_merged %>%
    filter(.merge == "matched") %>%
    select(-.merge)
  
  
  
  ###'######################################################################
  ###'
  ###' Save the resulting dataset
  ###'
  ###'
  
  setwd(data_dir)
  
  dualsave(df_course_matched, 
           paste0("CoursesTaught", year_num, "_enhanced_with_AssignmentCodes"))
  
  dualsave(df_staff_matched, 
           paste0("StaffAssign", year_num, "_enhanced_with_Unified Subject"))
  
  
  
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

