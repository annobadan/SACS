
###'######################################################################
###'
###' Generate Variables
###' 
###' Course-related Properties: 
###' 
###' => Merge all the ClassCourse infromation
###'
###' (1) CoursesTaught + 
###' (2) AssignmentCodes12On + 
###' (3) CourseEnrollment_managed 2012-2017 
###'  
###'   < Merge by the following six identifiers > :
###'   
###'   CountyCode, DistrictCode, SchoolCode, 
###'   
###'   ClassID, CourseCode, ClassCourseID   
###'         
###' 
###' 20181204 JoonHo Lee
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
  
  ### Load the collapsed CourseEnrollment data
  load(file = paste0("CourseEnrollment", year_num, "_managed", ".rda"))
  df_enr <- df_to_save; rm(df_to_save)
  classmode(df_enr, everything())
  
  
  ### Load the raw CoursesTaught data
  load(file = paste0("CoursesTaught", year_num, "_cleaned", ".rda"))
  df_taught <- df; rm(df)
  classmode(df_taught, everything())
  
  
  ### Load AssignmentCode data
  load(file = "AssignmentCodes12On_cleaned.rda")
  df_assign <- df; rm(df)
  classmode(df_assign, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Merge CourseTaught + AssignmentCode
  ###'
  ###'
  
  ### Check classmode of key variables
  classmode(df_taught, CourseCode)
  classmode(df_assign, Assign_Code)
  
  
  ### Merge!
  df_temp <- left_join(df_taught, df_assign, 
                       by = c("CourseCode" = "Assign_Code"))
  
  
  ### Check distribution
  tabdf(df_temp, Assign_Type)
  tabdf(df_temp, Assign_Subject)
  
  
  ### Remove unnecessary variables and reorder variables  
  names(df_temp)
  df_temp <- df_temp %>%
    select(-AcademicYear, -CD_code) %>%
    select(CountyCode, DistrictCode, SchoolCode, 
           CountyName, DistrictName, SchoolName, 
           ClassID, CourseCode, ClassCourseID, 
           Assign_Type, Assign_Subject, Assign_Name, 
           UCCSU_Requirements, UC_CSU_Approved, 
           NCLB_Core, NCLB_HQT, AP, IB, CTE, CTE_FundingProvider,  
           DistanceLearning, IndependentStudy, 
           MultipleTeacherCode, SEID_Indicator, Enrollment)
  
  
  
  ###'######################################################################
  ###'
  ###' Check missing and duplicated IDs of "CoursesTaught"
  ###' 
  ###'
  
  match_key <- c("CountyCode", "DistrictCode", "SchoolCode", 
                 "ClassID", "CourseCode", "ClassCourseID")
  
  df_temp <- tag_ID_miss_dups(df_temp, match_key = match_key)
  
  tabdf(df_temp, tag)
  
  
  ### Save the cases with duplicated ID as .csv file
  setwd(data_dir)
  write.csv(df_temp %>% filter(tag == "ID_duplicated"),
            paste0("CoursesTaught", year_num, "_cases with duplicated IDs", ".csv"))
  
  
  
  ###'######################################################################
  ###'
  ###' Aggregate the duplicated cases
  ###' 
  ###' - (1) Enrollment: group-level summation
  ###' - (2) NCLB HQT: Squashing multiple rows per group into one
  ###'
  ###'
  
  df_temp_unduplicated <- df_temp %>%
    filter(tag == "ID_duplicated") %>%
    group_by(CountyCode, DistrictCode, SchoolCode, 
             CountyName, DistrictName, SchoolName, 
             ClassID, CourseCode, ClassCourseID, 
             Assign_Type, Assign_Subject, Assign_Name, 
             UCCSU_Requirements) %>%
    summarise(UC_CSU_Approved = mean(UC_CSU_Approved, na.rm = TRUE), 
             NCLB_Core = mean(NCLB_Core, na.rm = TRUE), 
             NCLB_HQT = paste(NCLB_HQT, collapse = ", "), 
             AP = mean(AP, na.rm = TRUE), 
             IB = mean(IB, na.rm = TRUE), 
             CTE = mean(CTE, na.rm = TRUE), 
             CTE_FundingProvider = first(CTE_FundingProvider), 
             DistanceLearning = mean(DistanceLearning, na.rm = TRUE), 
             IndependentStudy = mean(IndependentStudy, na.rm = TRUE), 
             MultipleTeacherCode = first(MultipleTeacherCode), 
             SEID_Indicator = mean(SEID_Indicator, na.rm = TRUE), 
             Enrollment = sum(Enrollment, na.rm = TRUE), 
             tag = first(tag))
  
  
  
  ###'######################################################################
  ###'
  ###' Append the aggreagated duplicated cases to the original data  
  ###'
  ###'
  
  tabdf(df_temp, tag)
  
  df_bind <- df_temp %>%
    filter(tag == "ID_valid") %>%
    bind_rows(df_temp_unduplicated)
  
  classmode(df_bind, everything())
  
  tabdf(df_bind, NCLB_Core)
  tabdf(df_bind, NCLB_HQT)
  tabdf(df_bind, AP)
  tabdf(df_bind, IB)
  tabdf(df_bind, CTE)
  
  
  
  ###'######################################################################
  ###'
  ###' Merge with the collapsed CourseEnrollment Data
  ###'
  ###'
  
  ### Merge!
  df_merged <- df_bind %>% 
    full_join_track(df_enr, by = match_key, .merge = TRUE)
  

  ### Check unmerged cases
  names(df_merged)
  tabdf(df_merged, .merge)
  
  
  ### Unmerged CoursesTaught
  df_unmerged <- df_merged %>%
    filter(.merge != "matched")
  
  write.csv(df_unmerged, 
            file = paste0("CoursesTaught", year_num, 
                          "_unmerged cases with CourseEnrollment", ".csv"))

  

  ###'######################################################################
  ###'
  ###' Save the resulting dataframe 
  ###'
  ###'
  
  setwd(data_dir)
  
  dualsave(df_merged, paste0("CoursesTaught", year_num, 
                             "_merged with CourseEnrollment"))
  
  
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


