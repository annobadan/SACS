
###'######################################################################
###'
###' Generate Variables
###' 
###' Course-related Properties 2003-2011
###' 
###' - Enhance "assign" data with AssignmentCode11before with Unified Subject Name
###' 
###' 
###' 20181228 JoonHo Lee 
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

years <- c(sprintf("%02d", seq(3, 8)), sprintf("%02d", seq(10, 11)))

meta_list <- list()


for (i in seq_along(years)) {
  
  
  ###'######################################################################
  ###'
  ###' Assign year number
  ###' 
  ###' 
  
  year_num <- years[i]
  
  
  
  ###'######################################################################
  ###'
  ###' Import cleaned data files
  ###'
  ###'
  
  setwd(data_dir)
  
  ### (1) Import Assignment Codes data (Unified): 2003-2011
  load(file = "AssignmentCodes11before_Unified Subject.rda")
  df_assign_code <- df_to_save  
  
  classmode(df_assign_code, everything())
  
  
  ### (2) Load StaffAssign data
  load(file = paste0("assign", year_num, "_cleaned", ".rda"))
  
  classmode(df, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Merge StaffAssign data with Assignment Codes data
  ###'
  ###'
  
  ### Merge! 
  df_merged <- df %>%
    full_join_track(df_assign_code, 
                    by = c("Assign_Type", "Assign_Code"), 
                    .merge = TRUE)
  
  
  ### Check the unmerged cases
  tabdf(df_merged, .merge)
  
  df_unmerged_left <- df_merged %>%
    filter(.merge == "left_only")  # cases with Assign_Code == 4080, which doesn't exist
  
  df_unmerged_right <- df_merged %>%
    filter(.merge == "right_only") # courses are not provided in reality
  
  
  ### Remove the unmerged cases
  df_matched <- df_merged %>%
    filter(.merge == "matched") %>%
    dplyr::select(-.merge)
  
  
  
  ###'######################################################################
  ###'
  ###' Recode UC_CSU_Approved
  ###' 
  ###' 	Always_A-G인데 UC_CSU_Approved가 missing인 경우 => 1로 코딩
  ###' 	Sometimes_A-G인데 missing인 경우 => 어쩔 수 없음. missing으로 놔두기
  ###' 	Not A-G인데 missing인 경우 => 0으로 코딩
  ###'
  ###'
  
  ### Check distributions
  classmode(df_matched, everything())
  tabdf(df_matched, UC_CSU_Approved)
  tabdf(df_matched, UCCSU_Requirements)
  
  
  ### Always A-G
  cond_NonAdmin <- df_matched$Topic_Code != "03"
  cond_Always <- df_matched$UCCSU_Requirements == "Always_A-G"
  cond_Approve_Miss <- is.na(df_matched$UC_CSU_Approved)
  
  idx <- cond_NonAdmin & cond_Always & cond_Approve_Miss
  sum(idx)  # good!
  
  df_matched$UC_CSU_Approved[idx] <- 1
  
  
  ### Not A-G
  cond_Not <- df_matched$UCCSU_Requirements == "Not_A-G"
  
  idx <- cond_NonAdmin & cond_Not & cond_Approve_Miss
  sum(idx)  # good!
  
  df_matched$UC_CSU_Approved[idx] <- 0
  
  
  ### Check distributions 
  tabdf(df_matched, UC_CSU_Approved)
  tabdf(df_matched, UCCSU_Requirements)
  
  
  
  ###'######################################################################
  ###'
  ###' Save the matched data
  ###'
  ###'
  
  ### Arrange rows and columns
  classmode(df_matched, everything())
  
  df_matched <- df_matched %>%
    dplyr::select(AcademicYear, CDS_CODE, CountyCode, DistrictCode, SchoolCode, 
                  RecID, Assign_Type, Assign_Code, Assign_Name, 
                  EstimatedFTE, PERC_TIME, 
                  GradeLevelCode, Enroll_male, Enroll_female, Enroll_total, 
                  UCCSU_Requirements, UC_CSU_Approved, 
                  Topic_Code, Topic_Name, 
                  Assign_Subject_Code, Assign_Subject, 
                  Unified_Subject, Subject_Category) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, RecID, Assign_Type, Assign_Code)
  
  
  ### Save the resulting dataset
  setwd(data_dir)
  
  dualsave(df_matched, paste0("assign", year_num, 
                              "_enhanced_with_AssignmentCodes"))
  
  
  
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