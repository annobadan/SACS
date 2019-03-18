
###'######################################################################
###'
###' Generate Variables
###' 
###' School-level Teacher Compositions 2012-2017
###' 
###' - with "Staff_Cred" data 
###' 
###' 
###' 20181127 JoonHo Lee
###' 20181223 JoonHo Lee - Update with Unified_Subject
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
data_dir1 <- c("D:/Data/LCFF/Staff_Data/Certificated_Staff/Staff_Demographic")
data_dir2 <- c("D:/Data/LCFF/Staff_Data/Certificated_Staff/Staff_Assignment_and_Course")


### Call libraries
library(tidyverse)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import Unified_Subject data
###'
###'

setwd(data_dir2)
tbl_unified <- read.csv("AssignmentCodes_Subject_Naming_StaffCred1217_Edited.csv")



###'######################################################################
###'
###' Loop over years
###'
###'

years <- sprintf("%02d",seq(12, 17))

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
  
  ### Set data containing working directory
  setwd(data_dir1)
  
  
  ### Staff Cred => doesn't include CDS code 
  load(file = paste0("StaffCred", year_num, "_cleaned", ".rda"))
  df_Cred <- df

  
  ### SchoolStaffFTE => To link RecID with CDS code
  load(file = paste0("StaffSchoolFTE", year_num, "_cleaned", ".rda"))
  df_FTE <- df
  
  
  
  ###'######################################################################
  ###'
  ###' Merge StaffCred with StaffSchoolFTE
  ###' 
  ###' by RecID
  ###'
  ###'
  
  ### Check duplicated rows of df_FTE
  match_key <- c("CountyCode", "DistrictCode", "SchoolCode", "RecID")
  df_FTE_dups <- tag_ID_miss_dups(df_FTE, match_key)
  
  
  ### Check duplicateed rows of df_Cred
  df_Cred_dups <- tag_ID_miss_dups(df_Cred, match_key = c("RecID"))
  
  
  ### StaffSchoolFTE => Reduce to key dataframe
  names(df_FTE)
  df_FTE_key <- df_FTE %>% 
    dplyr::select(CountyCode, DistrictCode, SchoolCode, RecID) %>%
    distinct()
  
  
  ###' Check duplicated rows => different! 
  ###' This means that duplicated 17,534 RecIDs appears in different schools
  length(df_FTE_key$RecID)     # distinct for CDS & RecID
  n_distinct(df_FTE_key$RecID) # distinct for only RecID

  
  ###' Merge to df_Cred
  ###' Arrange
  
  match_key <- c("CountyCode", "DistrictCode", "SchoolCode")
  
  df_Cred_key <- df_Cred %>%
    left_join(df_FTE_key, by = c("RecID")) %>%
    dplyr::select(match_key, RecID, everything()) %>%
    dplyr::select(-FileCreated, -AcademicYear) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, RecID)
  
  
  
  ###'######################################################################
  ###' 
  ###' Data frame #1. "df_CredType"
  ###' 
  ###' => Total. Not broken down by Subjects
  ###' 
  ###' 
  ###' (1) Full_Cred: Total Count/Percent of teachers with full credential
  ###'    	
  ###'     Completed a teacher preparation program and holds a 
  ###'     preliminary, clear, professional clear, or life credential  
  ###' 
  ###' 
  ###' (2) Univ_Int: Total Count/Percent of teachers with university credential
  ###'    
  ###'     University credential program in which the intern is enrolled 
  ###'     in the University taking coursework while teaching     
  ###' 
  ###' 
  ###' (3) Dist_Int: Total Count/Percent of teachers with district credential
  ###' 
  ###'     District credential program in which interns participate 
  ###'     in preparation that includes staff development, 
  ###'     but may not include college coursework 
  ###' 
  ###' 
  ###' (4) Emergency: Total Count/Percent of teachers with emergency credential
  ###' 
  ###'     Requested by an employer on behalf of an individual 
  ###'     who does not qualify for a credential or internship 
  ###'     but meets minimum certification requirements. 
  ###'     The permit holder completes credential requirements 
  ###'     through a college or university for renewal. 
  ###'     
  ###'     
  ###' (5) Waiver: Total Count/Percent of teachers with waiver
  ###' 
  ###'     Requested by an employer on behalf of an individual 
  ###'     when the employer is unable to find credentialed teachers or individuals 
  ###'     who qualify for an emergency permit. 
  ###' 
  ###'      
  
  ### Check distribution
  tabdf(df_Cred_key, CredentialType_Name)

  
  ###' Summarise at teacher-level
  df_Cred_key_RecID <- df_Cred_key %>%
    group_by(CountyCode, DistrictCode, SchoolCode, RecID, 
             CredentialType, CredentialType_Name) %>%
    summarise(N_Cred = n()) 
  
  
  ###' Calculate unduplicated number of teachers within each school
  df_Cred_key_summary <- df_Cred_key_RecID %>%
    group_by(CountyCode, DistrictCode, SchoolCode) %>%
    summarise(N_RecID = n_distinct(RecID), 
              Full_Cred = sum(CredentialType == 10, na.rm = TRUE), 
              Univ_Int = sum(CredentialType == 20, na.rm = TRUE), 
              Dist_Int = sum(CredentialType == 30, na.rm = TRUE), 
              Emergency = sum(CredentialType == 80, na.rm = TRUE), 
              Waiver = sum(CredentialType == 40, na.rm = TRUE))
  
  
  ### Calculate percentages
  level_vec <- c("Full_Cred", "Univ_Int", "Dist_Int", "Emergency", "Waiver")
  level_vec_extend <- c(paste0("N_", level_vec), paste0("PCT_", level_vec))
  
  df_CredType_PCT <- df_Cred_key_summary %>%
    gather(key = key, value = N, Full_Cred:Waiver) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, key) %>%
    mutate(PCT = 100*(N/N_RecID)) %>%
    gather(key = quantity, value = value, N, PCT) %>%
    unite(variable, quantity, key) %>%
    mutate(variable = factor(variable, levels = level_vec_extend)) %>%
    spread(variable, value)
  
  
  ### Add AcademicYear & add Unified_Name
  df_CredType_PCT <- df_CredType_PCT %>%
    mutate(AcademicYear = as.numeric(year_num), 
           Unified_Name = "Total") %>%
    dplyr::select(AcademicYear, 
                  CountyCode, DistrictCode, SchoolCode, Unified_Name, 
                  everything())
  
  classmode(df_CredType_PCT, everything())
  
  
  
  ###'######################################################################
  ###' 
  ###' Data frame #2. "df_CredType_Subject"
  ###' 
  ###' => Broken down by Subjects
  ###' 
  ###' 
  
  ### Check distribution
  tabdf(df_Cred_key, CredentialType_Name)
  
  
  ###' Summarise at teacher-level
  df_Cred_key_RecID_Subject <- df_Cred_key %>%
    group_by(CountyCode, DistrictCode, SchoolCode, AuthorizationType_Name, RecID, 
             CredentialType, CredentialType_Name) %>%
    summarise(N_Cred = n()) 

  
  ###' Calculate unduplicated number of teachers within each school
  df_Cred_key_summary_Subject <- df_Cred_key_RecID_Subject %>%
    group_by(CountyCode, DistrictCode, SchoolCode, AuthorizationType_Name) %>%
    summarise(N_RecID = n_distinct(RecID), 
              Full_Cred = sum(CredentialType == 10, na.rm = TRUE), 
              Univ_Int = sum(CredentialType == 20, na.rm = TRUE), 
              Dist_Int = sum(CredentialType == 30, na.rm = TRUE), 
              Emergency = sum(CredentialType == 80, na.rm = TRUE), 
              Waiver = sum(CredentialType == 40, na.rm = TRUE))
  
  
  ### Calculate percentages
  level_vec <- c("Full_Cred", "Univ_Int", "Dist_Int", "Emergency", "Waiver")
  level_vec_extend <- c(paste0("N_", level_vec), paste0("PCT_", level_vec))
  
  df_CredType_PCT_Subject <- df_Cred_key_summary_Subject %>%
    gather(key = key, value = N, Full_Cred:Waiver) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, key) %>%
    mutate(PCT = 100*(N/N_RecID)) %>%
    gather(key = quantity, value = value, N, PCT) %>%
    unite(variable, quantity, key) %>%
    mutate(variable = factor(variable, levels = level_vec_extend)) %>%
    spread(variable, value)
  
  
  ### Add AcademicYear & rename Unified_Name
  df_CredType_PCT_Subject <- df_CredType_PCT_Subject %>%
    mutate(AcademicYear = as.numeric(year_num)) %>%
    dplyr::select(AcademicYear, everything()) %>%
    rename(Unified_Name = AuthorizationType_Name)
  
  classmode(df_CredType_PCT_Subject, everything())

  
  
  ###'######################################################################
  ###' 
  ###' Append the generated two dataframes
  ###' 
  ###' 
  
  ### Bind rows
  df_bind <- bind_rows(df_CredType_PCT, df_CredType_PCT_Subject) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, Unified_Name)
  
  
  ### Embed into the meta_list
  meta_list[[i]] <- df_bind
              
  
  
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


###'######################################################################
###' 
###' Convert list to dataframe
###' 
###' 

df_bound <- bind_rows(meta_list) %>%
  dplyr::select(CountyCode, DistrictCode, SchoolCode, Unified_Name, AcademicYear, 
                everything()) %>%
  arrange(CountyCode, DistrictCode, SchoolCode, Unified_Name, AcademicYear)


setwd(data_dir1)
dualsave(df_bound, "df_StaffCred_2012_2017_by_Subject")

