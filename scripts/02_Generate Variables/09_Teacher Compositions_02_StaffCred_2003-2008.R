
###'######################################################################
###'
###' Generate Variables
###' 
###' School-level Teacher Compositions 2003-2008
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
tbl_unified <- read.csv("AssignmentCodes_Subject_Naming_StaffCred0308_Edited.csv")



###'######################################################################
###'
###' Loop over years
###'
###'

years <- sprintf("%02d",seq(03, 08))

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
  
  
  ### Staff Cred 
  load(file = paste0("StaffCred", year_num, "_cleaned", ".rda"))
  df<- df_Cred_long
  names(df)
  
  
  ### Merge unified name and code
  df <- df %>%
    left_join(tbl_unified, 
              by = c("AuthorizationType_Key", "AuthorizationType_Name"))
  
  
  
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
  
  ### Summarise at teacher-level
  df_CredType_temp <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, RecID) %>%
    summarise_at(.vars = c("FULL_CRED", "UNIV_INT", "DIST_INT", "EMERGENCY", "WAIVER"), 
                 .funs = function(x) sum(x, na.rm = TRUE))
  
  
  ### Summarise at school-level
  count_nonzero <- function(x) {sum(x != 0, na.rm = TRUE)}
  
  df_CredType <- df_CredType_temp %>%
    group_by(CountyCode, DistrictCode, SchoolCode) %>%
    summarise(N_RecID = n_distinct(RecID), 
              Full_Cred = count_nonzero(FULL_CRED), 
              Univ_Int = count_nonzero(UNIV_INT), 
              Dist_Int = count_nonzero(DIST_INT), 
              Emergency = count_nonzero(EMERGENCY), 
              Waiver = count_nonzero(WAIVER)) 
  
  
  ### Calculate percentages
  level_vec <- c("Full_Cred", "Univ_Int", "Dist_Int", "Emergency", "Waiver")
  level_vec_extend <- c(paste0("N_", level_vec), paste0("PCT_", level_vec))
  
  df_CredType_PCT <- df_CredType %>%
    gather(key = key, value = N, Full_Cred:Waiver) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, key) %>%
    mutate(PCT = 100*(N/N_RecID)) %>%
    gather(key = quantity, value = value, N, PCT) %>%
    unite(variable, quantity, key) %>%
    mutate(variable = factor(variable, levels = level_vec_extend)) %>%
    spread(variable, value)
  
  
  ### Add AcademicYear
  df_CredType_PCT <- df_CredType_PCT %>%
    mutate(AcademicYear = as.numeric(year_num), 
           Unified_Name = "Total") %>%
    dplyr::select(AcademicYear, CountyCode, DistrictCode, SchoolCode, 
                  Unified_Name, everything())
  
  classmode(df_CredType_PCT, everything())
  
  

  ###'######################################################################
  ###' 
  ###' Data frame #2. "df_CredType_Subject" 
  ###' 
  ###' => Broken down by Subjects
  ###' 
  ###'      
  
  ### Summarise at teacher-level
  df_CredType_Subject_temp <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, Unified_Name, RecID) %>%
    summarise_at(.vars = c("FULL_CRED", "UNIV_INT", "DIST_INT", "EMERGENCY", "WAIVER"), 
                 .funs = function(x) sum(x, na.rm = TRUE))
  
  
  ### Summarise at school-level
  count_nonzero <- function(x) {sum(x != 0, na.rm = TRUE)}
  
  df_CredType_Subject <- df_CredType_Subject_temp %>%
    group_by(CountyCode, DistrictCode, SchoolCode, Unified_Name) %>%
    summarise(N_RecID = n_distinct(RecID), 
              Full_Cred = count_nonzero(FULL_CRED), 
              Univ_Int = count_nonzero(UNIV_INT), 
              Dist_Int = count_nonzero(DIST_INT), 
              Emergency = count_nonzero(EMERGENCY), 
              Waiver = count_nonzero(WAIVER)) 
  
  
  ### Calculate percentages
  level_vec <- c("Full_Cred", "Univ_Int", "Dist_Int", "Emergency", "Waiver")
  level_vec_extend <- c(paste0("N_", level_vec), paste0("PCT_", level_vec))
  
  df_CredType_Subject_PCT <- df_CredType_Subject %>%
    gather(key = key, value = N, Full_Cred:Waiver) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, Unified_Name, key) %>%
    mutate(PCT = 100*(N/N_RecID)) %>%
    gather(key = quantity, value = value, N, PCT) %>%
    unite(variable, quantity, key) %>%
    mutate(variable = factor(variable, levels = level_vec_extend)) %>%
    spread(variable, value)
  
  
  ### Add AcademicYear
  df_CredType_Subject_PCT <- df_CredType_Subject_PCT %>%
    mutate(AcademicYear = as.numeric(year_num)) %>%
    dplyr::select(AcademicYear, everything())
  
  classmode(df_CredType_Subject_PCT, everything())
  
  
  
  ###'######################################################################
  ###' 
  ###' Append the generated two dataframes
  ###' 
  ###' 
  
  ### Bind rows
  df_bind <- bind_rows(df_CredType_PCT, df_CredType_Subject_PCT) %>%
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
  dplyr::select(CountyCode, DistrictCode, SchoolCode, Unified_Name, 
                AcademicYear, everything()) %>%
  arrange(CountyCode, DistrictCode, SchoolCode, Unified_Name, AcademicYear)


setwd(data_dir1)
dualsave(df_bound, "df_StaffCred_2003_2008_by_Subject")

