
###'######################################################################
###'
###' Generate Variables
###' 
###' Course-related Properties 
###' 
###' StaffAssign 2003-2011 data
###' 
###' => Generate School-level variables 
###' 
###' 
###' 20181229 JoonHo Lee
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
  
  
  ### (1) StaffAssign 2003-2011 enhanced with AssignmentCodes
  
  load(file = paste0("assign", year_num, "_enhanced_with_AssignmentCodes.rda"))
  
  df_origin <- df_to_save; rm(df_to_save)
  
  classmode(df_origin, everything())
  
  
  ### (2) StaffAssign 2003-2011 Teacher-level data
  
  load(file = paste0("assign", year_num, "_Teacher-level variables.rda"))
  
  df_teacher <- df_to_save; rm(df_to_save)
  
  classmode(df_teacher, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Filter only Teachers
  ###' 
  ###' => Remove Administrator, Pupil Services
  ###'
  ###'
  
  tabdf(df_origin, Assign_Type)
  
  df <- df_origin %>%
    filter(Assign_Type == "Teacher")
  
  
  
  ###'######################################################################
  ###'
  ###' (1) The number of Subjects, Courses, and Class Periods
  ###'
  ###'
  
  ### Total: Not by subject
  df_count_total <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode) %>%
    summarise(N_Subject = n_distinct(Unified_Subject), 
              N_Course = n_distinct(Assign_Name), 
              N_Period = n()) %>%
    mutate(Subject_Category = "Total")
  
  
  ### By Subject
  df_count_subj <- df %>% 
    group_by(CountyCode, DistrictCode, SchoolCode, Subject_Category) %>%
    summarise(N_Subject = n_distinct(Unified_Subject), 
              N_Course = n_distinct(Assign_Name), 
              N_Period = n())
  
  
  ### Bind rows
  df_count <- bind_rows(df_count_subj, df_count_total) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, Subject_Category)
  
  
  
  
  ###'######################################################################
  ###'
  ###' (2) Average number of Subjects, Courses, 
  ###'     and Class Periods assigned to teachers
  ###'
  ###'  1.1. Not by subject: 
  ###'  - Reflect teachers cross-subject assignment
  ###'  - Individual teacher's working conditions
  ###'  
  ###'  1.2. By subject
  ###'  - working conditions by each subject division within school
  ###'  - ELA, Math, and others 
  ###'
  ###'
  
  ### Total: Not by subject
  df_MNcount_total <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, RecID) %>%
    summarise(N_Subject = n_distinct(Unified_Subject), 
              N_Course = n_distinct(Assign_Name), 
              N_Period = n()) %>%
    ungroup() %>%
    group_by(CountyCode, DistrictCode, SchoolCode) %>%
    summarise(MN_Subject = mean(N_Subject, na.rm = TRUE), 
              MN_Course = mean(N_Course, na.rm = TRUE), 
              MN_Period = mean(N_Period, na.rm = TRUE)) %>%
    mutate(Subject_Category = "Total")
    
  
  ### By Subject
  df_MNcount_subj <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, Subject_Category, RecID) %>%
    summarise(N_Subject = n_distinct(Unified_Subject), 
              N_Course = n_distinct(Assign_Name), 
              N_Period = n()) %>%
    ungroup() %>%
    group_by(CountyCode, DistrictCode, SchoolCode, Subject_Category) %>%
    summarise(MN_Subject = mean(N_Subject, na.rm = TRUE), 
              MN_Course = mean(N_Course, na.rm = TRUE), 
              MN_Period = mean(N_Period, na.rm = TRUE))
  
  
  ### Bind rows
  df_MNcount <- bind_rows(df_MNcount_subj, df_MNcount_total) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, Subject_Category)
  
  
  
  ###'######################################################################
  ###'
  ###' (3) Percentage of UCCSU_Approved Courses within school
  ###'
  ###'
  
  ### Total: Not by subject
  df_AG_Approved_Total <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, UC_CSU_Approved) %>% 
    summarise(N = n()) %>%
    filter(!is.na(UC_CSU_Approved)) %>%  # Calculate PCT only among non-NA cases
    ungroup() %>%
    group_by(CountyCode, DistrictCode, SchoolCode) %>%
    mutate(N_Sum = sum(N, na.rm = TRUE), 
           PCT = 100*(N/N_Sum)) %>%
    mutate(UC_CSU_Approved = recode(UC_CSU_Approved, 
                                    '1' = "Apprvd", 
                                    '0' = "Not_Apprvd")) %>% 
    select(-N_Sum) %>%
    gather(key = key, value = value, N, PCT) %>%
    unite(variable, key, UC_CSU_Approved) %>% 
    spread(key = variable, value = value, fill = 0) %>%
    mutate(Subject_Category = "Total")
  
  
  ### By subject
  df_AG_Approved_Subj <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, Subject_Category, UC_CSU_Approved) %>% 
    summarise(N = n()) %>%
    filter(!is.na(UC_CSU_Approved)) %>%  # Calculate PCT only among non-NA cases
    ungroup() %>%
    group_by(CountyCode, DistrictCode, SchoolCode, Subject_Category) %>%
    mutate(N_Sum = sum(N, na.rm = TRUE), 
           PCT = 100*(N/N_Sum)) %>%
    mutate(UC_CSU_Approved = recode(UC_CSU_Approved, 
                                    '1' = "Apprvd", 
                                    '0' = "Not_Apprvd")) %>% 
    select(-N_Sum) %>%
    gather(key = key, value = value, N, PCT) %>%
    unite(variable, key, UC_CSU_Approved) %>% 
    spread(key = variable, value = value, fill = 0) 
  
  
  ### Bind rows
  df_AG_Approved <- bind_rows(df_AG_Approved_Subj, df_AG_Approved_Total) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, Subject_Category)
  
  
  
  ###'######################################################################
  ###'
  ###' (4) Percentage of UCCSU_Requirements Courses within school
  ###'
  ###'
  
  ### Total: Not by subject
  df_AG_Required_Total <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, UCCSU_Requirements) %>% 
    summarise(N = n()) %>%
    filter(!is.na(UCCSU_Requirements)) %>%  # Calculate PCT only among non-NA cases
    ungroup() %>%
    group_by(CountyCode, DistrictCode, SchoolCode) %>%
    mutate(N_Sum = sum(N, na.rm = TRUE), 
           PCT = 100*(N/N_Sum)) %>%
    select(-N_Sum) %>%
    gather(key = key, value = value, N, PCT) %>%
    unite(variable, key, UCCSU_Requirements) %>% 
    spread(key = variable, value = value, fill = 0) %>%
    mutate(Subject_Category = "Total")
  
  names(df_AG_Required_Total) <- gsub("A-G", "AG", names(df_AG_Required_Total))
  
  
  ### By subject
  df_AG_Required_Subj <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, Subject_Category, UCCSU_Requirements) %>% 
    summarise(N = n()) %>%
    filter(!is.na(UCCSU_Requirements)) %>%  # Calculate PCT only among non-NA cases
    ungroup() %>%
    group_by(CountyCode, DistrictCode, SchoolCode, Subject_Category) %>%
    mutate(N_Sum = sum(N, na.rm = TRUE), 
           PCT = 100*(N/N_Sum)) %>%
    select(-N_Sum) %>%
    gather(key = key, value = value, N, PCT) %>%
    unite(variable, key, UCCSU_Requirements) %>% 
    spread(key = variable, value = value, fill = 0) 
  
  names(df_AG_Required_Subj) <- gsub("A-G", "AG", names(df_AG_Required_Subj))
  
  
  ### Bind rows
  df_AG_Required <- bind_rows(df_AG_Required_Subj, df_AG_Required_Total) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, Subject_Category) %>%
    select(CountyCode:Subject_Category, 
           N_Always_AG, N_Sometimes_AG, N_Not_AG, 
           PCT_Always_AG, PCT_Sometimes_AG, PCT_Not_AG)
  
  
  
  ###'######################################################################
  ###'
  ###' (5) Percentage of AP Courses within school
  ###'
  ###'
  
  ### Generate AP Dummy indicator
  names(df)
  tabdf(df, Topic_Name)
  
  df <- df %>%
    mutate(AP = if_else(Topic_Name == "AP", 1, 0, missing = NA_real_))
  
  tabdf(df, AP)
  
  
  ### Total: Not by subject
  df_AP_Total <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, AP) %>% 
    summarise(N = n()) %>%
    filter(!is.na(AP)) %>%  # Calculate PCT only among non-NA cases
    ungroup() %>%
    group_by(CountyCode, DistrictCode, SchoolCode) %>%
    mutate(N_Sum = sum(N, na.rm = TRUE), 
           PCT = 100*(N/N_Sum)) %>%
    mutate(AP = recode(AP, '1' = "AP", '0' = "NonAP")) %>% 
    select(-N_Sum) %>%
    gather(key = key, value = value, N, PCT) %>%
    unite(variable, key, AP) %>% 
    spread(key = variable, value = value, fill = 0) %>%
    mutate(Subject_Category = "Total")
  
  
  ### By subject
  df_AP_Subj <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, Subject_Category, AP) %>% 
    summarise(N = n()) %>%
    filter(!is.na(AP)) %>%  # Calculate PCT only among non-NA cases
    ungroup() %>%
    group_by(CountyCode, DistrictCode, SchoolCode, Subject_Category) %>%
    mutate(N_Sum = sum(N, na.rm = TRUE), 
           PCT = 100*(N/N_Sum)) %>%
    mutate(AP = recode(AP, '1' = "AP", '0' = "NonAP"))  %>% 
    select(-N_Sum) %>%
    gather(key = key, value = value, N, PCT) %>%
    unite(variable, key, AP) %>% 
    spread(key = variable, value = value, fill = 0) 
  
  
  ### Bind rows
  df_AP <- bind_rows(df_AP_Subj, df_AP_Total) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, Subject_Category)
  
  
  
  ###'######################################################################
  ###'
  ###' (6) Average Class Size 
  ###' 
  ###'  2.1. Not by subject: 
  ###'  - Reflect teachers cross-subject assignment
  ###'  - Individual teacher's working conditions
  ###'  
  ###'  2.2. By subject
  ###'  - working conditions by each subject division within school
  ###'  - ELA, Math, and others 
  ###'
  ###'
  
  ### Total: Not by subject
  df_size_total <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode) %>%
    summarise(MN_size = mean(Enroll_total, na.rm = TRUE)) %>%
    mutate(Subject_Category = "Total")
  
  
  ### By subject
  df_size_subj <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, Subject_Category) %>%
    summarise(MN_size = mean(Enroll_total, na.rm = TRUE))
  
  
  ### Bind rows
  df_size <- bind_rows(df_size_subj, df_size_total) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, Subject_Category)
  
  
  
  ###'######################################################################
  ###'
  ###' (7) Average Class Size by UC_CSU_Approved
  ###'
  ###'
  
  ### Total: Not by subject
  df_size_AG_Approved_Total <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, UC_CSU_Approved) %>%
    summarise(MN_size = mean(Enroll_total, na.rm = TRUE)) %>%
    filter(!is.na(UC_CSU_Approved)) %>%
    mutate(UC_CSU_Approved = recode(UC_CSU_Approved, 
                                    '1' = "MN_size_Apprvd", 
                                    '0' = "MN_size_Not_Apprvd")) %>% 
    spread(key = UC_CSU_Approved, value = MN_size, fill = NA) %>%
    mutate(Subject_Category = "Total")

  
  ### By subject
  df_size_AG_Approved_Subj <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, Subject_Category, UC_CSU_Approved) %>%
    summarise(MN_size = mean(Enroll_total, na.rm = TRUE)) %>%
    filter(!is.na(UC_CSU_Approved)) %>%
    mutate(UC_CSU_Approved = recode(UC_CSU_Approved, 
                                    '1' = "MN_size_Apprvd", 
                                    '0' = "MN_size_Not_Apprvd")) %>% 
    spread(key = UC_CSU_Approved, value = MN_size, fill = NA)
  
  
  ### Bind rows
  df_size_AG_Approved <- bind_rows(df_size_AG_Approved_Subj, 
                                   df_size_AG_Approved_Total) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, Subject_Category)
  
  
  
  ###'######################################################################
  ###'
  ###' (8) Average Class Size by UCCSU_Requirements
  ###'
  ###'
  
  ### Total: Not by subject
  df_size_AG_Required_Total <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, UCCSU_Requirements) %>%
    summarise(MN_size = mean(Enroll_total, na.rm = TRUE)) %>%
    filter(!is.na(UCCSU_Requirements)) %>%
    spread(key = UCCSU_Requirements, value = MN_size, fill = NA) %>%
    mutate(Subject_Category = "Total")
  
  names(df_size_AG_Required_Total) <- gsub("A-G", "AG", names(df_size_AG_Required_Total))
  
  
  ### By subject
  df_size_AG_Required_Subj <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, Subject_Category, UCCSU_Requirements) %>%
    summarise(MN_size = mean(Enroll_total, na.rm = TRUE)) %>%
    filter(!is.na(UCCSU_Requirements)) %>%
    spread(key = UCCSU_Requirements, value = MN_size, fill = NA) 
  
  names(df_size_AG_Required_Subj) <- gsub("A-G", "AG", names(df_size_AG_Required_Subj))

  
  ### Bind rows
  df_size_AG_Required <- bind_rows(df_size_AG_Required_Subj, 
                                   df_size_AG_Required_Total) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, Subject_Category)
  
  names(df_size_AG_Required)[5:7] <- c("MN_size_Always_AG", 
                                       "MN_size_Sometimes_AG", 
                                       "MN_size_Not_AG")
  
  
  ###'######################################################################
  ###'
  ###' (9) Average Class Size by AP 
  ###'
  ###'
  
  ### Total: Not by subject
  df_size_AP_Total <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, AP) %>%
    summarise(MN_size = mean(Enroll_total, na.rm = TRUE)) %>%
    filter(!is.na(AP)) %>%
    mutate(AP = recode(AP, '1' = "MN_size_AP", '0' = "MN_size_NonAP")) %>% 
    spread(key = AP, value = MN_size, fill = NA) %>%
    mutate(Subject_Category = "Total")
  
  
  ### By subject
  df_size_AP_Subj <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, Subject_Category, AP) %>%
    summarise(MN_size = mean(Enroll_total, na.rm = TRUE)) %>%
    filter(!is.na(AP)) %>%
    mutate(AP = recode(AP, '1' = "MN_size_AP", '0' = "MN_size_NonAP")) %>% 
    spread(key = AP, value = MN_size, fill = NA) 
  
  
  ### Bind rows
  df_size_AP <- bind_rows(df_size_AP_Subj, 
                          df_size_AP_Total) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, Subject_Category)
  
  
  
  ###'######################################################################
  ###'
  ###' Merge all generated school-level summaries
  ###'
  ###'
  
  ### Collect as a list
  list_temp <- list(df_count, 
                    df_MNcount, 
                    df_AG_Approved, 
                    df_AG_Required, 
                    df_AP, 
                    df_size, 
                    df_size_AG_Approved, 
                    df_size_AG_Required, 
                    df_size_AP)
  
  
  ### Merge!
  key_match <- c("CountyCode", "DistrictCode", "SchoolCode", "Subject_Category")
  df_bind <- reduce(list_temp, full_join_track, by = key_match)
  
  
  ### Tag AcademicYear
  df_bind <- df_bind %>%
    mutate(AcademicYear = as.numeric(year_num)) %>%
    select(AcademicYear, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Embed into the list
  ###'
  ###'
  
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
###' Bind rows
###' 
###' 

df_result <- bind_rows(meta_list) %>%
  arrange(CountyCode, DistrictCode, SchoolCode, Subject_Category, AcademicYear) %>%
  select(CountyCode, DistrictCode, SchoolCode, Subject_Category, AcademicYear, 
         everything())
  

###'######################################################################
###'
###' Save the resulting dataset
###'
###'

setwd(data_dir)

dualsave(df_result, "df_Class Periods and Sizes_2003-2011")  


