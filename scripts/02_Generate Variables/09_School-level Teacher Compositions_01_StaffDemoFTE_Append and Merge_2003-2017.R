
###'######################################################################
###'
###' Data Management 
###' 
###' School-level Teacher Compositions 2003-2017
###' 
###' Generate Longitudinal Datasets (merge + bind_rows)
###' 
###' 
###' 20181007 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Staff_Data/Certificated_Staff/Staff_Demographic/school-level teacher composition")


### Call libraries
library(tidyverse)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import the cleaned lists
###'
###'

### Set data containing working directory
setwd(data_dir)


### Loop over years
years <- sprintf("%02d",seq(03, 17))

for (i in seq_along(years)){
  
  ### Assign year
  year_num <- years[i]

  ### Load the cleaned list
  load(file = paste0("list_school-level_teacher_composition_", year_num, ".rda"))
  
  ### Assign year-specified list name
  assign(paste0("list", year_num), list_collect_df)
  
}



###'######################################################################
###'
###' Append data frames over years by data frames
###'
###'

years <- sprintf("%02d",seq(03, 17))
df_names <- names(list17)
list_teacher_composition <- list()

for (j in seq_along(df_names)){
  
  ### Prepare empty list to collect data frames
  meta_list <- list()
  
  
  ### Collect variable specific dataframes over years
  for (i in seq_along(years)){
    
    # Extract list by year
    year_num <- years[i]
    list_temp <- get(paste0("list", year_num))
    
    # Extract dataframe by df_name
    df_name <- df_names[j]
    df <- list_temp[[df_name]]
    
    # Append to pre-defined dataframe
    meta_list[[i]] <- df
  }
  
  
  ### Bind rows!
  df_bind <- bind_rows(meta_list)
  
  ### Arrange by school
  df_bind <- df_bind %>%
    arrange(CountyCode, DistrictCode, SchoolCode, 
            table, AcademicYear)
  
  
  ### Assign the names of dataframes
  assign(paste0(names(list17)[j]), df_bind)

}



###'######################################################################
###'
###' Prepare merge
###'
###'

merge_key <- c("CountyCode", "DistrictCode", "SchoolCode", "AcademicYear")
sumstats <- c("N", "mean", "median", "sd", "min", "max")


### (1) df_StaffType
df_StaffType_to_merge <- df_StaffType %>%
  rename(N_Total_Staffs = subtotal) %>%
  select(-table)


### (2) df_Gender
df_Gender_to_merge <- df_Teacher_Gender %>%
  rename(N_Total_Teachers = subtotal) %>%
  select(-table)

names(df_Gender_to_merge)[-(1:5)] <- paste0(names(df_Gender_to_merge)[-(1:5)], "_Tch")


### (3) df_Race
df_Race_to_merge <- df_Teacher_Race %>%
  select(-table, -subtotal)

names(df_Race_to_merge)[-(1:4)] <- paste0(names(df_Race_to_merge)[-(1:4)], "_Tch")


### (4) df_Education
df_Education_to_merge <- df_Teacher_Education %>%
  select(-table, -subtotal)


### (5) df_Years_Experience
df_Years_to_merge <- df_Teacher_Years %>%
  gather(key = key, value = value, N:max) %>%
  mutate(table = recode(table, 
                        "Years Teaching" = "yrs_teach", 
                        "Years in District" = "yrs_in_dist")) %>%
  unite(key_wide, key, table) %>%
  mutate(key_wide = factor(key_wide, levels = c(paste0(sumstats, "_yrs_teach"), 
                                                paste0(sumstats, "_yrs_in_dist")))) %>%
  spread(key = key_wide, value = value)
  

### (6) df_New_Teachers
new_vars <- c("N_New", "PCT_New")

df_New_to_merge <- df_Teacher_New %>%
  select(-subtotal, -ends_with("Not_New")) %>%
  gather(key = key, value = value, new_vars) %>%
  mutate(table = recode(table, 
                        "New Teaching" = "teach", 
                        "New in District" = "in_dist")) %>%
  unite(key_wide, key, table) %>%
  mutate(key_wide = factor(key_wide, levels = c(paste0(new_vars, "_teach"), 
                                                paste0(new_vars, "_in_dist")))) %>%
  spread(key = key_wide, value = value)


### (7) df_EmployStatus
df_EmployStatus_to_merge <- df_Teacher_EmployStatus %>%
  select(-table, -subtotal)


### (8) df_FTE
df_FTE_to_merge <- df_FTE %>%
  gather(key = key, value = value, N:max) %>%
  mutate(table = recode(table, 
                        "FTE_All Staffs" = "FTE_staff", 
                        "FTE_Teachers" = "FTE_teacher")) %>%
  unite(key_wide, key, table) %>%
  mutate(key_wide = factor(key_wide, levels = c(paste0(sumstats, "_FTE_staff"), 
                                                paste0(sumstats, "_FTE_teacher")))) %>%
  spread(key = key_wide, value = value)



###'######################################################################
###'
###' Merge multiple datasets
###'
###'

### Collect datasets as a list
meta_list <- list(df_StaffType_to_merge, 
                  df_Gender_to_merge, 
                  df_Race_to_merge, 
                  df_Education_to_merge, 
                  df_Years_to_merge, 
                  df_New_to_merge, 
                  df_EmployStatus_to_merge, 
                  df_FTE_to_merge)


### Merge the datasets
df_teacher_composition_0317 <- reduce(meta_list, full_join, 
                                      by = c("CountyCode", "DistrictCode", 
                                             "SchoolCode", "AcademicYear"))


### Save the resulting dataframe
setwd(data_dir)
dualsave(df_teacher_composition_0317, 
         "df_teacher_composition_0317")
