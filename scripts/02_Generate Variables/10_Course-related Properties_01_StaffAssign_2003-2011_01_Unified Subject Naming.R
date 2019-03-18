
###'######################################################################
###'
###' Generate Variables
###' 
###' Unified and Coherent Subject Naming
###' 
###' - with "Staff_Cred", "StaffAssign", and "CoursesTaught" data 
###' 
###' 
###' 20181218 JoonHo Lee
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
###' Extract Subject Information
###' 
###' (1) StaffCred 2003-2008
###'
###'

years <- sprintf("%02d",seq(03, 08))

meta_list <- list()

for (i in seq_along(years)) {
  
  ### Assign year number
  year_num <- years[i]
  
  
  ### Import cleaned file
  setwd(data_dir1)
  load(file = paste0("StaffCred", year_num, "_cleaned", ".rda"))
  df<- df_Cred_long
  
  
  ### Get distinct table
  names(df)
  tbl_authr <- df %>%
    dplyr::select(AuthorizationType_Key, AuthorizationType_Name) %>%
    distinct()
  
  meta_list[[i]] <- tbl_authr
  
}

df_StaffCred0308 <- reduce(meta_list, full_join, by = c("AuthorizationType_Key"))

df_StaffCred0308 <- reduce(meta_list, union)



###'######################################################################
###'
###' Extract Subject Information
###' 
###' (2) StaffCred 2012-2017
###'
###'

years <- sprintf("%02d",seq(12, 17))

meta_list <- list()

for (i in seq_along(years)) {
  
  ### Assign year number
  year_num <- years[i]
  
  
  ### Import cleaned file
  setwd(data_dir1)
  load(file = paste0("StaffCred", year_num, "_cleaned", ".rda"))
  
  
  ### Get distinct table
  names(df)
  tbl_authr <- df %>%
    dplyr::select(AuthorizationType, AuthorizationType_Name) %>%
    distinct() %>%
    filter(!is.na(AuthorizationType))
  
  meta_list[[i]] <- tbl_authr
  
}

df_StaffCred1217 <- reduce(meta_list, full_join, by = c("AuthorizationType"))

df_StaffCred1217 <- reduce(meta_list, union)



###'######################################################################
###'
###' Extract Subject Information
###' 
###' (3) StaffAssign 2003-2011
###'
###'

years <- c(sprintf("%02d", seq(3, 8)), sprintf("%02d", seq(10, 11)))

meta_list <- list()

for (i in seq_along(years)) {
  
  ### Assign year number
  year_num <- years[i]
  
  
  ###'######################################################################
  ###'
  ###' Import cleaned data files
  ###'
  ###'
  
  ## Import Assignment Codes data: 2003-2011
  setwd(data_dir2)
  load(file = "AssignmentCodes11before_cleaned.rda")
  df_assign_code <- df %>%
    filter(!is.na(Assign_Code)) 
  
  classmode(df_assign_code, everything())
  
  
  ### Check out the duplicated keys: df_assign_code$Assign_Code
  n_distinct(df_assign_code$Assign_Code)  # 962
  nrow(df_assign_code)  # 1030
  
  idx <- duplicated(df_assign_code$Assign_Code)
  assign_code_duplicated <- df_assign_code[idx, ]
  write.csv(assign_code_duplicated, file = "AssignmentCodes11before_duplicated.csv")
  
  
  ### Remove the duplicated keys
  df_assign_code <- df_assign_code[!idx, ]
  
  
  ### Load StaffAssign data
  setwd(data_dir2)
  load(file = paste0("assign", year_num, "_cleaned", ".rda"))
  names(df)
  
  classmode(df, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Merge Assignment codes to StaffAssign data
  ###'
  ###'
  
  ### Merge
  df_temp <- df %>%
    dplyr::select(-Assign_Type) %>%
    left_join(df_assign_code, by = c("Assign_Code"))
  
  
  ### Look into the merged dataset
  tabdf(df_temp, UC_CSU_Approved)
  tabdf(df_temp, UCCSU_Requirements)
  tabdf(df_temp, Assign_Type)
  tabdf(df_temp, Assign_Subject)
  tabdf(df_temp, Topic_Name)
  
  df_temp_topic_subject <- df_temp %>%
    group_by(Topic_Name, Assign_Subject) %>%
    summarise(N = n())
  
  
  ### Get distinct table
  names(df_temp)
  tbl_authr <- df_temp %>%
    dplyr::select(Assign_Subject_Code, Assign_Subject) %>%
    distinct() 
  
  meta_list[[i]] <- tbl_authr
  
}

df_StaffAssign0311 <- reduce(meta_list, full_join, by = c("Assign_Subject_Code"))

df_StaffAssign0311 <- reduce(meta_list, union)




###'######################################################################
###'
###' Extract Subject Information
###' 
###' (4) StaffAssign 2012-2017
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
  
  setwd(data_dir2)
  
  ### Load StaffAssign data
  load(file = paste0("StaffAssign", year_num, "_cleaned", ".rda"))
  names(df)
  
  classmode(df, everything())
  
  
  ### Get distinct table
  names(df)
  tbl_authr <- df %>%
    dplyr::select(Assign_Subject) %>%
    distinct() 
  
  meta_list[[i]] <- tbl_authr

}

df_StaffAssign1217 <- reduce(meta_list, union)



###'######################################################################
###'
###' Save as .csv file
###'
###'

setwd(data_dir2)

write.csv(df_StaffCred0308, file = "AssignmentCodes_Subject_Naming_StaffCred0308.csv")
write.csv(df_StaffCred1217, file = "AssignmentCodes_Subject_Naming_StaffCred1217.csv")

write.csv(df_StaffAssign0311, file = "AssignmentCodes_Subject_Naming_StaffAssign0311.csv")
write.csv(df_StaffAssign1217, file = "AssignmentCodes_Subject_Naming_StaffAssign1217.csv")


