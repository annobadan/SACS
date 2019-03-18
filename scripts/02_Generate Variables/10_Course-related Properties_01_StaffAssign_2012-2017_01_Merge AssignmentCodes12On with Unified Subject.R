
###'######################################################################
###'
###' Generate Variables
###' 
###' Course-related Properties 2012-2017
###' 
###' - Merge AssignmentCodes 2012-2017 with Unified Subject Naming
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
###' Import cleaned data files
###'
###'

setwd(data_dir)

### (1) Import Assignment Codes data: 2012-2017
load(file = "AssignmentCodes12On_cleaned.rda")
df_assign_code <- df %>%
  filter(!is.na(Assign_Code)) 

classmode(df_assign_code, everything())


### (2) Import unified subject name file: 2012-2017
df_unified <- read.csv(file = "AssignmentCodes_Subject_Naming_StaffAssign1217_Edited.csv")



###'######################################################################
###'
###' Prepare merge (1) df_assign_code
###'
###'

### Check out the duplicated keys: df_assign_code$Assign_Code
temp <- tabdf(df_assign_code, Assign_Code)
df_assign_code <- tag_ID_miss_dups(df_assign_code, match_key = c("Assign_Code"))


### Select only necessary variables
df_assign_code <- df_assign_code %>%
  select(-tag)



###'######################################################################
###'
###' Prepare merge (2) df_unified
###'
###'

### Check out the duplicated keys: df_unified$Assign_Subject
temp <- tabdf(df_unified, Assign_Subject)
df_unified <- tag_ID_miss_dups(df_unified, match_key = c("Assign_Subject"))


### Remove duplicates
df_unified <- distinct(df_unified) %>%
  dplyr::select(-tag)



###'######################################################################
###'
###' Merge (1) df_assign_code + df_unified = df_assign_code_unified
###'
###'

### Check distributions
tabdf(df_assign_code, Assign_Subject)
tabdf(df_unified, Assign_Subject)

classmode(df_assign_code, everything())
classmode(df_unified, everything())


### Merge!
df_assign_code_unified <- df_assign_code %>% 
  # mutate(Assign_Subject_Code = as.integer(Assign_Subject_Code)) %>%
  full_join_track(df_unified, 
                  by = c("Assign_Subject"), 
                  .merge = TRUE)


### Remove unmerged entries
df_assign_code_unified <- df_assign_code_unified %>%
  filter(!is.na(Assign_Code)) %>%
  dplyr::select(-.merge)



###'######################################################################
###'
###' Save the resulting codebook
###'
###'

setwd(data_dir)

dualsave(df_assign_code_unified, "AssignmentCodes12On_Unified Subject")

write.csv(df_assign_code_unified, "AssignmentCodes12On_Unified Subject.csv")




