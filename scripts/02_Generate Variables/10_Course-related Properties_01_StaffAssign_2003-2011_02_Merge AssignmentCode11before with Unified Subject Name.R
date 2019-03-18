
###'######################################################################
###'
###' Generate Variables
###' 
###' Course-related Properties 2003-2011
###' 
###' - Merge AssignmentCodes 2003-2011 with Unified Subject Naming
###' 
###' 
###' 20181227 JoonHo Lee
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

### (1) Import Assignment Codes data: 2003-2011
load(file = "AssignmentCodes11before_cleaned.rda")
df_assign_code <- df %>%
  filter(!is.na(Assign_Code)) 

classmode(df_assign_code, everything())


### (2) Import unified subject name file: 2003-2011
df_unified <- read.csv(file = "AssignmentCodes_Subject_Naming_StaffAssign0311_Edited.csv")



###'######################################################################
###'
###' Prepare merge (1) df_assign_code
###'
###'

### Check out the duplicated keys: df_assign_code$Assign_Code
temp <- tabdf(df_assign_code, Assign_Code)
df_assign_code <- tag_ID_miss_dups(df_assign_code, match_key = c("Assign_Code"))


### Remove "Renamed" entries => now we have unduplicated Assign_Codes
idx <- grepl("Renamed", df_assign_code$Comments)
sum(idx)
df_assign_code_undup <- df_assign_code[!idx, ]
nrow(df_assign_code_undup)
df_assign_code_undup <- tag_ID_miss_dups(df_assign_code_undup, 
                                         match_key = c("Assign_Code"))


### Select only necessary variables
names(df_assign_code_undup)
df_assign_code <- df_assign_code_undup %>%
  dplyr::select(Assign_Code:UCCSU_Requirements)



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
  mutate(Assign_Subject_Code = as.integer(Assign_Subject_Code)) %>%
  full_join_track(df_unified, 
                  by = c("Assign_Subject_Code", "Assign_Subject"), 
                  .merge = TRUE)


### Remove unmerged entries
df_assign_code_unified <- df_assign_code_unified %>%
  filter(!is.na(Assign_Code)) %>%
  dplyr::select(-.merge)



###'######################################################################
###'
###' Remove still remaining duplicates
###' 
###'

### Check duplicated IDs
df_assign_code_unified <- tag_ID_miss_dups(df_assign_code_unified, 
                                           match_key = c("Assign_Type", "Assign_Code"))


### Split into two parts by tag
tabdf(df_assign_code_unified, tag)

df_valid <- df_assign_code_unified %>%
  filter(tag == "ID_valid") %>%
  arrange(Assign_Code)

df_dup <- df_assign_code_unified %>%
  filter(tag == "ID_duplicated") %>%
  arrange(Assign_Code) %>%
  group_by(Assign_Code) %>%
  filter(row_number() == n())


### Bind rows
df_assign_code_bind <- bind_rows(df_valid, df_dup) %>%
  arrange(Assign_Code) %>%
  dplyr::select(-tag)



###'######################################################################
###'
###' Save the resulting codebook
###'
###'

setwd(data_dir)

dualsave(df_assign_code_bind, "AssignmentCodes11before_Unified Subject")

write.csv(df_assign_code_bind, "AssignmentCodes11before_Unified Subject.csv")




