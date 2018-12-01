
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - Assignment Codes Before 2012
###' 
###' 2003-04 ~ 2011-12  (9 fiscal years)
###' 
###'  
###' Just clean raw file - Manipulate variable later
###' 
###' 
###' 20170707 JoonHo Lee
###' 20180928 JoonHo Lee
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
library(readxl)
library(Hmisc)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import AssignmentCodes Reference data: 
###' 
###' AssignmentCodes12On 2012-2017
###'
###'

setwd(data_dir)

load(file = "AssignmentCodes12On_cleaned.rda")

assign_codes_df_2012 <- df; rm(df)



###'######################################################################
###'
###' Import raw text file:
###' 
###' AssignmentCodes11before 2003-2011
###'
###'

setwd(data_dir)

### Import tab delimited text file
df <- read.delim(file = "AssignmentCodes11before.txt", 
                 header = FALSE, 
                 colClasses = "character")

classmode(df, everything())


### Convert empty strings to NA
df <- df %>% mutate_all(.funs = empty_as_na)



###'######################################################################
###'
###' Rename variables
###'
###'

names(df) <- c("Assign_Code", "Assign_Name", "Assign_Type", 
               "Assign_Subject_Code", "Assign_Subject", 
               "Topic_Code", "Topic_Name", 
               "Start_Date", "Last_Date", 
               "UCCSU_Requirements", "Comments")



###'######################################################################
###'
###' Assignment Type:
###' 
###' Convert to a factor
###'
###'

tabdf(df, Assign_Type)

df <- df %>%
  mutate(Assign_Type = factor(Assign_Type, 
                              levels = c("T", "A", "P"), 
                              labels = c("Teacher", 
                                         "Administrator", 
                                         "Pupil_Services")))



###'######################################################################
###'
###' Assign Subject
###'
###'

tbl_sub_11 <- tabdf(df, Assign_Subject)

tbl_sub_12 <- tabdf(assign_codes_df_2012, Assign_Subject)

tbl_sub_11$Assign_Subject

tbl_sub_12$Assign_Subject



###'######################################################################
###'
###' Topic Subject
###'
###'

tabdf(df, Topic_Name)



###'######################################################################
###'
###' Meets UC/CSU Requirements
###' 
###' Y = always or almost always approved as UC "a-g" course 
###' N = cannot be approved as UC "a-g" course
###' U = sometimes approved as UC "a-g" course
###' Blank = not a teaching assignment
###'
###'

classmode(df, everything())
tabdf(df, UCCSU_Requirements)

df <- df %>%
  mutate(UCCSU_Requirements = factor(UCCSU_Requirements, 
                                     levels = c("Y", "U", "N"), 
                                     labels = c("Always_A-G", 
                                                "Sometimes_A-G", 
                                                "Not_A-G")))



###'######################################################################
###'
###' Remove unnecessary variables
###' 
###' 

df <- df %>%
  select(-Start_Date, -Last_Date)



###'######################################################################
###' 
###' Save data objects
###' 
###' 

setwd(data_dir)

save(df, file = "AssignmentCodes11before_cleaned.rda")
write.dta(df, file = "AssignmentCodes11before_cleaned.dta")

