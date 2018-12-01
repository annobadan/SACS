
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - Assignment Codes 2012-13 On
###' 
###' 2012-13, 2013-14, 2014-15, 2015-16, 2016-17, 2017-18
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
###' Import raw excel file
###'
###'

setwd(data_dir)

df <- read_excel("AssignmentCodes12On.xlsx", col_names = TRUE)

classmode(df, everything())


###'######################################################################
###'
###' Remove unnecessary variables
###' 
###' 

df <- df %>%
  select(-EffectiveStartDate, -EffectiveEndDate, -FileCreated)




###'######################################################################
###'
###' Change variable names:
###' 
###'
###'

names(df) <- c("Assign_Code", "Assign_Name", "Assign_Type", "Assign_Subject", 
               "AP", "IB", "CTE", "UCCSU_Requirements")

classmode(df, everything())



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
###' AP / IB / CTE: Generate dummy variables
###'
###'

tabdf(df, AP)
tabdf(df, IB)
tabdf(df, CTE)

df <- df %>%
  mutate_at(.vars = c("AP", "IB", "CTE"), 
            .funs = function(x){if_else(x == "Y", 1, 0)})



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
###' Save data objects
###' 
###' 

setwd(data_dir)

save(df, file = "AssignmentCodes12On_cleaned.rda")
write.dta(df, file = "AssignmentCodes12On_cleaned.dta")

