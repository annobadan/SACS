
###'######################################################################
###'
###' Task    : Statewide Student Assessment. 
###'           01. Smarter BalancedSmarter Balanced Assessments (SBA)
###'           Split into subgroup panel datasets
###'           
###' Category: Generate Variables 
###' 
###' Data    : Statewide Student Assessment
###'           Smarter Balanced Assessments (SBA)
###'           Cleaned "All Subgroups" Data
###'           2014-15 ~ 2017-18 
###'            
###'          
###' Date    : 2019-04-28
###' 
###' Author  : JoonHo Lee (joonho@berkeley.edu)
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
data_dir <- c("D:/Data/LCFF/Statewide_Student_Assessment/CAASPP/Smarter Balanced Assessments")


### Call libraries
library(tidyverse)
library(readxl)
library(Hmisc)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Load "All Subgroups" Datasets
###'
###'

setwd(data_dir)


### Academic Year 2014-15
load(file = "sb_ca1415_All_Subgroups_cleaned.rda")
df1415 <- df; rm(df)


### AcademicYear 2015-16
load(file = "sb_ca1516_All_Subgroups_cleaned.rda")
df1516 <- df; rm(df)


### AcademicYear 2016-17
load(file = "sb_ca1617_All_Subgroups_cleaned.rda")
df1617 <- df; rm(df)


### AcademicYear 2017-18
load(file = "sb_ca1718_All_Subgroups_cleaned.rda")
df1718 <- df; rm(df)


### Embed as a list 
list_rawdf <- list(df1415, df1516, df1617, df1718)



###'######################################################################
###'
###' Prepare loop
###' 
###'

### Check variable names of each year's raw dataframe
lapply(list_rawdf, names)


###' Check the subcategories of 
###' "Subject" (TestID), 
###' "Grade", "Subgroup1"
###' It seems that all the subcategories are the same across years
map(list_rawdf, "TestID") %>% map(table)
map(list_rawdf, "Grade") %>% map(table)
map(list_rawdf, "Subgrp1") %>% map(table)  


###' Extract/Define subcategories to use in loops
subject_vec <- c("SBA_ELA", "SBA_Math")

grade_vec <- c(paste0("GR", c(3:8, 11, "_All")))

subgrp_vec <- unique(df1415$Subgrp1)


### Prepare an empty list 
list_collect <- list()



###'######################################################################
###'
###' Start a loop 
###' 
###'
###'

counter <- 0

for (i in seq_along(subject_vec)){
  
  for (j in seq_along(grade_vec)){
    
    for (k in seq_along(subgrp_vec)){
      
      
      ###'######################################################################
      ###'
      ###' Add a counter
      ###'
      ###'
      
      counter <- counter + 1
      
      
      ###'######################################################################
      ###'
      ###' Extract loop elements
      ###'
      ###'
      
      subject <- subject_vec[i]
      
      grade <- grade_vec[j]
      
      subgrp <- subgrp_vec[k]
      
      
      
      ###'######################################################################
      ###'
      ###' Filter based on (1) Subject
      ###'
      ###'
      
      ### Define a function for filtering
      filter_subject <- function(df){
        df %>% filter(TestID == subject)
      }
      
      
      ### Filter
      list_temp <- map(list_rawdf, filter_subject)
      
      
      ### Check
      map(list_temp, "TestID") %>% map(table)
      
      
      
      ###'######################################################################
      ###'
      ###' Change the specific subarea variable names based on subject
      ###' 
      ###' (1) ELA:
      ###' 
      ###' Area 1. Reading, 
      ###' Area 2. Writing, 
      ###' Area 3. Listening, 
      ###' Area 4. Researchs/Inquiry
      ###' 
      ###' (2) Math:
      ###' 
      ###' Area 1. Cocepts and Procedures, 
      ###' Area 2. Problem Solving/Modeling and Data Analysis, 
      ###' Area 3. Communicating Reasoning
      ###' 
      ###' *** Area 4 of Math doesn't exist. 
      ###'     But in the raw data, they are coded as zero. 
      ###'     Just remove the columns
      ###' 
      ###'
      
      ### Check variable names
      map(list_temp, names)
      
      
      ### Define a function 
      rename_areas <- function(df){
        
        if (as.character(unique(df$TestID)) == "SBA_ELA"){
         
          names(df) <- names(df) %>%
            str_replace("Area1", "Reading") %>%
            str_replace("Area2", "Writing") %>%
            str_replace("Area3", "Listening") %>%
            str_replace("Area4", "Research") 
          
          return(df)
          
        } else if (as.character(unique(df$TestID)) == "SBA_Math"){
          
          df <- df %>%
            select(-contains("Area4"))
          
          names(df) <- names(df) %>%
            str_replace("Area1", "Concepts") %>%
            str_replace("Area2", "ProbSolving") %>%
            str_replace("Area3", "Reasoning")
          
          return(df)
        }
      }
      
      
      ### Apply the function
      list_temp <- map(list_temp, rename_areas)
      map(list_temp, names)
      
      
      
      
      ###'######################################################################
      ###'
      ###' Filter based on (2) Grade 
      ###'
      ###'
      
      ### Define a function
      filter_grade <- function(df){
        df %>% filter(Grade == grade)
      }
      
      
      ### Filter
      list_temp <- map(list_temp, filter_grade)
      
      
      ## Check
      map(list_temp, "Grade") %>% map(table)
      
      
      
      ###'######################################################################
      ###'
      ###' Filter based on (3) Subgroup 
      ###'
      ###'
      
      ### Define a function
      filter_subgrp <- function(df){
        df %>% filter(Subgrp1 == subgrp)
      }
      
      
      ### Filter
      list_temp <- map(list_temp, filter_subgrp)
      
      
      ## Check
      map(list_temp, "Subgrp1") %>% map(table)
      
      
      
      ###'######################################################################
      ###'
      ###' Append all 4 years datasets
      ###'
      ###'
      
      ###' Generate "AcademicYear" variable out of "TestYear"
      ###' TestYear - 1 = AcademicYear
      add_AcdemicYear <- function(df){
        
        df %>%
          mutate(AcademicYear = TestYear - 1) %>%
          select(contains("Code"), TestYear, AcademicYear, everything())
        
      }
      
      
      list_temp <- map(list_temp, add_AcdemicYear)
      
      
      ###' Append all 4 years datasets
      ###' Check whether all variables have the same meaning across years
      map(list_temp, names)
      
      df_bind <- bind_rows(list_temp) %>%
        dplyr::select(TestID, Grade, 
                      contains("Code"), 
                      Subgroup, Subgrp1, Subgrp2, 
                      TestYear, AcademicYear, 
                      everything()) %>%
        arrange(CountyCode, DistrictCode, SchoolCode, 
                Subgrp2, AcademicYear)
      
      
      
      ###'######################################################################
      ###'
      ###' Generate variables
      ###' 
      ###' Test Participation Rate
      ###'
      ###' => Not possible for AcademicYear 2014-15
      ###'    because N_CAASPP_Enrollment is not provided for each subgroup
      ###'    
      ###'

      
      
      ###'######################################################################
      ###'
      ###' Save the resulting dataframe
      ###'
      ###'
      
      setwd(paste0(data_dir, "/splitted_panel_df"))
      
      filename <- paste(sprintf("%02d", i), subject,
                        sprintf("%02d", j), grade, 
                        sprintf("%02d", k), str_trim(subgrp), 
                        sep = "_")
      
      dualsave(df_bind, filename)
      
      
      
      ###'######################################################################
      ###'
      ###' Embed into the list & name the list components
      ###'
      ###'
      
      list_collect[[counter]] <- df_bind
      
      names(list_collect)[counter] <- filename
      
      
      
      ###'######################################################################
      ###' 
      ###' Print the progress  
      ###' 
      
      cat(paste0(filename, "\n"))
      
      
    }  ### End of loop over k => subgroups
  }    ### End of loop over j => grades
}      ### End of loop over i => subjects



###'######################################################################
###' 
###' Save the splited panel data list
###' 
###' 

setwd(data_dir)

save(list_collect, file = "SBAC_Splitted Subgroup Panel Data 2015-17.rda")











