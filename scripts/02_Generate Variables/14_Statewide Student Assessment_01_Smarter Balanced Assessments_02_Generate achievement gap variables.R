
###'######################################################################
###'
###' Task    : Statewide Student Assessment. 
###'           01. Smarter BalancedSmarter Balanced Assessments (SBA)
###'           Generate achievement gap variables
###'           
###'           
###' Category: Generate Variables 
###' 
###' Data    : Statewide Student Assessment
###'           Smarter Balanced Assessments (SBA)
###'           splited panel dataframes
###'           2014-15 ~ 2017-18 
###'            
###'          
###' Date    : 2019-05-08
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


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' generate_gap_df():
###' 
###' Define a function to generate a dataframe with "Gap" between subgroups  
###'
###'


generate_gap_df <- function(df, 
                            factor_level, 
                            factor_label){
  
  ### Define an order of subgroup factor
  df_temp0 <- df %>%
    mutate(Subgroup = factor(Subgroup, 
                             levels = factor_level, 
                             labels = factor_label))
  
  
  ### Add PCT_Tested variables
  df_temp1 <- df_temp0 %>%
    group_by(CountyCode, DistrictCode, SchoolCode, AcademicYear) %>%
    mutate(N_Tested_Sum = sum(N_Tested, na.rm = TRUE), 
           N_with_Scores_Sum = sum(N_with_Scores, na.rm = TRUE), 
           PCT_Tested = 100*(N_Tested / N_Tested_Sum), 
           PCT_with_Scores = 100*(N_with_Scores / N_with_Scores_Sum)) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, AcademicYear) %>%
    dplyr::select(TestID:N_CAASPP_Enrollment_Reported, 
                  N_Tested_Sum, N_Tested, PCT_Tested, 
                  N_with_Scores_Sum, N_with_Scores, PCT_with_Scores, 
                  everything())
  
  
  ### Generate the "Gap" variables
  df_temp2 <- df_temp1 %>%
    gather(key = variable, value = value, 
           N_Tested, N_with_Scores, contains("PCT_"), 
           factor_key = TRUE) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, variable, AcademicYear, Subgroup) %>%
    group_by(CountyCode, DistrictCode, SchoolCode, AcademicYear, variable) %>%
    mutate(Gap = value - lag(value))
  
  
  ### Sort out only the calculated gap values 
  df_temp3 <- df_temp2 %>% 
    ungroup() %>%
    filter(Subgroup == factor_label[2]) %>%
    dplyr::select(TestID, Grade, Subgrp1, contains("Code"),  
                  TestYear, AcademicYear, 
                  N_Tested_Sum, N_with_Scores_Sum, 
                  variable, Gap) %>% 
    spread(key = variable, value = Gap)
  
  
  ### Rename Gap variables
  start <- which(names(df_temp3) == "N_Tested")
  end <- length(names(df_temp3))
  names(df_temp3)[start:end] <- paste0("Gap_", names(df_temp3)[start:end]) 
  
  return(df_temp3)
}



###'######################################################################
###'
###' Prepare the list for looping  
###' 
###' Define lists for the gap calculation   
###'
###'

### Define lists for the gap calculation 
list_01 <- list("02_Gender.rda", 
                c(3, 4), 
                c("Males", "Females"), 
                c("Males", "Females"))


list_02 <- list("05_Economic Status.rda", 
                c(31, 111), 
                c("Economically Disadvantaged", "Not Economically Disadvantaged"), 
                c("EconDis", "Not_EconDis"))


list_03 <- list("03_English-Language Fluency.rda", 
                c(160, 6), 
                c("English Learner", "Fluent-English Proficient and English Only"), 
                c("EL", "FEP"))


list_04 <- list("03_English-Language Fluency.rda", 
                c(160, 8), 
                c("English Learner", "Reclassified-Fluent English Proficient (RFEP)"), 
                c("EL", "REFP"))


list_05 <- list("06_Ethnicity.rda", 
                c(78, 80), 
                c("Hispanic or Latino", "White"), 
                c("Latino", "White"))


list_06 <- list("06_Ethnicity.rda", 
                c(74, 80), 
                c("Black or African American", "White"), 
                c("Black", "White"))


list_07 <- list("06_Ethnicity.rda", 
                c(76, 80), 
                c("Asian", "White"), 
                c("Asian", "White"))


list_08 <- list("08_Disability Status.rda", 
                c(128, 99), 
                c("Students with Disability", "Students with No Reported Disability"), 
                c("Disable", "Not_Disable"))


list_09 <- list("09_Ethnicity for Economically Disadvantaged.rda", 
                c(204, 206), 
                c("Hispanic or Latino", "White"), 
                c("Latino", "White"))


list_10 <- list("09_Ethnicity for Economically Disadvantaged.rda", 
                c(200, 206), 
                c("Black or African American", "White"), 
                c("Black", "White"))


list_11 <- list("09_Ethnicity for Economically Disadvantaged.rda", 
                c(202, 206), 
                c("Asian", "White"), 
                c("Asian", "White"))


### Save the list of these lists
list_gap_definitions <- list()

for (k in seq(1, 11)){
  
  list_gap_definitions[[k]] <- get(paste0("list_", sprintf("%02d", k)))
  
}

setwd(file.path(data_dir, "splitted_panel_df", "Gap_data"))
save(list_gap_definitions, file = "list_gap_definitions.rda")



###'######################################################################
###'
###' Execute looping
###'
###'

### Define subject vector
subject_vec <- c("SBA_ELA", "SBA_Math")


for (i in seq(1, 11)){  # Loop over pre-defined lists
  
  for (j in seq_along(subject_vec)){  # Loop over subjects
    
    ### Call the list
    list_temp <- get(paste0("list_", sprintf("%02d", i)))
    
    
    ### Load the managed dataset
    setwd(file.path(data_dir, "splitted_panel_df"))
    load(file = paste0(sprintf("%02d", j), "_", subject_vec[j], 
                       "_08_GR_All_", 
                       list_temp[[1]]))
    
    df <- df_to_save; rm(df_to_save)
    
    
    ### Generate the "Gap" dataset
    df_temp <- generate_gap_df(df, 
                               factor_level = list_temp[[2]], 
                               factor_label = list_temp[[3]])
    
    
    ### Save the generated dataset
    filename <- paste0("Gap_df_", 
                       sprintf("%02d", j), "_", subject_vec[j], 
                       "_08_GR_All_", 
                       sprintf("%02d", i), "_", 
                       str_trim(unique(df_temp$Subgrp1)), "_", 
                       list_temp[[4]][2], "-", list_temp[[4]][1])
    
    
    setwd(file.path(data_dir, "splitted_panel_df", "Gap_data"))
    dualsave(df_temp, filename)
    
    
    ### Print the progress
    cat(paste0(filename, "\n"))
    
  }
}


