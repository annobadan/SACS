
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
###' 
###'
###'








###'######################################################################
###'
###' (1) SBA_ELA / GR_ALL
###' 
###' Gap = Not Economically Disadvantaged - Economically Disadvantaged
###'
###'

### Load the managed dataset
setwd(file.path(data_dir, "splitted_panel_df"))
load(file = "01_SBA_ELA_08_GR_All_05_Economic Status.rda")
df <- df_to_save; rm(df_to_save)


###' Define factor order, levels, and labels
###' This is important to define the "Gap" variables
tabdf(df, Subgroup)
factor_level <- c(31, 111)
factor_label <- c("Economically Disadvantaged", 
                  "Not Economically Disadvantaged")


### Generate the "Gap" dataset
df_temp <- generate_gap_df(df, 
                           factor_level = factor_level, 
                           factor_label = factor_label)

### Save the generated dataset
filename <- paste("Gap_df", 
                  unique(df_temp$TestID),
                  unique(df_temp$Grade), 
                  str_trim(unique(df_temp$Subgrp1)), 
                  sep = "_")

setwd(file.path(data_dir, "splitted_panel_df", "Gap_df"))
dualsave(df_temp, filename)



###'######################################################################
###'
###' (2) SBA_Math / GR_ALL
###' 
###' Gap = Not Economically Disadvantaged - Economically Disadvantaged
###'
###'

### Load the managed dataset
setwd(file.path(data_dir, "splitted_panel_df"))
load(file = "02_SBA_Math_08_GR_All_05_Economic Status.rda")
df <- df_to_save; rm(df_to_save)


### Define factor order, levels, and labels
tabdf(df, Subgroup)
factor_level <- c(31, 111)
factor_label <- c("Economically Disadvantaged", 
                  "Not Economically Disadvantaged")


### Generate the "Gap" dataset
df_temp <- generate_gap_df(df, 
                           factor_level = factor_level, 
                           factor_label = factor_label)

### Save the generated dataset
filename <- paste("Gap_df", 
                  unique(df_temp$TestID),
                  unique(df_temp$Grade), 
                  str_trim(unique(df_temp$Subgrp1)), 
                  sep = "_")

setwd(file.path(data_dir, "splitted_panel_df", "Gap_df"))
dualsave(df_temp, filename)



###'######################################################################
###' 
###' (3) SBA_ELA / GR_ALL
###' 
###' Gap = English Only - English Learner
###'
###'

### Load the managed dataset
setwd(file.path(data_dir, "splitted_panel_df"))
load(file = "01_SBA_ELA_08_GR_All_03_English-Language Fluency.rda")
df <- df_to_save; rm(df_to_save)


### Define factor order, levels, and labels
tabdf(df, Subgroup)
tabdf(df, Subgrp2)
factor_level <- c(160, 180)
factor_label <- c("English Learner", 
                  "English Only")


### Generate the "Gap" dataset
df_temp <- generate_gap_df(df %>% filter(Subgroup %in% factor_level), 
                           factor_level = factor_level, 
                           factor_label = factor_label)


### Save the generated dataset
filename <- paste("Gap_df", 
                  unique(df_temp$TestID),
                  unique(df_temp$Grade), 
                  str_trim(unique(df_temp$Subgrp1)), 
                  sep = "_")

setwd(file.path(data_dir, "splitted_panel_df", "Gap_df"))
dualsave(df_temp, filename)



###'######################################################################
###' 
###' (4) SBA_Math / GR_ALL
###' 
###' Gap = English Only - English Learner
###'
###'

### Load the managed dataset
setwd(file.path(data_dir, "splitted_panel_df"))
load(file = "02_SBA_Math_08_GR_All_03_English-Language Fluency.rda")
df <- df_to_save; rm(df_to_save)


### Define factor order, levels, and labels
tabdf(df, Subgroup)
tabdf(df, Subgrp2)
factor_level <- c(160, 180)
factor_label <- c("English Learner", 
                  "English Only")


### Generate the "Gap" dataset
df_temp <- generate_gap_df(df %>% filter(Subgroup %in% factor_level), 
                           factor_level = factor_level, 
                           factor_label = factor_label)


### Save the generated dataset
filename <- paste("Gap_df", 
                  unique(df_temp$TestID),
                  unique(df_temp$Grade), 
                  str_trim(unique(df_temp$Subgrp1)), 
                  sep = "_")

setwd(file.path(data_dir, "splitted_panel_df", "Gap_df"))
dualsave(df_temp, filename)

