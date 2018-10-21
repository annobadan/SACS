
###'######################################################################
###'
###' Generate Variables
###' 
###' School-level Student Compositions 
###' 
###' - with Enrollment Data 1993-2017
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character/01_Enrollment")


### Call libraries
library(tidyverse)
library(foreign)
library(rlang)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Loop over years
###'
###'

years <- c(sprintf("%02d",seq(93, 99)), sprintf("%02d",seq(00, 17)))


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
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Import cleaned enrollment data
  load(file = paste0("enr", year_num, "_cleaned", ".rda"))
  classmode(df, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Dataframe #1. Student Gender Composition
  ###'
  ###'
  
  tabdf(df, GENDER)
  classmode(df, GENDER)
  levels(df$GENDER)
  levels_to_replace <- c("male", "female")
  
  df_Gender <- school_composition_sum(df, 
                                      var_to_sum = ENR_TOTAL, 
                                      factor = GENDER, 
                                      levels_to_replace = levels_to_replace, 
                                      table_name = "Student Gender", 
                                      year = year_num)
  
  
  
  ###'######################################################################
  ###'
  ###' Dataframe #2.  Student Race/Ethnicity Composition
  ###' 
  ###'
  
  tabdf(df, ETHNIC)
  classmode(df, ETHNIC)
  levels(df$ETHNIC)
  levels_to_replace <- c("Not_Reported", 
                         "Native", "Asian", "Pacific", "Filipino", "Latino", 
                         "Black", "White", "Multi_or_No", "Two_more")
  
  df_Ethnic <- school_composition_sum(df, 
                                      var_to_sum = ENR_TOTAL, 
                                      factor = ETHNIC, 
                                      levels_to_replace = levels_to_replace, 
                                      table_name = "Student Ethnicity", 
                                      year = year_num)
  
  
  
  ###'######################################################################
  ###'
  ###' Dataframe #3.  Student Grade Composition
  ###' 
  ###'
  
  classmode(df, everything())
  
  ### Define levels to replace
  idx_start <- which(names(df) == "KDGN")
  idx_end <- which(names(df) == "UNGR_SEC")
  idx_plus <- which(names(df) == "ADULT")
  
  levels_to_replace <- names(df)[c(idx_start:idx_end, idx_plus)]
  
  
  ### Summarise data to long format
  df_Grade_long <- df %>%
    select(ends_with("Code"), 
           KDGN:UNGR_SEC, ADULT, ENR_TOTAL) %>%
    gather(Grade, ENR_SUB, KDGN:ADULT) %>% 
    mutate(Grade = factor(Grade, 
                          levels = levels_to_replace)) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, Grade)
  
  
  ### Generate table using school_composition_sum()
  df_Grade <- school_composition_sum(df_Grade_long, 
                                     var_to_sum = ENR_SUB, 
                                     factor = Grade, 
                                     levels_to_replace = levels_to_replace, 
                                     table_name = "Student Grade", 
                                     year = year_num)
  
  
  
  ###'######################################################################
  ###' 
  ###' Save data objects
  ###' 
  ###' 
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Collect and save all the resulting dataframes as list
  list_collect_df <- list(df_Gender,
                          df_Ethnic,
                          df_Grade)
  
  names(list_collect_df) <- c("df_Gender", 
                              "df_Ethnic", 
                              "df_Grade")
  
  save(list_collect_df, 
       file = paste0("list_school-level_student_composition_01_Enrollment", 
                     year_num, ".rda"))
  
  
  ### Save individual dataframes within the list
  for (i in seq_along(names(list_collect_df))){
    
    ### Extract the name of data frame => add fiscal year
    filename <- paste0(names(list_collect_df)[i], year_num)
    
    ### Extract dataframe
    df_save <- list_collect_df[[i]]
    
    ### Save the resulting dataframe
    save(df_save, file = paste0(filename, ".rda"))
    write.dta(df_save, file = paste0(filename, ".dta"))
    
  }
  
  
  
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

