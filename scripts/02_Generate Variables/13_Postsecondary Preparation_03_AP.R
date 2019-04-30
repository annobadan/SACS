
###'######################################################################
###'
###' Task    : Postsecondary Preparation 03. AP
###'           
###' Category: Generate Variables 
###' 
###' Data    : Postsecondary Preparation AP data 1998-1999 ~ 2017-2018
###' 
###' Date    : 2019-04-26
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
data_dir <- c("D:/Data/LCFF/School_Performance/Postsecondary_Preparation/AP")


### Call libraries
library(tidyverse)
library(readxl)
library(Hmisc)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Append only School-level data
###'
###'

### Set an empty list to collect data frames
meta_list <- list()


### Loop over years: 9899-1718 
years <- c(sprintf("%02d", seq(98, 99)), sprintf("%02d", seq(0, 17))) 

for (i in seq_along(years)) {
  
  
  ###'######################################################################
  ###'
  ###' Assign year number
  ###'
  ###'
  
  year_num <- years[i]
  
  
  
  ###'######################################################################
  ###'
  ###' Import the cleaned .rda file
  ###'
  ###'
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Assign filenames
  if (year_num != c("17")){
    
    filename <- paste0("ap", years[i], years[i + 1]) 
    
  } else if (year_num == c("17")){
    
    filename <- paste0("ap", year_num, as.numeric(year_num) + 1)
    
  }
  
  
  ### Import .rda file
  load(file = paste0(filename, "_cleaned.rda"))
  classmode(df, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Encode structural missingness: When N_ExamTakers == 0
  ###'
  ###'
  
  rows_str_miss <- df$N_ExamTakers == 0 
  
  idx <- names(df) %in% c(names(df)[grep("N_Exams_Scr", names(df))], 
                          "N_Exams_3above")
  
  cols_str_miss <- names(df)[idx]  
  
  df[rows_str_miss, cols_str_miss] <- NA
  
  
  
  ###'######################################################################
  ###'
  ###' (1) "Test Takers" Related variables
  ###' 
  ###' 
  
  if (year_num %in% sprintf("%02d", c(98:99, 0:12))){
    
    ### Percentages of Exam Takers out of the enrollment
    df <- df %>%
      mutate(PCT_ExamTakers_GR11_12 = 100*(N_ExamTakers / N_Enroll_GR11_12), 
             PCT_ExamTakers_Common = 100*(N_ExamTakers / N_Enroll_GR11_12))
    
    
  } else if (year_num %in% sprintf("%02d", c(13:17))){
    
    ### Percentages of Exam Takers out of the enrollment
    df <- df %>%
      mutate(PCT_ExamTakers_GR10_12 = 100*(N_ExamTakers / N_Enroll_GR10_12), 
             PCT_ExamTakers_Common = 100*(N_ExamTakers / N_Enroll_GR10_12))
    
  }
  
  
  
  ###'######################################################################
  ###'
  ###' (2) "Test" Related variables 
  ###'
  ###'
  

  df <- df %>%
    mutate(
      
      ### Number of whole AP exams taken
      N_Exams_Taken = rowSums(select(., contains("N_Exams_Scr"))), 
      
      
      ### Number of Tests with an AP Score of 3 or higher (Score 3, 4, 5)
      N_Exams_3above = rowSums(select(., N_Exams_Scr3, N_Exams_Scr4, N_Exams_Scr5)), 
      
      
      ### Percentage of each score out of N_Exams_Taken
      PCT_Score1 = 100 * (N_Exams_Scr1 / N_Exams_Taken),
      PCT_Score2 = 100 * (N_Exams_Scr2 / N_Exams_Taken),
      PCT_Score3 = 100 * (N_Exams_Scr3 / N_Exams_Taken),
      PCT_Score4 = 100 * (N_Exams_Scr4 / N_Exams_Taken),
      PCT_Score5 = 100 * (N_Exams_Scr5 / N_Exams_Taken),
      PCT_Score3above = 100 * (N_Exams_3above / N_Exams_Taken), 
      
      
      ### Average number of tests Taken by each test taker
      N_Exams_Taken_per_Taker = N_Exams_Taken / N_ExamTakers
      
      )
  
  
  
  ###'######################################################################
  ###'
  ###' Re-Order variables
  ###'
  ###'
  
  names(df)
  
  df <- df %>%
    dplyr::select(Year,  
                  contains("Code"), 
                  contains("Name"),
                  contains("N_Enroll_"), 
                  N_ExamTakers, contains("PCT_ExamTakers"), 
                  N_Exams_Taken_per_Taker, N_Exams_Taken, 
                  contains("N_Exams_Scr"), N_Exams_3above, 
                  contains("PCT_Score"), 
                  everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Save as .rda & .dta file format
  ###'
  ###'
  
  setwd(data_dir)
  
  dualsave(df, paste0(filename, "_managed"))
  
  
  
  ###'######################################################################
  ###'
  ###' Embed into the list
  ###'
  ###'
  
  meta_list[[i]] <- df
  
  
  
  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("Year ", years[i], " completed", "\n"))
  
  
}  # End of loop over years



###'######################################################################
###'
###' Append!
###'
###'

### Check variable names over years
lapply(meta_list, names)


### Append!
df_bind <- bind_rows(meta_list) %>%
  arrange(CountyCode, DistrictCode, SchoolCode, Year) %>%
  dplyr::select(ReportType,   
                contains("Code"), 
                contains("Name"),
                Year,
                contains("N_Enroll_"), 
                N_ExamTakers, contains("PCT_ExamTakers"), 
                N_Exams_Taken_per_Taker, N_Exams_Taken, 
                contains("N_Exams_Scr"), N_Exams_3above, 
                contains("PCT_Score"), 
                everything())


### Save the resulting dataframe
setwd(data_dir)
dualsave(df_bind, "AP_Longitudinal_98-17")

