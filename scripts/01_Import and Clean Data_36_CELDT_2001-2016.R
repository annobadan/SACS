
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - California English Language Development Test (CELDT)
###'  
###' 
###' 20181022 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Statewide_Student_Assessment/CELDT")


### Call libraries
library(tidyverse)
library(readxl)
library(Hmisc)
library(ldat)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Loop over years: 2001-2016
###'
###'

years <- sprintf("%02d",seq(1, 16))


for (i in 1:length(years)) {
  
  
  ###'######################################################################
  ###'
  ###' Assign year number
  ###' 
  ###' 
  
  year_num <- years[i]
  
  
  
  ###'######################################################################
  ###'
  ###' Import raw text files
  ###'
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Assign filenames
  filename <- paste0("statecsv", 
                     year_num, 
                     sprintf("%02d", as.numeric(year_num) + 1))
  
  
  ###' Import text files as character class
  
  # ###' (1) Entities
  # df_entities <- read.csv(file = paste0(filenames[1], ".csv"), 
  #                         header = TRUE, colClasses = "character")
  # 
  # classmode(df_entities, everything())
  
  
  ###' (2) Statewide research data file
  df <- read.csv(file = paste0(filename, ".csv"), 
                 header = TRUE, colClasses = "character")
  
  classmode(df, everything())
  

  ### Convert empty strings to NA
  # df_entities <- df_entities %>% mutate_all(funs(empty_as_na)) 
  df <- df %>% mutate_all(funs(empty_as_na)) 
  
  
  
  ###'######################################################################
  ###'
  ###' Assign variable names & Delete two unused columns
  ###'
  ###'
  
  names(df)
  
  if (year_num %in% sprintf("%02d", seq(5, 16))){
    
    names(df) <- c("CountyCode", "DistrictCode", "SchoolCode", 
                   "RecordType", "CharterNum", 
                   "TestYear", "Subgroup", "OverallPerfLevel", "TestPurpose", 
                   "N_Students", "PCT_Students", "Grade", 
                   "Unused1", "Score_Listening", 
                   "Unused2", "Score_Reading", "Score_Speaking", "Score_Writing", 
                   "N_Met_Criterion", "PCT_Met_Criterion")
    
    df <- df %>%  select(-Unused1, -Unused2)
    
  } else if (year_num %in% sprintf("%02d", seq(3, 4))){
    
    names(df) <- c("CountyCode", "DistrictCode", "SchoolCode", 
                   "RecordType", "CharterNum", 
                   "TestYear", "Subgroup", "Grade", 
                   "OverallPerfLevel", "TestPurpose", 
                   "N_Students", "PCT_Students",  
                   "Score_Listening_Speaking", "Score_Reading", "Score_Writing", 
                   "N_Met_Criterion", "PCT_Met_Criterion")
    
  } else if (year_num %in% sprintf("%02d", c(2))){
    
    names(df) <- c("CountyCode", "DistrictCode", "SchoolCode", 
                   "RecordType", "CharterNum", 
                   "TestYear", "Subgroup", "Grade", 
                   "OverallPerfLevel", "TestPurpose", 
                   "N_Students", "PCT_Students",  
                   "Score_Listening_Speaking", "Score_Reading", "Score_Writing", 
                   "N_Exempt_Listening", "PCT_Exempt_Listening", 
                   "N_Met_Criterion", "PCT_Met_Criterion")
    
  } else if (year_num %in% sprintf("%02d", c(1))){
    
    names(df) <- c("CountyCode", "DistrictCode", "SchoolCode", 
                   "RecordType", "CharterNum", 
                   "TestYear", "Subgroup", "Grade", 
                   "OverallPerfLevel", "TestPurpose", 
                   "N_Students", "PCT_Students",  
                   "Score_Listening_Speaking", "Score_Reading", "Score_Writing")
    
  }
  

  
  ###'######################################################################
  ###'
  ###' CDS code 
  ###' 
  ###' 
  
  ### Check distributions
  tabdf(df, CountyCode)
  tabdf(df, DistrictCode)
  tabdf(df, SchoolCode)
  
  
  ### Convert character to numeric
  df <- df %>%
    mutate_at(.vars = c("CountyCode", "DistrictCode", "SchoolCode"), 
              .funs = as.numeric)
  
  classmode(df, ends_with("Code"))
  
  
  
  ###'######################################################################
  ###'
  ###' Record Type
  ###' 
  ###' 01 = School 
  ###' 02 = District
  ###' 03 = County
  ###' 04 = State
  ###' 09 = Independent Charter
  ###' 10 = Dependent Charter
  ###' 
  ###' 
  
  ### Check distributions
  tabdf(df, RecordType)
  
  
  ### Record numeric format
  df$RecordType <- sprintf("%02d", as.numeric(df$RecordType))
  
  
  ### Generate factor variable
  Record_level <- c("01", "02", "03", "04", "09", "10")
  Record_label <- c("School", "District", "County", "State", 
                    "Independent_Charter", "Dependent_Charter")
  
  df <- df %>%
    mutate(RecordType = factor(RecordType, 
                               levels = Record_level, 
                               labels = Record_label))
  
  tabdf(df, RecordType)
  
  
  
  ###'######################################################################
  ###'
  ###' Charter Number => Charter Dummy
  ###'
  ###'
  
  ### Check distribution
  tabdf(df, CharterNum)
  
  
  ### Generate Charter dummy variable
  df <- df %>%
    mutate(Charter = if_else(CharterNum == "0000", 0, 1, missing = NA_real_))
  
  tabdf(df, Charter)
  
  
  ### Reorder variables
  df <- df %>%
    select(TestYear, RecordType, ends_with("Code"), Charter, CharterNum, 
           everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Subgroup => Generate category variables
  ###'
  ###'
  
  ### Check distributions
  tabdf(df, Subgroup)
  
  
  ### Convert to numeric
  df <- df %>% 
    mutate(Subgroup = as.numeric(Subgroup))
  
  
  ### Subgroup 1. Gender   
  Gender_level_vec <- c(1, 2, 3)
  
  Gender_label_vec <- c("Total", "female", "male") 
  
  exclude_vec <- unique(df$Subgroup)[!unique(df$Subgroup) %in% Gender_level_vec]
  
  df <- df %>%
    mutate(Subgroup_Gender = factor(Subgroup, 
                                    levels = Gender_level_vec, 
                                    labels = Gender_label_vec, 
                                    exclude = exclude_vec))
  
  tabdf(df, Subgroup_Gender)
  
  
  ### Subgroup 2. SPED   
  
  if (year_num %in% sprintf("%02d", seq(5, 16))){
    
    SPED_level_vec <- c(1, 5, 7, 29)
    
  } else if (year_num %in% sprintf("%02d", seq(1, 4))){
    
    SPED_level_vec <- c(1, 5, 7, 25)
    
  }

  SPED_label_vec <- c("Total", 
                      "Not Receiving SPED", 
                      "Receiving SPED", 
                      "Receiving SPED tested with others") 
  
  exclude_vec <- unique(df$Subgroup)[!unique(df$Subgroup) %in% SPED_level_vec]
  
  df <- df %>%
    mutate(Subgroup_SPED = factor(Subgroup, 
                                  levels = SPED_level_vec, 
                                  labels = SPED_label_vec, 
                                  exclude = exclude_vec))
  
  tabdf(df, Subgroup_SPED)
  
  
  ### Subgroup 3. Primary Language   
  LANG_level_vec <- c(1, 8:19)
  
  LANG_label_vec <- c("Total",
                      "Spanish", 
                      "Vietnamese", 
                      "Cantonese", 
                      "Korean", 
                      "Filipino", 
                      "Hmong", 
                      "Mandarin", 
                      "Armenian", 
                      "Cambodian", 
                      "Russian", 
                      "Other", 
                      "Not Specified") 
  
  exclude_vec <- unique(df$Subgroup)[!unique(df$Subgroup) %in% LANG_level_vec]
  
  df <- df %>%
    mutate(Subgroup_LANG = factor(Subgroup, 
                                  levels = LANG_level_vec, 
                                  labels = LANG_label_vec, 
                                  exclude = exclude_vec))
  
  tabdf(df, Subgroup_LANG)
  
  
  ### Subgroup 4. Program participation   
  
  if (year_num %in% sprintf("%02d", seq(13, 16))){
    
    Program_level_vec <- c(1, 20, 21, 22, 23, 24, 26, 28)
    
    Program_label_vec <- c("Total",
                           "ELD and/or SDAIE and Primary", 
                           "ELD Only", 
                           "SDAIE Only", 
                           "ELD and SDAIE Not Primary", 
                           "No EL instruction", 
                           "Gifted and Talented", 
                           "Migrant Education") 
    
    exclude_vec <- unique(df$Subgroup)[!unique(df$Subgroup) %in% Program_level_vec]
    
  } else if (year_num %in% sprintf("%02d", seq(3, 12))){
    
    Program_level_vec <- c(1, 4, 6, 20, 21, 22, 23, 24, 26, 25, 28, 27)
    
    Program_label_vec <- c("Total",
                           "EL in ELD", 
                           "Unknown", 
                           "EL in ELD and SDAIE", 
                           "EL in ELD and SDAIE with Primary", 
                           "EL in ELD and Subjects with Primary", 
                           "Other", 
                           "None", 
                           "Gifted and Talented", 
                           "Indian Education", 
                           "Migrant Education", 
                           "Title 1") 
    
    exclude_vec <- unique(df$Subgroup)[!unique(df$Subgroup) %in% Program_level_vec]
    
  } else if (year_num %in% sprintf("%02d", seq(1, 2))){
    
    Program_level_vec <- c(1, 4, 5, 6)
    
    Program_label_vec <- c("Total",
                           "EL in ELD", 
                           "EL in Bilingual", 
                           "EL in SDAIE") 
    
    exclude_vec <- unique(df$Subgroup)[!unique(df$Subgroup) %in% Program_level_vec]
    
  }
  
  df <- df %>%
    mutate(Subgroup_Program = factor(Subgroup, 
                                     levels = Program_level_vec, 
                                     labels = Program_label_vec, 
                                     exclude = exclude_vec))
  
  tabdf(df, Subgroup_Program)
  
  
  ### Reorder variables
  names(df)
  
  df <- df %>%
    select(TestYear:CharterNum, starts_with("Subgroup"), everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Overall Performance Level
  ###'
  ###' 0 = All Performance Levels
  ###' 1 = Beginning
  ###' 2 = Early Intermediate
  ###' 3 = Intermediate
  ###' 4 = Early Advanced
  ###' 5 = Advanced
  ###'
  ###'

  ### Check distribution
  tabdf(df, OverallPerfLevel)
  classmode(df, OverallPerfLevel)
  
  
  ### Convert to factor
  df <- df %>%
    mutate(OverallPerfLevel = as.numeric(OverallPerfLevel)) %>%
    mutate(OverallPerfLevel = factor(OverallPerfLevel, 
                                     levels = c(0:5), 
                                     labels = c("All_Levels", 
                                                "Beginning", 
                                                "Early_Intermediate", 
                                                "Intermediate",
                                                "Early_Advanced", 
                                                "Advanced")))
  
  
  
  ###'######################################################################
  ###'
  ###' Test Purpose
  ###' 
  ###' 1 = Initial Assessment
  ###' 2 = Annual Assessment
  ###' 3 = All Assessments
  ###' 4 = Annual Assessment Outside the Window
  ###' 5 = Test Purpose Unknown
  ###' 
  ###'
  
  ### Check distribution
  tabdf(df, TestPurpose)
  
  
  ### Convert to factor
  df <- df %>%
    mutate(TestPurpose = as.numeric(TestPurpose)) %>%
    mutate(TestPurpose = factor(TestPurpose, 
                                levels = c(1:5), 
                                labels = c("Initial_Assessment", 
                                           "Annual_Assessment", 
                                           "All_Assessment", 
                                           "Annual_Assessment_Outside", 
                                           "Test_Purpose_Unknown")))
  
  
  
  ###'######################################################################
  ###'
  ###' Grade
  ###'
  ###'
  
  ### Check distribution
  tabdf(df, Grade)
  
  
  ### Convert to factor
  df <- df %>%
    mutate(Grade = as.numeric(Grade)) %>%
    mutate(Grade = factor(Grade, 
                          levels = c(0:13), 
                          labels = c("KN", 
                                     paste0("Grade ", 1:12), 
                                     "All Grades")))
  
  
  
  ###'######################################################################
  ###'
  ###' Reorder variables & Sort data
  ###' 
  ###' 
  
  df <- df %>%
    arrange(RecordType, CountyCode, DistrictCode, SchoolCode, 
            TestPurpose, OverallPerfLevel, Grade, Subgroup) %>%
    select(TestYear, RecordType, CountyCode, DistrictCode, SchoolCode,
           Charter, CharterNum, 
           TestPurpose, OverallPerfLevel, Grade, 
           starts_with("Subgroup"), everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Drop unncessary variables
  ###'
  ###'
  
  if (year_num %in% c("02")){
    
    df <- df %>%
      select(-N_Exempt_Listening, -PCT_Exempt_Listening)
    
  }
  
  
  ###'######################################################################
  ###'
  ###' Convert to numeric 
  ###'
  ###'

  ### Variables to convert
  names(df)
  to_numeric <- c("N_Students", "PCT_Students", 
                  "Score_Listening_Speaking",
                  "Score_Listening", 
                  "Score_Reading", 
                  "Score_Speaking", "Score_Writing", 
                  "N_Met_Criterion", "PCT_Met_Criterion")
  
  to_numeric <- to_numeric[to_numeric %in% names(df)]
  
  
  ### Convert selected columns to numeric
  df[, to_numeric] <- df[, to_numeric] %>% 
    mutate_all(.funs = as.numeric)
  
  classmode(df, everything())
  

  
  ###'######################################################################
  ###' 
  ###' Save data objects
  ###' 
  ###' 
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Save the resulting dataframe
  save(df, file = paste0(filename, "_cleaned", ".rda"))
  write.dta(df, file = paste0(filename, "_cleaned", ".dta"))
  
  
  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("Year ", years[i], " completed", "\n"))
  
  
} # End of loop


