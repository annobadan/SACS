
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - Postsecondary Preparation
###' 
###'   (1) SAT
###'   
###' 
###' 20181026 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/School_Performance/Postsecondary_Preparation/SAT")


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
###' Loop over years: 9899-1617 
###' 
###' 

years <- c(sprintf("%02d", seq(98, 99)), sprintf("%02d", seq(0, 16))) 

for (i in seq_along(years)){
  
  
  ###'######################################################################
  ###'
  ###' Assign year number & filetype
  ###' 
  ###' 
  
  year_num <- years[i]
  
  
  
  ###'######################################################################
  ###'
  ###' Import raw excel file
  ###'
  ###'
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Assign filenames
  if (year_num != c("16")){
    
    filename <- paste0("sat", years[i], years[i + 1]) 
  
  } else if (year_num == c("16")){
    
    filename <- paste0("sat", year_num, as.numeric(year_num) + 1)
    
  }
  
  
  ### Import data file
  if (year_num %in% sprintf("%02d", c(98:99, 0:12))){
    
    df <- read_excel(paste0(filename, ".xls"), 
                     sheet = 1, skip = 2)
    
    classmode(df, everything())
    
  } else if (year_num %in% sprintf("%02d", c(13:16))){
    
    df <- read_excel(paste0(filename, ".xls"), 
                     sheet = 1, skip = 0)
    
    classmode(df, everything())
    
  }
  
  
  ### Convert empty strings to NA
  df <- df %>% mutate_all(funs(empty_as_na)) 
  
  
  
  ###'######################################################################
  ###'
  ###' Assign variable names
  ###'
  ###'
  
  classmode(df, everything())
  
  if (year_num %in% sprintf("%02d", c(98))){
    
    names(df) <- c("CountyCode", "DistrictCode", "SchoolCode", 
                   "DistrictName", "SchoolName", 
                   "N_Enroll_GR12", "N_TestTakers", "PCT_TestTakers", 
                   "Avg_Verbal", "Avg_Math", "Avg_Total", 
                   "N_1000_above", "PCT_1000_above")
    
  } else if (year_num %in% sprintf("%02d", c(99, 0:4))){
    
    names(df) <- c("CountyCode", "DistrictCode", "SchoolCode", 
                   "CountyName", "DistrictName", "SchoolName", 
                   "N_Enroll_GR12", "N_TestTakers", "PCT_TestTakers", 
                   "Avg_Verbal", "Avg_Math", "Avg_Total", 
                   "N_1000_above", "PCT_1000_above")
    
  } else if (year_num %in% sprintf("%02d", c(5:12))){
    
    names(df) <- c("CountyCode", "DistrictCode", "SchoolCode", 
                   "CountyName", "DistrictName", "SchoolName", 
                   "N_Enroll_GR12", "N_TestTakers", "PCT_TestTakers", 
                   "Avg_Verbal", "Avg_Math", "Avg_Writing", "Avg_Total", 
                   "N_1500_above", "PCT_1500_above")
    
  } else if (year_num %in% sprintf("%02d", c(13:15))){
    
    names(df) <- c("CDS", "ReportType", 
                   "SchoolName", "DistrictName", "CountyName", 
                   "N_Enroll_GR12", "N_TestTakers",  
                   "Avg_Verbal", "Avg_Math", "Avg_Writing",  
                   "N_1500_above", "PCT_1500_above")
    
  } else if (year_num %in% sprintf("%02d", c(16))){
    
    names(df) <- c("CDS", "CountyCode", "CDCode", "SchoolCode", 
                   "ReportType",
                   "SchoolName", "DistrictName", "CountyName", 
                   "N_Enroll_GR12", "N_TestTakers", 
                   "N_ELA_Bench_Curr", "N_ELA_Bench_Pre", 
                   "N_ELA_Bench_Total", "PCT_ELA_Bench_Total",  
                   "N_Math_Bench_Curr", "N_Math_Bench_Pre", 
                   "N_Math_Bench_Total", "PCT_Math_Bench_Total", 
                   "N_Both_Bench_Total", "PCT_Both_Bench_Total")
    
  } 
  
  
  ###'######################################################################
  ###'
  ###' CDS code 
  ###' 
  ###' 
  
  if (year_num %in% sprintf("%02d", c(13:16))){
    
    ### Check number of characters: should be 14
    table(nchar(df$CDS))
    
    
    ### Substring County, District, School Codes
    df <- df %>%
      mutate(CountyCode = substr(CDS, start = 1, stop = 2), 
             DistrictCode = substr(CDS, start = 3, stop = 7), 
             SchoolCode = substr(CDS, start = 8, stop = 14))
    
    
    ### Check distributions
    tabdf(df, CountyCode)
    tabdf(df, DistrictCode)
    tabdf(df, SchoolCode)
    
  } 
  

  ### Convert character to numeric
  df <- df %>%
    mutate_at(.vars = c("CountyCode", "DistrictCode", "SchoolCode"), 
              .funs = as.numeric)
  
  classmode(df, everything())
  
  
  ### Delete CDcode for 2016 onward
  if (year_num %in% sprintf("%02d", c(16))){
    
    df <- df %>% select(-CDCode)
    
  } 
  
  
  
  ###'######################################################################
  ###'
  ###' Convert to numeric
  ###'
  ###'
  
  ### Variables to convert
  classmode(df, everything())
  
  to_numeric <- c(names(df)[grepl("N_", names(df))], 
                  names(df)[grepl("Avg_", names(df))], 
                  names(df)[grepl("PCT_", names(df))])
  
  
  ### Convert selected columns to numeric
  df[, to_numeric] <- df[, to_numeric] %>% 
    mutate_all(.funs = as.numeric)
  
  classmode(df, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Rearrange rows and columns
  ###'
  ###'
  
  if (year_num %in% sprintf("%02d", c(13:16))){
    
    classmode(df, everything())
    
    df <- df %>%
      select(-CDS) %>%
      select(ReportType, 
             CountyCode, DistrictCode, SchoolCode, 
             CountyName, DistrictName, SchoolName, 
             everything()) %>%
      arrange(ReportType, CountyCode, DistrictCode, SchoolCode)
    
  }
  
  
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
  
  
} # End of loop over years


