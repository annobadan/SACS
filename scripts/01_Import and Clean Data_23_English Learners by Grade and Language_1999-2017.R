
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - English Learners by Grade & Language: 1999-2017 
###' 
###' 
###' 20181020 JoonHo Lee 
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character/04_English_Learner/EL_by_Grade_Language")


### Call libraries
library(tidyverse)
library(readxl)
library(Hmisc)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Loop over years: 1999-2017
###'
###'

years <- c(sprintf("%02d", 99), sprintf("%02d",seq(0, 17)))


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
  
  
  ### Get file name
  filename <- paste0("elsch", year_num)
  
  
  ### Define common variable names
  varnames <- c("CDS", 
                "COUNTY", "DISTRICT", "SCHOOL", 
                "LC", "LANGUAGE", 
                "KDGN", paste0("GR_", seq(1, 12)), 
                "UNGR", "TOTAL_EL") 
  
  
  ###' Import text files as character class
  ###' and assign brief name
  if (year_num %in% c("99", "00")) {
    
    ### Import dataframe
    df <- read.dbf(file = paste0(filename, ".dbf"), as.is = TRUE)
    
    ### Assign variable names
    names(df) <- varnames

  
  } else if (year_num %in% c(sprintf("%02d", 1), sprintf("%02d",seq(7, 17)))){
    
    ### Import dataframe
    df <- read.delim(file = paste0(filename, ".txt"), 
                     header = TRUE, 
                     colClasses = "character")
    
    # ### Rename variables: if contains "SumOf" prefix
    # idxl <- grepl("SumOf", names(df))
    # names(df)[idxl] <- gsub("SumOf", "", names(df)[idxl])
    
    ### Assign variable names
    names(df) <- varnames
    
  } else if (year_num %in% sprintf("%02d",seq(2, 6))){
    
    ### Import dataframe: without header
    df <- read.delim(file = paste0(filename, ".txt"), 
                     header = FALSE, 
                     colClasses = "character")
    
    ### Assign variable names
    names(df) <- varnames
  } 
  
  classmode(df, everything())
  
  
  ### Convert empty strings to NA
  df <- df %>% mutate_all(funs(empty_as_na)) 
  
  
  
  ###'######################################################################
  ###'
  ###' CDS code & name: 
  ###' 
  ###' Generate County, District, and School Codes
  ###'
  ###'
  
  ### Check number of characters: should be 14
  table(nchar(df$CDS))
  
  
  ### Substring County, District, School Codes
  df <- df %>%
    mutate(CountyCode = substr(CDS, start = 1, stop = 2), 
           DistrictCode = substr(CDS, start = 3, stop = 7), 
           SchoolCode = substr(CDS, start = 8, stop = 14))
  
  tabdf(df, CountyCode)
  tabdf(df, DistrictCode)
  tabdf(df, SchoolCode)
  
  
  ### Convert character to numeric
  df <- df %>%
    mutate_at(.vars = c("CountyCode", "DistrictCode", "SchoolCode"), 
              .funs = as.numeric)
  
  classmode(df, ends_with("Code"))
  
  
  ### Rename variables for COUNTY, DISTRICT, SCHOOL
  df <- df %>%
    rename(CountyName = COUNTY, 
           DistrictName = DISTRICT, 
           SchoolName = SCHOOL)
  
  
  ### Rearrange row and column orders
  df <- df %>%
    arrange(CountyCode, DistrictCode, SchoolCode) %>% 
    select(CDS, 
           CountyCode, DistrictCode, SchoolCode, 
           CountyName, DistrictName, SchoolName,
           everything())
  
  classmode(df, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Language Identification Code and Name
  ###'
  ###'
  
  ### Check distribution
  tabdf(df, LC)
  
  
  ### Convert Lauguage ID Code to numeric
  df <- df %>%
    mutate(LC = as.numeric(LC))
  
  
  ### Sort data by CDS code and Language code
  df <- df %>%
    arrange(CountyCode, DistrictCode, SchoolCode, LC)
  
  
  
  ###'######################################################################
  ###'
  ###' Convert Character Variables to Numeric
  ###'
  ###'
  
  ### Variables to convert
  names(df)
  to_numeric <- c("KDGN", names(df)[grepl("GR_", names(df))], "UNGR",
                  names(df)[grepl("TOTAL", names(df))])
  
  
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
  
 