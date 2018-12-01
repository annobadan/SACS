
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - EL Reclassification Data: 2011-2017 
###' 
###' Students reclassified from English learner (EL) to 
###' fluent English proficient (FEP) since the last census.
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character/04_English_Learner/EL_Reclassification")


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

years <- sprintf("%02d",seq(11, 17))


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
  filename <- paste0("reclass", year_num)
  
  
  ### Import text files as character class
  df <- read.delim(file = paste0(filename, ".txt"), 
                   header = TRUE, 
                   colClasses = "character")
  
  
  ### Assign variable names
  names(df) <- c("CDS", "CountyName", "DistrictName", "SchoolName", 
                 "EL", "FEP", "Reclass")
  
  
  
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
  ###' Convert Character Variables to Numeric
  ###'
  ###'
  
  ### Variables to convert
  names(df)
  to_numeric <- c("EL", "FEP", "Reclass")
  
  
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
  
  
  
  
  
  