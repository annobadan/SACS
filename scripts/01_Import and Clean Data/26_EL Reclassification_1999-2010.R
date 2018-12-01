
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - EL Reclassification Data: 1999-2010
###' 
###' => 1999-2005 datafiles couldn't be executed. 
###'  
###' 
###' Students reclassified from English learner (EL) to 
###' fluent English proficient (FEP) since the last census.
###' 
###' 
###' 20181023 JoonHo Lee 
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
###' Loop over years: 1999, 2002, 2005-2010
###' 
###' Do 2000, 2001, 2003, 2004 later
###'
###'

years <- sprintf("%02d", c(99, 2, 5:10))

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
  
  if (year_num %in% c("09", "10")){
    
    df <- read.delim(file = paste0(filename, ".txt"), 
                     header = TRUE, 
                     colClasses = "character")
    
  } else if (year_num %in% sprintf("%02d", seq(6, 8))){
    
    df <- read.dbf(file = paste0(filename, ".dbf"), as.is = TRUE)
    
    df <- df %>%
      rename(RECLASS = REDES)
    
  } else if (year_num %in% sprintf("%02d", c(99, 2, 5))){
    
    df <- read.dbf(file = paste0(filename, ".dbf"), as.is = TRUE)
    
    df <- df %>%
      rename(RECLASS = REDES)
    
  }

  
  ### Convert empty strings to NA
  df <- df %>% mutate_all(funs(empty_as_na))
  
  
  
  ###'######################################################################
  ###'
  ###' Rename variables
  ###' 
  ###' 
  
  names(df)[1] <- c("CDS")
  
  names(df)[2:4] <- c("CountyName", "DistrictName", "SchoolName")
  
  classmode(df, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' CDS code 
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
  
  if (year_num %in% c("09", "10")) {
    
    ### Variables to convert
    names(df)
    idxl <- grepl("Code", names(df))|grepl("Name", names(df))
    to_numeric <- names(df)[!names(df) %in% c("CDS", names(df)[idxl])]
    
    
    ### Convert selected columns to numeric
    df[, to_numeric] <- df[, to_numeric] %>%
      mutate_all(.funs = as.numeric)
    
    classmode(df, everything())
    
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
  
  
} # End of loop





