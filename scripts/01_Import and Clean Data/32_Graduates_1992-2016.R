
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - Graduates by Race & Gender: 1992-2016
###' 
###' 
###' 20181024 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character/08_Graduate")


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
###' Loop over years: 1992-2016
###'
###'

years <- c(sprintf("%02d",seq(92, 99)), sprintf("%02d",seq(0, 16)))


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
  
  
  ### Assign filename
  filename <- paste0("grads", year_num)
  
  
  ###' Import text files as character class
  ###' and assign brief name
  df <- read.delim(file = paste0(filename, ".txt"), 
                   header = TRUE, 
                   colClasses = "character", 
                   na.strings = c("*"))
  
  classmode(df, everything())
  
  
  ### Convert empty strings to NA
  df <- df %>% mutate_all(funs(empty_as_na)) 
  
  
  
  ###'######################################################################
  ###'
  ###' CDS code: 
  ###' 
  ###' Generate County, District, and School Codes
  ###'
  ###'
  
  ### Check number of characters: should be 14
  table(nchar(df$CDS_CODE))
  
  
  ### Substring County, District, School Codes
  df <- df %>%
    mutate(CountyCode = substr(CDS_CODE, start = 1, stop = 2), 
           DistrictCode = substr(CDS_CODE, start = 3, stop = 7), 
           SchoolCode = substr(CDS_CODE, start = 8, stop = 14))
  
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
    select(YEAR, CDS_CODE, CountyCode, DistrictCode, SchoolCode,
           everything())
  
  
  
  ###'######################################################################
  ###' 
  ###' Ethnicity
  ###' 
  ###' 
  
  classmode(df, ETHNIC)
  tabdf(df, ETHNIC)
  
  df <- df %>%
    mutate(ETHNIC = factor(ETHNIC, 
                           levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 
                           labels = c("Not reported", 
                                      "American Indian or Alaska Native", 
                                      "Asian", 
                                      "Pacific Islander", 
                                      "Filipino", 
                                      "Hispanic or Latino", 
                                      "African American", 
                                      "White", 
                                      "Multiple or No Response", 
                                      "Two or More Races")))
  
  
  
  ###'######################################################################
  ###' 
  ###' Gender
  ###' 
  
  classmode(df, GENDER)
  tabdf(df, GENDER)
  
  df <- df %>%
    mutate(GENDER = factor(GENDER, 
                           levels = c("M", "F"), 
                           labels = c("male", "female")))
  
  
  
  ###'######################################################################
  ###' 
  ###' GRADS:
  ###' 
  ###' Number of twelfth-grade graduates. 
  ###' This data includes summer graduates and 
  ###' does not include students with high school equivalencies 
  ###' (i.e., General Educational Development (GED) test or 
  ###' California High School Proficiency Examination (CHSPE)).
  ###' 
  ###' 
  
  df <- df %>%
    mutate(GRADS = as.numeric(GRADS))
  
  summary(df$GRADS)
  
  
  
  ###'######################################################################
  ###' 
  ###' UC_GRADS:
  ###' 
  ###' Number of twelfth-grade graduates who also completed all courses 
  ###' required for entry into the University of California (UC) and/or 
  ###' California State University (CSU) with a grade "C" or better. 
  ###' This data includes summer graduates and 
  ###' does not include students with high school equivalencies (i.e., GED or CHSPE).
  ###' 
  ###' 
  
  df <- df %>%
    mutate(UC_GRADS = as.numeric(UC_GRADS))
  
  summary(df$UC_GRADS)
  
  
  
  ###'######################################################################
  ###' 
  ###' Save data objects
  ###' 
  ###' 
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Arrange data before saving
  df <- df %>%
    arrange(CountyCode, DistrictCode, SchoolCode, ETHNIC, GENDER)
  
  
  ### Save the resulting dataframe
  save(df, file = paste0(filename, "_cleaned", ".rda"))
  write.dta(df, file = paste0(filename, "_cleaned", ".dta"))
  
  
  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("Year ", years[i], " completed", "\n"))
  
  
} # End of loop


