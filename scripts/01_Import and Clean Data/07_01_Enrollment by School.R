
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - Enrollment by school
###' 
###' Useful for school racial composition
###' 
###' Just clean raw file - Manipulate variable later
###' 
###' 
###' 20170707 JoonHo Lee
###' 20180923 JoonHo Lee
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
library(readxl)
library(Hmisc)
library(ldat)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Loop over years: 1993-2016
###'
###'

enr_years <- c(sprintf("%02d",seq(93, 99)), sprintf("%02d",seq(0, 17)))


for (i in 1:length(enr_years)) {
  
  
  ###'######################################################################
  ###'
  ###' Assign year number
  ###' 
  ###' 
  
  year_num <- enr_years[i]
  
  
  
  ###'######################################################################
  ###'
  ###' Import raw text files
  ###'
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ###' Import text files as character class
  ###' and assign brief name
  df <- read.delim(file = paste0("enr", year_num, ".txt"), 
                   header = TRUE, 
                   colClasses = "character")
  
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
  
  
  ###' Rearrange row and column orders
  ###' Note that COUNTY, DISTRICT, SCHOOL names are missing from 1993-2006
  df <- df %>%
    arrange(CountyCode, DistrictCode, SchoolCode) %>% 
    select(CDS_CODE, CountyCode, DistrictCode, SchoolCode,
           everything())
    

  
  ###'######################################################################
  ###' 
  ###' Ethnicity
  ###' 
  ###' 

  classmode(df, ETHNIC)
  
  df <- df %>%
    mutate(ETHNIC = factor(ETHNIC, 
                           levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 
                           labels = c("Not reported (07-16)", 
                                      "American Indian or Alaska Native", 
                                      "Asian", 
                                      "Pacific Islander", 
                                      "Filipino", 
                                      "Hispanic or Latino", 
                                      "African American", 
                                      "White", 
                                      "Multiple or No Response (98-06)", 
                                      "Two or More Races (07-16)")))
  
  tabdf(df, ETHNIC)
  classmode(df, ETHNIC)
  
  
  
  ###'######################################################################
  ###' 
  ###' Gender
  ###' 
  
  classmode(df, GENDER)
  
  df <- df %>%
    mutate(GENDER = factor(GENDER, 
                           levels = c("M", "F"), 
                           labels = c("male", "female")))

  tabdf(df, GENDER)
  classmode(df, GENDER)

  
  
  ###'######################################################################
  ###' 
  ###' Convert character to numeric
  ###' 
  ###' 
  
  ### Variables to convert
  names(df)
  to_numeric <- c("KDGN", "GR_1", "GR_2", "GR_3", "GR_4", "GR_5", "GR_6", "GR_7", "GR_8","UNGR_ELM", 
                  "GR_9", "GR_10", "GR_11", "GR_12", "UNGR_SEC", "ENR_TOTAL", "ADULT")
  
  
  ### Convert selected columns to numeric
  
  df[, to_numeric] <- df[, to_numeric] %>% 
    mutate_all(.funs = as.numeric)
  
  classmode(df, everything())
  
  summary(df)


  
  ###'######################################################################
  ###' 
  ###' Save data objects
  ###' 
  ###' 
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Save the resulting dataframe
  save(df, file = paste0("enr", year_num , "_cleaned", ".rda"))
  write.dta(df, file = paste0("enr", year_num , "_cleaned", ".dta"))

  
  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("Year ", enr_years[i], " completed", "\n"))
  
  
} # End of loop


