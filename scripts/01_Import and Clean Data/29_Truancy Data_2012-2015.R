
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - Truancy: 2012-2015
###' - Chronic Absenteeism: 2016 ~
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character/06_Truancy_and_Chronic_Absenteeism")


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
###' Loop over years: 2012-2015 => Truancy Data
###'
###'

years <- sprintf("%02d",seq(12, 15))


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
  filename <- paste0("truancy", year_num)
  
  ###' Import text files as character class
  ###' and assign brief name
  df <- read.delim(file = paste0(filename, ".txt"), 
                   header = TRUE, 
                   colClasses = "character")
  
  classmode(df, everything())
  
  
  ### Convert empty strings to NA
  df <- df %>% mutate_all(funs(empty_as_na)) 
  
  
  
  ###'######################################################################
  ###'
  ###' Assign variable names
  ###'
  ###'
  
  names(df)
  
  names(df) <- c("Year", "CDS", "AggLevel", "Name", 
                 "N_Truant", "Enrollment_Census", "Enrollment_Cum", 
                 "Truancy_Rate")
  
  
  
  ###'######################################################################
  ###'
  ###' CDS code: 
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
  
  
  
  ###'######################################################################
  ###'
  ###' Aggregate Level
  ###' 
  ###' D = Local educational agency totals 
  ###'     (includes districts and direct funded charter schools)
  ###' O = County totals
  ###' S = School totals
  ###' T = State totals
  ###'
  ###'
  
  tabdf(df, AggLevel)
  
  df <- df %>% 
    mutate(AggLevel = factor(AggLevel, 
                             levels = c("T", "O", "D", "S"), 
                             labels = c("State", 
                                        "County", 
                                        "LEA", 
                                        "School")))
  
  
  
  ###'######################################################################
  ###'
  ###' Rearrange row and column orders
  ###'
  ###'
  
  df <- df %>%
    arrange(AggLevel, CountyCode, DistrictCode, SchoolCode) %>% 
    select(Year, CDS, CountyCode, DistrictCode, SchoolCode, Name, AggLevel,  
           starts_with("Enrollment"), N_Truant, Truancy_Rate, 
           everything())
  
  
  
  ###'######################################################################
  ###' 
  ###' Convert character to numeric
  ###' 
  ###' 
  
  ### Variables to convert
  names(df)
  to_numeric <- c("Enrollment_Census", "Enrollment_Cum", "N_Truant", "Truancy_Rate")
  
  
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
  
  cat(paste0("Year ", years[i], " completed", "\n"))
  
  
} # End of loop


