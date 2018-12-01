
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - Transitional Kindergarten Data: 2013-2016
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character/10_Transitional_Kindergarten")


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
###' Loop over years: 2013-2016
###'
###'

years <- sprintf("%02d", seq(13, 16))


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
  filename <- paste0("tkdata", year_num)
  
  
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
  ###' Assign variable names
  ###'
  ###'
  
  names(df)
  
  names(df) <- c("Year", "CDS", "Name", "AggLevel", 
                 "Subgroup", "Race", "Gender", 
                 "N_Kindergarten", "N_TK_Census", "N_TK_Cum")
  
  
  
  ###'######################################################################
  ###'
  ###' Aggregate Level
  ###' 
  ###' 
  
  tabdf(df, AggLevel)
  
  df <- df %>%
    mutate(AggLevel = factor(AggLevel, 
                             levels = c("State", "County", "District", "School"), 
                             labels = c("State", "County", "District", "School")))
  
  
  
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
  ###' Subgroup
  ###' 
  ###' < Program Participation >
  ###'   EL – English learners
  ###'   MIG – Migrant Education
  ###'   SD – Socioeconomically Disadvantaged
  ###'   All – Total of all subgroups
  ###'
  ###'
  
  tabdf(df, Subgroup)
  
  ### Category #1. Program Participation
  level_vec <- c("EL", "MIG", "SD", "ALL")
  
  label_vec <- c("English Learners", 
                 "Migrant Education", 
                 "Socioeconomically Disadvantaged", 
                 "Total")
  
  exclude_vec <- unique(df$Subgroup)[!unique(df$Subgroup) %in% level_vec]
  
  df <- df %>%
    mutate(Subgroup = factor(Subgroup, 
                             levels = level_vec, 
                             labels = label_vec, 
                             exclude = exclude_vec))
  
  tabdf(df, Subgroup)

  
  
  ###'######################################################################
  ###'
  ###' Race
  ###' 
  ###' 
  
  tabdf(df, Race)
  
  df <- df %>%
    mutate(Race = factor(Race, 
                         levels = c(0:9, "ALL"), 
                         labels = c("Not reported", 
                                    "American Indian or Alaska Native", 
                                    "Asian", 
                                    "Pacific Islander", 
                                    "Filipino", 
                                    "Hispanic or Latino", 
                                    "African American", 
                                    "White", 
                                    "Multiple or No Response", 
                                    "Two or More Races", 
                                    "Total")))
  
  tabdf(df, Race)
  
  
  
  ###'######################################################################
  ###'
  ###' Gender
  ###'
  ###'
  
  tabdf(df, Gender)
  
  df <- df %>%
    mutate(Gender = factor(Gender, 
                           levels = c("F", "M", "ALL"), 
                           labels = c("female", "male", "Total")))
  
  
  
  ###'######################################################################
  ###'
  ###' Arrange rows and columns
  ###'
  ###'
  
  df <- df %>%
    arrange(AggLevel, CountyCode, DistrictCode, SchoolCode, 
            Subgroup, Race, Gender) %>% 
    select(Year, AggLevel, CDS, CountyCode, DistrictCode, SchoolCode, Name,  
           Subgroup, Race, Gender, 
           everything())
  
  
  
  ###'######################################################################
  ###' 
  ###' Convert character to numeric
  ###' 
  ###' 
  
  ### Variables to convert
  names(df)
  to_numeric <- c(names(df)[grepl("N_", names(df))])
  
  
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
  save(df, file = paste0(filename, "_cleaned", ".rda"))
  write.dta(df, file = paste0(filename, "_cleaned", ".dta"))
  
  
  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("Year ", years[i], " completed", "\n"))
  
  
} # End of loop


