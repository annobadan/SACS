
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - Cohort Outcome Data: 2009-2015
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character/09_Cohort_Outcome")


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
###' Loop over years: 2009-2015
###'
###'

years <- sprintf("%02d",seq(9, 15))


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
  filename <- paste0("cohort", year_num)
  
  
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
  
  names(df) <- c("CDS", "Name", "AggLevel", "DFC", "Subgroup", "Subgrouptype", 
                 "N_Cohort", "N_Graduates", "Rate_Cohort_Graduates", 
                 "N_Dropouts", "Rate_Cohort_Dropouts", 
                 "N_SPED", "Rate_SPED_Complete", 
                 "N_Still_Enrolled", "Rate_Still_Enrolled", 
                 "N_GED", "Rate_GED", "Year")

  
  
  ###'######################################################################
  ###'
  ###' Aggregate Level
  ###' 
  ###' D = Local educational agency totals 
  ###'    (includes districts and direct funded charter schools)
  ###' O = County totals
  ###' S = School totals
  ###' T = State totals
  ###' 
  ###' 
  
  tabdf(df, AggLevel)
  
  df <- df %>%
    mutate(AggLevel = factor(AggLevel, 
                             levels = c("T", "O", "D", "S"), 
                             labels = c("State", "County", "LEA", "School")))
  
  
  
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
  ###' DFC: Direct funded charter indicator
  ###'
  ###'
  
  tabdf(df, DFC)
  
  df <- df %>% 
    mutate(DFC = if_else(DFC == "Y", 1, 0, missing = NA_real_))
  
  
  
  ###'######################################################################
  ###'
  ###' Subgroup
  ###' 
  ###' < Program Participation >
  ###'   EL – English learners
  ###'   MIG – Migrant Education
  ###'   SD – Socioeconomically Disadvantaged
  ###'   SE – Special Education
  ###'   
  ###' < Gender >
  ###'   FEM – Female
  ###'   MAL – Male
  ###'   
  ###' < Other >
  ###'   All – Total of all subgroups
  ###'
  ###'
  
  ### Category #1. Program Participation
  level_vec <- c("EL", "MIG", "SD", "SE", "All")
  
  label_vec <- c("English Learners", 
                 "Migrant Education", 
                 "Socioeconomically Disadvantaged", 
                 "Special Education", 
                 "Total")
  
  exclude_vec <- unique(df$Subgroup)[!unique(df$Subgroup) %in% level_vec]
  
  df <- df %>%
    mutate(Subgroup_Program = factor(Subgroup, 
                                     levels = level_vec, 
                                     labels = label_vec, 
                                     exclude = exclude_vec))
  
  tabdf(df, Subgroup_Program)
  
  
  ### Category #1. Gender 
  level_vec <- c("FEM", "MAL", "All")
  
  label_vec <- c("female", "male", "Total")
  
  exclude_vec <- unique(df$Subgroup)[!unique(df$Subgroup) %in% level_vec]
  
  df <- df %>%
    mutate(Subgroup_Gender = factor(Subgroup, 
                                    levels = level_vec, 
                                    labels = label_vec, 
                                    exclude = exclude_vec))
  
  tabdf(df, Subgroup_Gender)
  
  
  
  ###'######################################################################
  ###'
  ###' Subgroup Type = Ethnicity
  ###' 
  ###' 
  
  tabdf(df, Subgrouptype)
  
  df <- df %>%
    mutate(Subgroup_Race = factor(Subgrouptype, 
                                  levels = c(0:9, "All"), 
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
  
  tabdf(df, Subgroup_Race)
  
  
  
  ###'######################################################################
  ###'
  ###' Arrange rows and columns
  ###'
  ###'
  
  df <- df %>%
    arrange(AggLevel, CountyCode, DistrictCode, SchoolCode, 
            Subgroup_Program, Subgroup_Race, Subgroup_Gender) %>% 
    select(Year, AggLevel, CDS, CountyCode, DistrictCode, SchoolCode, Name, DFC, 
           Subgroup, Subgrouptype, Subgroup_Program, Subgroup_Race, Subgroup_Gender, 
           everything())
  
  
  
  ###'######################################################################
  ###' 
  ###' Convert character to numeric
  ###' 
  ###' 
  
  ### Variables to convert
  names(df)
  to_numeric <- c(names(df)[grepl("N_", names(df))], 
                  names(df)[grepl("Rate_", names(df))])
  
  
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


