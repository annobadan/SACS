
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - Adjusted Cohort Graduation Rate and Outcome Data: 2016 ~
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
###' Loop over years: 2016 ~
###'
###'

years <- sprintf("%02d", c(16))


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
  filename <- paste0("acgr", year_num)
  
  
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
  
  names(df)[1] <- c("Year")
  
  ### ~ Count => "N_"
  vars <- names(df)[grepl("Count", names(df))] 
  vars <- vars[!vars %in% c("CountyCode", "CountyName")]
  names(df)[names(df) %in% vars] <- paste0("N_", gsub("Count", "", vars))
  names(df)
  
  ### ~ Rate => "Rate_"
  vars <- names(df)[grepl("Rate", names(df))] 
  names(df)[names(df) %in% vars] <- paste0("Rate_", gsub("Rate", "", vars))
  names(df)
  

  
  ###'######################################################################
  ###'
  ###' Aggregate Level
  ###' 
  ###' T = State
  ###' C = County
  ###' D = District
  ###' S = School
  ###' 
  ###' 
  
  tabdf(df, AggLevel)
  
  df <- df %>%
    mutate(AggLevel = factor(AggLevel, 
                             levels = c("T", "C", "D", "S"), 
                             labels = c("State", "County", "District", "School")))
  
  
  
  ###'######################################################################
  ###'
  ###' CDS code: 
  ###' 
  ###' Generate County, District, and School Codes
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
  ###' Charter School (All/Y/N)
  ###' 
  ###' The Dashboard Alternative School Status Program (DASS)
  ###'
  ###'
  
  tabdf(df, CharterYN)
  tabdf(df, DASSYN)
  

  
  ###'######################################################################
  ###'
  ###' Reporting Category
  ###' 
  ###' RB = African American
  ###' RI = American Indian or Alaska Native
  ###' RA = Asian
  ###' RF = Filipino
  ###' RH = Hispanic or Latino
  ###' RD = Not Reported
  ###' RP = Pacific Islander
  ###' RT = Two or More Races
  ###' RW = White
  ###' GM = Male
  ###' GF = Female
  ###' SE = English Learners
  ###' SD = Students with Disabilities
  ###' SS = Socioeconomically Disadvantaged
  ###' SM = Migrant
  ###' SF = Foster
  ###' SH = Homeless
  ###' TA = Total
  ###'
  ###' Note: SD, SS, SM, SF, and SH subgroup data are 
  ###'       not available prior to the 2015–16 academic year.
  ###'
  ###'
  
  ### Rename ReportingCategory to just Category
  df <- df %>%
    rename(Category = ReportingCategory)
  
  
  ### Category 01. Category_Race
  Race_level_vec <- c("RB", "RI", "RA", "RF", "RH", "RD", "RP", "RT", "RW", 
                      "TA")
  
  Race_label_vec <- c("African American", 
                      "American Indian or Alaska Native", 
                      "Asian", 
                      "Filipino", 
                      "Hispanic or Latino", 
                      "Not Reported", 
                      "Pacific Islander", 
                      "Two or More Races", 
                      "White", 
                      "Total") 
  
  exclude_vec <- unique(df$ReportingCategory)[!unique(df$Category) %in% Race_level_vec]
  
  df <- df %>%
    mutate(Category_Race = factor(Category, 
                                  levels = Race_level_vec, 
                                  labels = Race_label_vec, 
                                  exclude = exclude_vec))
  
  
  ### Category 02. Category_Gender
  Gender_level_vec <- c("GF", "GM", "TA")
  
  Gender_label_vec <- c("Female", "Male", "Total") 
  
  exclude_vec <- unique(df$Category)[!unique(df$Category) %in% Gender_level_vec]
  
  df <- df %>%
    mutate(Category_Gender = factor(Category, 
                                    levels = Gender_level_vec, 
                                    labels = Gender_label_vec, 
                                    exclude = exclude_vec))
  
  
  ### Category 03. Category_Program
  Program_level_vec <- c("SE", "SD", "SS", "SM", "SF", "SH", "TA")
  
  Program_label_vec <- c("English Learners", 
                         "Students with Disabilities", 
                         "Socioeconomically Disadvantaged", 
                         "Migrant", 
                         "Foster", 
                         "Homeless",
                         "Total") 
  
  exclude_vec <- unique(df$Category)[!unique(df$Category) %in% Program_level_vec]
  
  df <- df %>%
    mutate(Category_Program = factor(Category, 
                                     levels = Program_level_vec, 
                                     labels = Program_label_vec, 
                                     exclude = exclude_vec))
  
  
  ### Check the resulting variables and reorder variables
  tabdf(df, Category_Race)
  tabdf(df, Category_Gender)
  tabdf(df, Category_Program)
  

  
  
  
  ###'######################################################################
  ###'
  ###' Arrange rows and columns
  ###'
  ###'
  
  df <- df %>%
    select(Year:DASSYN,  
           starts_with("Category"), 
           everything()) %>%
    arrange(AggLevel, CountyCode, DistrictCode, SchoolCode, 
            Category_Program, Category_Race, Category_Gender) 
  
  
  
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


