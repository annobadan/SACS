
###'######################################################################
###'
###' Import and clean data files 
###' 
###' < Chronic Absenteeism Data >
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
###' Loop over years
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
  filename <- paste0("chronicabsenteeism", year_num)
  
  
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
  
  names(df) <- c("Year", "AggLevel", 
                 "CountyCode", "DistrictCode", "SchoolCode", 
                 "CountyName", "DistrictName", "SchoolName", 
                 "Charter", 
                 "Category", 
                 "Enrollment_Cum", "N_ChronicAbsent", "Rate_ChronicAbsent")
  
  
  
  ###'######################################################################
  ###'
  ###' Aggregate Level
  ###' 
  ###' T = State
  ###' C = County
  ###' D1 = District (All Schools)
  ###' D2 = District (Non-Charter Schools)
  ###' S = School
  ###'
  ###'
  
  tabdf(df, AggLevel)
  
  df <- df %>% 
    mutate(AggLevel = factor(AggLevel, 
                             levels = c("T", "C", "D1", "D2", "S"), 
                             labels = c("State",
                                        "County", 
                                        "District_All", 
                                        "District_NonCharter", 
                                        "School")))
  
  
  
  ###'######################################################################
  ###'
  ###' CDS code: 
  ###' 
  ###' County, District, and School Codes
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
  ###' Rearrange row and column orders
  ###'
  ###'
  
  df <- df %>%
    arrange(AggLevel, CountyCode, DistrictCode, SchoolCode) %>% 
    select(Year, AggLevel, 
           CountyCode, DistrictCode, SchoolCode, 
           CountyName, DistrictName, SchoolName, 
           Charter, 
           Category, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Generate Category variables
  ###' 
  ###' RB = African American
  ###' RI = American Indian or Alaska Native
  ###' RA = Asian
  ###' RF = Filipino
  ###' RH = Hispanic or Latino
  ###' RD = Did not Report
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
  ###' GRK = Kindergarten
  ###' GR13 = Grades 1–3
  ###' GR46 = Grades 4–6
  ###' GR78 = Grades 7–8
  ###' GRK8 = Grades K–8
  ###' GR912 = Grades 9–12
  ###' GRUG = Ungraded Elementary and Secondary
  ###' TA = Total
  ###'
  ###'
  
  ### Check distributions
  tabdf(df, Category)
  
  
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
  
  exclude_vec <- unique(df$Category)[!unique(df$Category) %in% Race_level_vec]
  
  df <- df %>%
    mutate(Category_Race = factor(Category, 
                                  levels = Race_level_vec, 
                                  labels = Race_label_vec, 
                                  exclude = exclude_vec))
  
  tabdf(df, Category_Race)
  
  
  ### Category 02. Category_Gender
  Gender_level_vec <- c("GF", "GM", "TA")
  
  Gender_label_vec <- c("Female", "Male", "Total") 
  
  exclude_vec <- unique(df$Category)[!unique(df$Category) %in% Gender_level_vec]
  
  df <- df %>%
    mutate(Category_Gender = factor(Category, 
                                    levels = Gender_level_vec, 
                                    labels = Gender_label_vec, 
                                    exclude = exclude_vec))
  
  tabdf(df, Category_Gender)
  
  
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
  
  tabdf(df, Category_Program)
  
  
  ### Category 04. Grade
  df <- df %>%
    mutate(Category = recode(Category, 
                             "Grade K" = "GRK", 
                             "Grade 1-3" = "GR13", 
                             "Grade 4-6" = "GR46", 
                             "Grade 7-8" = "GR78", 
                             "Grade K-8" = "GRK8", 
                             "Grade 9-12" = "GR912", 
                             "Grade Ungr" = "GRUG"))
  
  Grade_level_vec <- c("GRK", "GR13", "GR46", "GR78", "GRK8", "GR912", "GRUG", "TA")
  Grade_label_vec <- c("Kindergarten", "Grade 1-3", "Grade 4-6", "Grade 7-8", 
                       "Grade K-8", "Grade 9-12", "Grade Ungraded", "Total")
  
  exclude_vec <- unique(df$Category)[!unique(df$Category) %in% Grade_level_vec]
  
  df <- df %>%
    mutate(Category_Grade = factor(Category, 
                                   levels = Grade_level_vec, 
                                   labels = Grade_label_vec, 
                                   exclude = exclude_vec))

  tabdf(df, Category_Grade)
  
  
  ### Reorder variables
  names(df)
  
  df <- df %>%
    select(Year:Charter, starts_with("Category"), 
           Enrollment_Cum, N_ChronicAbsent, Rate_ChronicAbsent)
  
  
  
  ###'######################################################################
  ###' 
  ###' Convert character to numeric
  ###' 
  ###' 
  
  ### Variables to convert
  names(df)
  to_numeric <- c("Enrollment_Cum", "N_ChronicAbsent", "Rate_ChronicAbsent")
  
  
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
  save(df, file = paste0(filename , "_cleaned", ".rda"))
  write.dta(df, file = paste0(filename, "_cleaned", ".dta"))
  
  
  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("Year ", years[i], " completed", "\n"))
  
  
} # End of loop


