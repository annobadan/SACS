
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - Enrollment by School 02: Primary & Short-Term Enrollment
###' 
###' Useful for identifying student mobility (Short-term enrollment)
###' 
###' Just clean raw file - Manipulate variable later
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character/01_Enrollment_Primary_and_Short_Term")


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

enr_years <- c(sprintf("%02d",seq(09, 17)))


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
  df <- read.delim(file = paste0("enrps", year_num, ".txt"), 
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
  ### 14, 16 => Problem!
  table(nchar(df$CDS_CODE))
  
  
  # ###' Save as stata data file to investigate the imported dataset
  # setwd(data_dir)
  # write.dta(df, file = paste0("enrps", year_num, ".dta"))

  ###' Remove if N_character of CDS_CODE == 16
  ###' It turns out that the dataset includes duplicates:
  ###' If number of characters == 16, it means that the entries are duplicates
  ###' (because the redundant county codes are added)
  ###' 
  ###' But, the following years with unduplicated CDS_CODE, 
  ###' provides CDS_CODE == 16. So choose 16 and then trim the first 2 characters
  nrow(df)
  df <- df %>% 
    filter(nchar(CDS_CODE) == 16)
  nrow(df)
  
  df <- df %>%
    mutate(CDS_CODE = substr(CDS_CODE, start = 3, stop = 16))
  
  
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
  
  
  ### Rename county, district, school names
  classmode(df, COUNTY, DISTRICT, SCHOOL)
  tabdf(df, COUNTY)
  df <- df %>%
    rename(CountyName = COUNTY, 
           DistrictName = DISTRICT, 
           SchoolName = SCHOOL)
  
  classmode(df, everything())
  
  
  ###' Rearrange row and column orders
  df <- df %>%
    arrange(CountyCode, DistrictCode, SchoolCode) %>% 
    select(CDS_CODE, 
           CountyCode, DistrictCode, SchoolCode, 
           CountyName, DistrictName, SchoolName, 
           everything())
  
  
  
  ###'######################################################################
  ###' 
  ###' Type of Enrollment
  ###' 
  ###' P = Primary Enrollment
  ###' 
  ###' The student’s name appears on a register, roll, or list, 
  ###' the student is currently attending (or intends to attend) 
  ###' the educational service institution (ESI), 
  ###' or is responsible for the students instruction 
  ###' (students attending NPS schools).
  ###'     
  ###' C = Combined Enrollment
  ###' 
  ###' The combined enrollment of primary and short-term students. 
  ###' Short-term enrollment is defined as 
  ###' when the student’s name appears on a register, roll, or list, 
  ###' the student is currently attending the educational service institution, 
  ###' and receives or will receive the majority of their instruction 
  ###' at the institution for less than 30 days.
  ###' 
  ###' 
  
  classmode(df, ENR_TYPE)
  
  df <- df %>%
    mutate(ENR_TYPE = factor(ENR_TYPE, 
                             levels = c("P", "C"), 
                             labels = c("Primary_Enrollment", 
                                        "Combined_Enrollment")))
  tabdf(df, ENR_TYPE)
  
  
  
  ###'######################################################################
  ###' 
  ###' Ethnicity
  ###' 
  ###' 
  
  classmode(df, ETHNIC)
  
  tabdf(df, ETHNIC)
  
  df <- df %>%
    mutate(ETHNIC = factor(ETHNIC, 
                           levels = c(0, 1, 2, 3, 4, 5, 6, 7, 9), 
                           labels = c("Not reported", 
                                      "American Indian or Alaska Native", 
                                      "Asian", 
                                      "Pacific Islander", 
                                      "Filipino", 
                                      "Hispanic or Latino", 
                                      "African American", 
                                      "White", 
                                      "Two or More Races")))
  
  tabdf(df, ETHNIC)
  classmode(df, ETHNIC)
  
  
  
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
  save(df, file = paste0("enrps", year_num , "_cleaned", ".rda"))
  write.dta(df, file = paste0("enrps", year_num , "_cleaned", ".dta"))
  
  
  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("Year ", enr_years[i], " completed", "\n"))
  
  
} # End of loop


