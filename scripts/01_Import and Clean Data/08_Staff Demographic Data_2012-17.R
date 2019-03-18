
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - Staff Demographics
###' 
###' 2012-13, 2013-14, 2014-15, 2015-16, 2016-17, 2017-18
###' 
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
data_dir <- c("D:/Data/LCFF/Staff_Data/Certificated_Staff/Staff_Demographic")


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

years <- sprintf("%02d",seq(12, 17))


for (i in seq_along(years)) {
  
  
  ###'######################################################################
  ###'
  ###' Assign year number
  ###' 
  ###' 
  
  year_num <- years[i]
  
  
  
  ###'######################################################################
  ###'
  ###' Import raw dataset
  ###'
  ###'
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Import tab delimited text file
  df <- read.delim(file = paste0("StaffDemo", year_num, ".txt"), header = TRUE, 
                   colClasses = "character")
  
  classmode(df, everything())
  
  
  ### Convert empty strings to NA
  df <- df %>% mutate_all(.funs = empty_as_na)
  
  
  
  ###'######################################################################
  ###'
  ###' Academic year 
  ###'
  
  tabdf(df, AcademicYear)
  
  # df <- df %>%
  #   mutate(AcademicYear = as.numeric(AcademicYear))

  
  
  ###'######################################################################
  ###'
  ###' RecID:
  ###' Do not convert to numeric
  ###' 
  ###'
  
  tabdf(df, RecID)
  
  # df <- df %>%
  #   mutate(RecID = as.numeric(RecID))
  


  ###'######################################################################
  ###'
  ###' CD Code:
  ###' 
  ###' Split DistrictCode into "CountyCode" and "DistrictCode"
  ###' 
  ###'
  
  ### Rename DistrictCode to CDcode
  tabdf(df, DistrictCode)
  df <- df %>% rename(CD_code = DistrictCode)
  
  
  ### Check number of characters: should be 7
  table(nchar(df$CD_code))

  
  ### Substring CountyCode, DistrictCode
  tabdf(df, CD_code)
  
  df <- df %>%
    mutate(CountyCode = if_else(nchar(CD_code) == 7, 
                                substr(CD_code, start = 1, stop = 2), 
                                substr(CD_code, start = 1, stop = 1)), 
           DistrictCode = if_else(nchar(CD_code) == 7, 
                                  substr(CD_code, start = 3, stop = 7), 
                                  substr(CD_code, start = 2, stop = 6)))
  
  
  ### Convert to numeric
  df <- df %>%
    mutate_at(.vars = c("CountyCode", "DistrictCode"), 
              .funs = as.numeric)
  
  tabdf(df, CountyCode)
  tabdf(df, DistrictCode)
  
  
  ### Rearrange row and column
  df <- df %>% 
    select(AcademicYear, CD_code, CountyCode, DistrictCode, 
           CountyName, DistrictName, RecID, everything()) %>%
    arrange(CountyCode, DistrictCode, RecID)
  
  

  ###'######################################################################
  ###' 
  ###' Gender Code: factor labeling
  ###' 
  ###' 
  
  tabdf(df, GenderCode)
  
  df <- df %>%
    mutate(GenderCode = factor(GenderCode, 
                               levels = c("F", "M"), 
                               labels = c("female", "male")))
  
  classmode(df, GenderCode)

  
  
  ###'######################################################################
  ###' 
  ###' Age variable
  ###' 
  
  if(year_num %in% sprintf("%02d",seq(12, 17))) {
    
    idx <- which(names(df) %in% c("age", "Age"))
    
    df <- df %>%
      mutate_at(.vars = names(df)[idx], 
                .funs = as.numeric)
  }
  
  classmode(df, everything())
  
  
  
  ###'######################################################################
  ###' 
  ###' Education Level: Dummy for Master / Doctorate
  ###' 
  ###' 
  ###' D = Doctorate
  ###' S = Special
  ###' V = Master's degree plus 30 or more semester hours
  ###' M = Master's degree
  ###' U = Fifth year within bachelor's degree
  ###' Y = Fifth year induction
  ###' F = Fifth year
  ###' C = Baccalaureate plus 30 or more semester hours
  ###' B = Baccalaureate
  ###' A = Associate degree
  ###' N = Not reported
  ###' 
  ###' 
  
  tabdf(df, EducationLevel)
  classmode(df, EducationLevel)
  
  
  ### Recode missing values
  df$EducationLevel[df$EducationLevel == "N"] <- NA
  tabdf(df, EducationLevel)
  

  ### Generate factor variable
  df <- df %>%
    mutate(EducationLevel = factor(df$EducationLevel, 
                                   levels = c("A", "B", "C", "F", "Y", "U", 
                                              "M", "V", "S", "D")))
  
  tabdf(df, EducationLevel)
  
  
  
  ###'######################################################################
  ###' 
  ###' Ethnic Group
  ###' 
  ###' 0 = Not Reported 
  ###' 1 = American Indian or Alaska Native, not Hispanic
  ###' 2 = Asian, not Hispanic
  ###' 3 = Pacific Islander, not Hispanic
  ###' 4 = Filipino, not Hispanic
  ###' 5 = Hispanic or Latino
  ###' 6 = African American, not Hispanic
  ###' 7 = White, not Hispanic
  ###' 9 = Two or More Races, not Hispanic
  ###' 
  ###' 
  
  tabdf(df, EthnicGroup)
  

  ### Recode missing values
  df$EthnicGroup[df$EthnicGroup == 0] <- NA  # Many missing values
  
  
  ### Factor labeling
  df <- df %>%
    mutate(EthnicGroup = factor(EthnicGroup, 
                                levels = c(7, 5, 6, 2, 4, 3, 1, 9), 
                                labels = c("White", "Hispanic/Latino", "Black", 
                                           "Asian", "Filipino", "Pacific Islander", 
                                           "American Indian/Alaska Native", 
                                           "Two or more races")))
  
  tabdf(df, EthnicGroup)
  
  
  
  ###'######################################################################
  ###' 
  ###' Years Teaching / Years in district
  ###' 
  ###' Dummy variables for being new teaching / new in district
  ###' 
  ###' 
  
  ### Convert to numeric
  df <- df %>%
    mutate_at(.vars = c("YearsInDistrict", "YearsTeaching"), 
              .funs = as.numeric)
  
  tabdf(df, YearsInDistrict)
  tabdf(df, YearsTeaching)
  
  

  ###'######################################################################
  ###' 
  ###'  EmploymentStatusCode
  ###' 
  ###' L = Long term substitute or temporary employee
  ###' P = Probationary
  ###' T = Tenured
  ###' O = Other
  ###' Blank = Not reported
  ###' 
  ###' 
  
  tabdf(df, EmploymentStatusCode)

  
  ### Recode missing values
  df$EmploymentStatusCode[!df$EmploymentStatusCode %in% c("L", "O", "P", "T")] <- NA
  
  
  ### Generate factor variable
  df <- df %>%
    mutate(EmploymentStatusCode = factor(df$EmploymentStatusCode, 
                                         levels = c("L", "P", "T", "O"), 
                                         labels = c("Long term substitute/temporary", 
                                                    "Probationary", 
                                                    "Tenured", 
                                                    "Other")))
  
  tabdf(df, EmploymentStatusCode)
  classmode(df, EmploymentStatusCode)
  
  
  
  ###'######################################################################
  ###' 
  ###' FTE variables
  ###' 
  ###' 
  
  ### Convert to numeric
  idx <- grep("FTE", names(df), value = FALSE)
  df <- df %>%
    mutate_at(.vars = names(df)[idx], 
              .funs = as.numeric)
  
  classmode(df, contains("FTE"))
  
  
  ### Change variable names
  names(df)[idx] <- gsub("\\.", "_", names(df[idx]))
  
  
  
  ###'######################################################################
  ###' 
  ###' Delete file created date
  ###'
  
  df <- df %>% select(-FileCreated)
  
  
  
  ###'######################################################################
  ###' 
  ###' Save data objects
  ###' 
  ###' 
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Save the resulting dataset
  save(df, file = paste0("StaffDemo", year_num, "_cleaned", ".rda"))
  write.dta(df, file = paste0("StaffDemo", year_num, "_cleaned", ".dta"))
  
  
  
  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("Year ", year_num, " completed", "\n"))

  
  
  ###'######################################################################
  ###' 
  ###' End for loop
  ###' 
  
}



