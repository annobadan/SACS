
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - Staff Demographics + StaffFTE + Staff Credential Records
###' 
###'  2003-2011 (9 fiscal years)
###' 
###' 
###'  
###' Just clean raw file - Manipulate variable later
###' 
###' 
###' 20170707 JoonHo Lee
###' 20180925 JoonHo Lee
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

years <- sprintf("%02d",seq(3, 11))


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
  df <- read.delim(file = paste0("paif", year_num, ".txt"), header = TRUE, 
                   colClasses = "character")
  
  classmode(df, everything())
  
  
  ### Convert empty strings to NA
  df <- df %>% mutate_all(.funs = empty_as_na)
  
  

  ###'######################################################################
  ###'
  ###' Academic year 
  ###'
  ###'
  
  df <- df %>%
    mutate(AcademicYear = paste0(year_num, as.numeric(year_num) + 1))
  
  tabdf(df, AcademicYear)
  
  
  
  ###'######################################################################
  ###'
  ###' RecID: 
  ###' DO NOT convert to numeric 
  ###'
  ###'
  
  names(df)[1] <- "REC_ID"
  
  
  tabdf(df, REC_ID)
  classmode(df, REC_ID)
  
  
  # df <- df %>%
  #   mutate(REC_ID = as.numeric(REC_ID))
  
  
  
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
  
  if (year_num %in% c("10", "11")){
    
    df <- df %>%
      arrange(CountyCode, DistrictCode, SchoolCode, REC_ID) %>% 
      select(AcademicYear, 
             CDS_CODE, CountyCode, DistrictCode, SchoolCode,  
             COUNTY, DISTRICT, SCHOOL, REC_ID,
             everything())
    
  } else if (year_num %in% c("09")) {
    
    df <- df %>%
      arrange(CountyCode, DistrictCode, SchoolCode, REC_ID) %>% 
      select(AcademicYear, 
             CDS_CODE, CountyCode, DistrictCode, SchoolCode, REC_ID,
             everything())
  }

  
  
  ###'######################################################################
  ###' 
  ###' Gender Code: factor labeling
  ###' 
  ###' 
  
  tabdf(df, GENDER)
  
  df <- df %>%
    mutate(GENDER = factor(GENDER, 
                               levels = c("F", "M"), 
                               labels = c("female", "male")))
  
  classmode(df, GENDER)
  
  

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
  
  tabdf(df, ED_LEVEL)
  classmode(df, ED_LEVEL)
  
  
  ### Recode missing values
  df$ED_LEVEL[df$ED_LEVEL == "N"] <- NA
  tabdf(df, ED_LEVEL)
  
  
  ### Generate factor variable
  df <- df %>%
    mutate(ED_LEVEL = factor(df$ED_LEVEL, 
                                   levels = c("A", "B", "C", "F", "Y", "U", 
                                              "M", "V", "S", "D")))
  
  tabdf(df, ED_LEVEL)
  
  
  
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
  
  tabdf(df, ETHNIC_GP)
  
  
  ### Recode missing values
  df$ETHNIC_GP[df$ETHNIC_GP == 0] <- NA  # Many missing values
  
  
  ### Factor labeling
  df <- df %>%
    mutate(ETHNIC_GP = factor(ETHNIC_GP, 
                              levels = c(7, 5, 6, 2, 4, 3, 1, 9), 
                              labels = c("White", "Hispanic/Latino", "Black", 
                                         "Asian", "Filipino", "Pacific Islander", 
                                         "American Indian/Alaska Native", 
                                         "Two or more races")))
  
  tabdf(df, ETHNIC_GP)
  
  
  
  ###'######################################################################
  ###' 
  ###' Years Teaching / Years in district
  ###' 
  ###' 
  
  ### Convert to numeric
  df <- df %>%
    mutate_at(.vars = c("YRS_TEACH", "YRS_DIST"), 
              .funs = as.numeric)
  
  
  ### Check distributions
  tabdf(df, YRS_TEACH)
  tabdf(df, YRS_DIST)
  
  
  ### Convert negative integers to missing varlues
  df$YRS_TEACH[df$YRS_TEACH < 0] <- NA
  df$YRS_DIST[df$YRS_DIST < 0] <- NA
  
  
  
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
  
  tabdf(df, STATUS)
  
  
  ### Recode missing values
  df$STATUS[!df$STATUS %in% c("L", "O", "P", "T")] <- NA
  
  
  ### Generate factor variable
  df <- df %>%
    mutate(STATUS = factor(df$STATUS, 
                           levels = c("L", "P", "T", "O"), 
                           labels = c("Long term substitute/temporary", 
                                      "Probationary", 
                                      "Tenured", 
                                      "Other")))
  
  tabdf(df, STATUS)
  classmode(df, STATUS)
  
  
  
  ###'######################################################################
  ###' 
  ###' FTE variables:
  ###' 
  ###' Teacher, Administrator, Pupil Services
  ###' 
  ###' 
  
  ### Check distributions: fine as it is 
  tabdf(df, TEACH)
  tabdf(df, PCTTEACH)
  
  tabdf(df, ADMIN)
  tabdf(df, PCTADMIN)
  
  tabdf(df, PUPIL)
  tabdf(df, PCTPUPIL)
  
  
  ### Convert to numeric
  df <- df %>%
    mutate_at(.vars = c("TEACH", "PCTTEACH", 
                        "ADMIN", "PCTADMIN", 
                        "PUPIL", "PCTPUPIL"), 
              .funs = as.numeric)
  
  classmode(df, everything())
  
  
  
  ###'######################################################################
  ###' 
  ###' Save data objects
  ###' 
  ###' 
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Save the resulting dataset
  save(df, file = paste0("paif", year_num, "_cleaned", ".rda"))
  write.dta(df, file = paste0("paif", year_num, "_cleaned", ".dta"))
  
  
  
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



