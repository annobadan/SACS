
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - Staff School FTE
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
  df <- read.delim(file = paste0("StaffSchoolFTE", year_num, ".txt"), 
                   header = TRUE, 
                   colClasses = "character")
  
  classmode(df, everything())
  
  
  ### Convert empty strings to NA
  df <- df %>% mutate_all(funs(empty_as_na)) 


  
  ###'######################################################################
  ###'
  ###' Academic year 
  ###'
  
  names(df)[1] <- "AcademicYear"
  
  tabdf(df, AcademicYear)
  
  # df <- df %>%
  #   mutate(AcademicYear = as.numeric(AcademicYear))
  
  classmode(df, AcademicYear)
  
  
  
  ###'######################################################################
  ###'
  ###' RecID:
  ###' Do not convert to numeric
  ###'
  
  tabdf(df, RecID)
  
  # df <- df %>%
  #   mutate(RecID = as.numeric(RecID))
  
  
  
  ###'######################################################################
  ###'
  ###' CDS Code:
  ###' 
  ###' Split DistrictCode into "CountyCode" and "DistrictCode"
  ###' Arrange by County, District, School, Teacher IDs
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
    mutate(CountyCode = substr(CD_code, start = 1, stop = 2), 
           DistrictCode = substr(CD_code, start = 3, stop = 7))
  
  
  ### Convert to numeric: County, District, and School Codes
  df <- df %>%
    mutate_at(.vars = c("CountyCode", "DistrictCode", "SchoolCode"), 
              .funs = as.numeric)
  
  classmode(df, ends_with("Code"))
  tabdf(df, CountyCode)
  tabdf(df, DistrictCode)
  tabdf(df, SchoolCode)
  
  
  ### Rearrange row and column
  df <- df %>% 
    select(AcademicYear, CD_code, CountyCode, DistrictCode, SchoolCode,  
           CountyName, DistrictName, SchoolName, RecID, everything()) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, RecID)
  
  
  
  ###'######################################################################
  ###' 
  ###' Staff type
  ###' 
  ###' 
  
  tabdf(df, StaffType)
  
  df <- df %>%
    mutate(StaffType = factor(StaffType, 
                              levels = c("T", "A", "P"), 
                              labels = c("Teacher", 
                                         "Administrator", 
                                         "Pupil services")))
  
  classmode(df, StaffType)
  
  
  
  ###'######################################################################
  ###' 
  ###' Job classification
  ###' 
  ###' 10 = Administrator
  ###' 11 = Pupil services
  ###' 12 = Teacher
  ###' 25 = Non-certificated Administrator
  ###' 26 = Charter School Non-certificated Teacher
  ###' 27 = Itinerant or Pull-Out/Push-In Teacher
  ###' 
  ###' 
  ###' Educational Service Job Classification: 27 
  ###' (Itinerant or Pull-Out/Push-In Teacher) 
  ###' An itinerant staff member assigned to more than one school site 
  ###' and/or a teacher who provides one-on-one or small group support or 
  ###' resource instruction by either pulling students out of the classroom, 
  ###' or coming into the classroom to provide the instruction.
  ###' 
  
  tabdf(df, JobClassification)
  
  df <- df %>%
    mutate(JobClassification =  factor(JobClassification, 
                                       levels = c(12, 10, 11, 26, 25, 27), 
                                       labels = c("Teacher", "Administrator", 
                                                  "Pupil services", 
                                                  "Charter Non-certificated Teacher", 
                                                  "Non-certificated Administrator", 
                                                  "Itinerant or Pull-out/Push-in Teacher")))
  
  
  
  ###'######################################################################
  ###' 
  ###' Check duplicated RecID
  ###' 
  ###' 

  RecID_dup <- df %>% 
    group_by(RecID) %>% 
    summarise(N = n())
  
  tabdf(RecID_dup, N)
  
  
  ### Tabulate based on Job Classification
  tab_jobclass_N <- RecID_dup %>% 
    left_join(select(df, RecID, JobClassification), by = c("RecID")) %>% 
    group_by(JobClassification) %>%
    summarise(mean_value = mean(N, na.rm = TRUE))
  
  tab_jobclass_N

  
  
  ###'######################################################################
  ###' 
  ###' FTE: convert to numeric
  ###'
  ###'  

  df <- df %>%
    mutate(FTE = as.numeric(FTE))
  
  tabdf(df, FTE)
  

  
  ###'######################################################################
  ###' 
  ###' Save data objects
  ###' 
  ###' 
  
  ### Set data containing working directory
  setwd(data_dir)

  
  ### Save the resulting datasets
  save(df, file = paste0("StaffSchoolFTE", year_num, "_cleaned", ".rda"))
  write.dta(df, file = paste0("StaffSchoolFTE", year_num, "_cleaned", ".dta"))
  
  
  
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
