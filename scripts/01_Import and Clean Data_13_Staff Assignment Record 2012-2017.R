
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - Staff Assignment Record
###' 
###'  2012-2017 (6 fiscal years)
###' 
###'  
###' Just clean raw file - Manipulate variable later
###' 
###' 
###' 20170707 JoonHo Lee
###' 20180928 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Staff_Data/Certificated_Staff/Staff_Assignment_and_Course")


### Call libraries
library(tidyverse)
library(readxl)
library(Hmisc)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import AssignmentCodes Reference data: AssignmentCodes12On
###'
###'

setwd(data_dir)

load(file = "AssignmentCodes12On_cleaned.rda")

assign_codes_df <- df; rm(df)



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
  df <- read.delim(file = paste0("StaffAssign", year_num, ".txt"), header = TRUE, 
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
    mutate(CountyCode = substr(CD_code, start = 1, stop = 2), 
           DistrictCode = substr(CD_code, start = 3, stop = 7))
  
  
  ### Rename SchoolCode
  idx <- names(df) %in% c("SchoolCode", "Schoolcode")
  names(df)[idx] <- "SchoolCode"
  
  
  ### Convert to numeric
  df <- df %>%
    mutate_at(.vars = c("CountyCode", "DistrictCode", "SchoolCode"), 
              .funs = as.numeric)
  
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
  ###' Generate Assign_Code: 
  ###' 
  ###' By combining 
  ###' 
  ###' 1) AssignmentCode for non-teaching assignments
  ###' 2) CourseCode for teaching assignments    
  ###' 
  ###' 
  
  ### Check the distributions
  tabdf(df, AssignmentCode)
  tabdf(df, CourseCode)
  
  
  ### Generate Assign_Code
  df$Assign_Code <- df$CourseCode
  df$Assign_Code[is.na(df$Assign_Code)] <- df$AssignmentCode[is.na(df$Assign_Code)]
  
  
  ### Reorder columns
  classmode(df, everything())
  df <- df %>%
    select(AcademicYear:RecID, 
           AssignmentCode, CourseCode, Assign_Code, ClassID, 
           EstimatedFTE)
  
  
  ### Rearrange rows
  df <- df %>% 
    arrange(CountyCode, DistrictCode, SchoolCode, RecID, Assign_Code, ClassID)
  
  
  
  ###'######################################################################
  ###'
  ###' Estimated FTE: Convert to numeric
  ###'
  ###'
  
  ### Check distributions: There's something weird. Maybe coding error
  tabdf(df, EstimatedFTE)
  # df[df$EstimatedFTE == "08/20/2015", ]
  
  
  ### Convert to numeric
  df <- df %>%
    mutate(EstimatedFTE = as.numeric(EstimatedFTE))

  
  
  ###'######################################################################
  ###'
  ###' Merge information to Assign_Code
  ###'
  ###'
  
  df <- df %>%
    left_join(assign_codes_df, by = c("Assign_Code"))
  
  
  
  ###'######################################################################
  ###' 
  ###' Save data objects
  ###' 
  ###' 
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Save the resulting dataset
  save(df, file = paste0("StaffAssign", year_num, "_cleaned", ".rda"))
  write.dta(df, file = paste0("StaffAssign", year_num, "_cleaned", ".dta"))
  

  
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

