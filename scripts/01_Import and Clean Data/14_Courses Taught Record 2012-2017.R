
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - Courses Taught Record
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
  df <- read.delim(file = paste0("CoursesTaught", year_num, ".txt"), 
                   header = TRUE, 
                   colClasses = "character")
  
  classmode(df, everything())
  
  
  ### Convert empty strings to NA
  df <- df %>% mutate_all(.funs = empty_as_na)
  
  
  ### Remove wrongly imported row &  unnecessary column
  tabdf(df, AcademicYear)
  df <- df %>%
    filter(AcademicYear != "AcademicYear") %>%
    select(-FileCreated)
  
  
  ### Replace . with _ in variable names
  names(df) <- gsub("\\.", "_", names(df))
  
  
  
  ###'######################################################################
  ###'
  ###' Academic year 
  ###'
  
  tabdf(df, AcademicYear)
  
  # df <- df %>%
  #   mutate(AcademicYear = as.numeric(AcademicYear))
  
  
  
  ###'######################################################################
  ###'
  ###' CD Code:
  ###' 
  ###' Split DistrictCode into "CountyCode" and "DistrictCode"
  ###' 
  ###' Arrange by CDS + Course & Class IDs
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
  idx <- names(df) %in% c("SchoolCode", "Schoolcode", "schoolCode")
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
           CountyName, DistrictName, SchoolName, 
           CourseCode, ClassID, ClassCourseID, 
           Enrollment, 
           everything()) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, 
            CourseCode, ClassID, ClassCourseID)

  
  
  ###'######################################################################
  ###'
  ###' Enrollment
  ###'  
  ###'    
  
  ### Check and sort out character values: conver to NA
  tabdf(df, Enrollment)
  
  df <- df %>%
    mutate(Enrollment = as.numeric(Enrollment))
  
  
  
  ###'######################################################################
  ###'
  ###' UC CSU Approved
  ###'
  ###'
  
  tabdf(df, UC_CSU_Approved)
  
  df$UC_CSU_Approved <- recode(df$UC_CSU_Approved, "Y" = 1, "N" = 0)

  
  
  ###'######################################################################
  ###'
  ###' NCLB Core & NCLB HQT
  ###' 
  ###' A = Compliant on the basis of passage of an approved exam
  ###' B = Compliant on the basis of coursework
  ###' C = Compliant on the basis of national board certification
  ###' D = Compliant on the basis of High Objective Uniform State Standard Evaluation 
  ###'     (HOUSSE)
  ###' G = Compliant on the basis of the Subject Matter Verification Process 
  ###'     for Middle and High School Level Teachers in Special Settings (VPSS) option
  ###' N = Not NCLB compliant
  ###' Blank = Field is not relative to the assignment
  ###'
  ###'
  
  ### NCLB Core
  tabdf(df, NCLB_Core)
  df$NCLB_Core <- recode(df$NCLB_Core, "Y" = 1, "N" = 0)
  
  
  ### NCLB HQT
  tabdf(df, NCLB_HQT)
  classmode(df, everything())
  df <- df %>%
    mutate(NCLB_HQT = factor(NCLB_HQT))
  
  
  
  ###'######################################################################
  ###'
  ###' Distance Learning 
  ###'
  ###'
  
  tabdf(df, DistanceLearning)
  df$DistanceLearning <- recode(df$DistanceLearning, "Y" = 1, "N" = 0)
  
  
  
  ###'######################################################################
  ###'
  ###' Independent Study
  ###'
  ###'
  
  tabdf(df, IndependentStudy)
  df$IndependentStudy <- recode(df$IndependentStudy, "Y" = 1, "N" = 0)
  
  
  
  ###'######################################################################
  ###'
  ###' Multiple Teacher Code
  ###'
  ###'
  
  tabdf(df, MultipleTeacherCode)
  classmode(df, MultipleTeacherCode)
  
  df <- df %>%
    mutate(MultipleTeacherCode = factor(MultipleTeacherCode, 
                                        levels = c(1, 2), 
                                        labels = c("Team Teaching", 
                                                   "Job Sharing")))
  
  
  
  ###'######################################################################
  ###'
  ###' CTE_FundingProvider
  ###' 
  ###' The Career Technical Education course is provided by 
  ###' 
  ###' 1) the Regional Occupational Center or Program (ROC/P), or
  ###' 2) the district
  ###'
  ###'
  
  tabdf(df, CTE_FundingProvider) 
  classmode(df, CTE_FundingProvider)
  
  df$CTE_FundingProvider <- recode_factor(df$CTE_FundingProvider, 
                                          "1" = "CTE by ROC/P", 
                                          "2" = "CTE by District", 
                                          .default = NA_character_)
  
  
  
  ###'######################################################################
  ###'
  ###' SEID Indicator
  ###' 
  ###' This SEID (Statewide Educator Identifier) Indicator specifies 
  ###' whether or not there are staff-level data 
  ###' available for a specific class/course in the 
  ###' StaffDemographics, StaffAssignment, and StaffSchoolFTE data-sets.
  ###'
  ###'
  
  tabdf(df, SEID_Indicator)
  classmode(df, SEID_Indicator)
  
  df$SEID_Indicator <- recode(df$SEID_Indicator, "Y" = 1, "N" = 0, 
                              .default = NA_real_)

  
  ###'######################################################################
  ###' 
  ###' Save data objects
  ###' 
  ###' 
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Save the resulting dataset
  save(df, file = paste0("CoursesTaught", year_num, "_cleaned", ".rda"))
  write.dta(df, file = paste0("CoursesTaught", year_num, "_cleaned", ".dta"))
  
  
  
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
