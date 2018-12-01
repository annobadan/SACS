
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - (Class) Course Enrollment Record
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
  df <- read.delim(file = paste0("CourseEnrollment", year_num, ".txt"), 
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
  ###' Arrange by CDS codes + Course & Class IDs + Grade & Gender (later)
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
  
  

  ###'######################################################################
  ###'
  ###' GradeLevelCode
  ###' 
  ###' KN = Kindergarten
  ###' 01 = 1st Grade
  ###' 02 = 2nd Grade
  ###' 03 = 3rd Grade
  ###' 04 = 4th Grade
  ###' 05 = 5th Grade
  ###' 06 = 6th Grade
  ###' 07 = 7th Grade
  ###' 08 = 8th Grade
  ###' 09 = 9th Grade
  ###' 10 = 10th Grade
  ###' 11 = 11th Grade
  ###' 12 = 12th Grade
  ###'
  ###'
  
  ### Check distribution
  tabdf(df, GradeLevelCode)
  
  
  ### Convert to missing values other than K-12
  idx <- df$GradeLevelCode %in% c("KN", sprintf("%02d",seq(1, 12)))
  df$GradeLevelCode[!idx] <- NA
  
  
  ### Convert to factor
  df <- df %>%
    mutate(GradeLevelCode = factor(GradeLevelCode, 
                                   levels = c("KN", sprintf("%02d",seq(1, 12)))))
  
  
  
  ###'######################################################################
  ###'
  ###' GenderCode
  ###'
  ###'
  
  ### Check distribution
  tabdf(df, GenderCode)
  
  
  ### Convert to missing values other than M and F
  idx <- df$GenderCode %in% c("M", "F")
  df$GenderCode[!idx] <- NA
  
  
  ### Convert to factor
  df <- df %>%
    mutate(GenderCode = factor(GenderCode, 
                               levels = c("F", "M"), 
                               labels = c("female", "male")))
  
  
  
  ###'######################################################################
  ###'
  ###' Rearrange row and column
  ###' 
  ###' 
  
  df <- df %>% 
    select(AcademicYear, CD_code, CountyCode, DistrictCode, SchoolCode,  
           CountyName, DistrictName, SchoolName, 
           ClassID, CourseCode, ClassCourseID, 
           GradeLevelCode, GenderCode,  
           everything()) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, 
            ClassID, CourseCode, ClassCourseID, 
            GradeLevelCode, GenderCode)
  
  
  
  ###'######################################################################
  ###'
  ###' Enrollments: Convert to numeric
  ###'
  ###'
  
  classmode(df, everything())
  
  Enroll_vars <- names(df)[grepl("Enroll", names(df))]
  
  df <- df %>%
    mutate_at(.vars = Enroll_vars, 
              .funs = as.numeric)
  
  
  
  
  # ###'######################################################################
  # ###'
  # ###' Reshape to long format  
  # ###'
  # ###'
  # 
  # ### Gather to long data format
  # df <- df %>% 
  #   gather(key = "Subgroup", value = "Enrollment", starts_with("Enroll"))
  # 
  # 
  # ### Generate factor variable
  # df <- df %>%
  #   mutate(Subgroup = factor(Subgroup, 
  #                            levels = c("EnrollWhite", 
  #                                       "EnrollHispanic", 
  #                                       "EnrollAfrAm", 
  #                                       "EnrollAsian", 
  #                                       "EnrollFilipino", 
  #                                       "EnrollPacIsl", 
  #                                       "EnrollAmInd", 
  #                                       "EnrollTwoOrMore", 
  #                                       "EnrollNoEthRptd",
  #                                       "EnrollTotal", 
  #                                       "EnrollEL"), 
  #                            labels = c("White", 
  #                                       "Hispanic/Latino", 
  #                                       "Black", 
  #                                       "Asian", 
  #                                       "Filipino", 
  #                                       "Pacific Islander", 
  #                                       "American Indian/Alaska Native", 
  #                                       "Two or more races", 
  #                                       "Race Not reported", 
  #                                       "Total", 
  #                                       "English learner")))
  # 
  # 
  # 
  # ###'######################################################################
  # ###'
  # ###' Enrollment: Convert to numeric
  # ###'
  # ###'
  # 
  # df <- df %>%
  #   mutate(Enrollment = as.numeric(Enrollment))
  
  

  ###'######################################################################
  ###' 
  ###' Save data objects
  ###' 
  ###' 
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Save the resulting dataset
  save(df, file = paste0("CourseEnrollment", year_num, "_cleaned", ".rda"))
  write.dta(df, file = paste0("CourseEnrollment", year_num, "_cleaned", ".dta"))
  
  
  
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
  