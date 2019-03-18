
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - CBEDS Data about Schools & Districts:
###' 
###' California Basic Educational Data System (CBEDS) 
###' downloadable data files for data about schools and districts 
###' for 2008–09 through current year.
###' 
###'  2008-2017 (10 fiscal years)
###'  
###'  
###'  (2) School-level data
###' 
###'  
###' Just clean raw file - Manipulate variable later
###' 
###' 
###' 20190210 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Staff_Data/Certificated_Staff/CBEDS Data")


### Call libraries
library(tidyverse)
library(readxl)
library(Hmisc)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Loop over years
###'
###'

years <- sprintf("%02d",seq(8, 17))

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
  
  
  ### Differed process by years
  
  if (year_num %in% c(sprintf("%02d",seq(8, 10)), 
                      sprintf("%02d",seq(12, 15)), "17")){
    
    ### Import tab delimited text file
    df <- read.delim(file = paste0("cbedsora", year_num, "b.txt"), 
                     header = TRUE, 
                     colClasses = "character", 
                     quote = "")
    
    classmode(df, everything())
    
  } else if (year_num %in% c("11")) {
    
    ### Import tab delimited text file
    df <- read.delim(file = paste0("cbedsora", year_num, "b.txt"), 
                     header = FALSE, 
                     colClasses = "character")
    
    classmode(df, everything())
    
    ### Assign variable names
    names(df) <- c("CdsCode", "CountyName", "DistrictName", "SchoolName", 
                   "Description", "Level", "Section", "RowNumber", 
                   "Value", "Year")
    
    ### Clean the first cell
    df[1, 1] <- df[2, 1]
    
  } else if (year_num %in% c("16")) {
    
    ### Import tab delimited text file
    df <- read.delim(file = paste0("cbedsora", year_num, "b_stata.txt"), 
                     header = TRUE, 
                     colClasses = "character")
    
    classmode(df, everything())
    
  }
  
  
  ### Convert empty strings to NA
  df <- df %>% mutate_all(.funs = empty_as_na)
  
  
  
  ###'######################################################################
  ###'
  ###' AcademicYear
  ###'
  ###'
  
  tabdf(df, Year)
  
  classmode(df, everything())
  
  df <- df %>%
    rename(AcademicYear = Year) 
  
  
  ### Filter out only valid academic years
  df <- df %>%
    filter(!is.na(AcademicYear))
  
  tabdf(df, AcademicYear)
  
  
  
  ###'######################################################################
  ###'
  ###' CDS Code
  ###'
  ###' Generate County, District, and School Codes
  ###' 
  ###'
  
  ### Rename as a consistent variable name
  idx <- names(df) %in% c("Cdscode", "CDSCODE", "CdsCode")
  names(df)[idx] <- "CDS_CODE"
  names(df)[1] <- "CDS_CODE"
  
  
  ### Check number of characters: should be 14
  table(nchar(df$CDS_CODE))
  
  
  ### Concatenate "0" for nchar = 13
  if (year_num == "16"){
    idx <- nchar(df$CDS_CODE) == 13
    df$CDS_CODE[idx] <- paste0("0", df$CDS_CODE[idx]) 
  }
  
  
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
  
  
  ### Differed process by years
  if (year_num %in% sprintf("%02d",seq(8, 10))){
    
    ###' Rearrange row and column orders
    ###' Note that COUNTY, DISTRICT, SCHOOL names are missing from 1993-2006
    df <- df %>%
      arrange(CountyCode, DistrictCode, SchoolCode) %>% 
      select(AcademicYear, 
             CDS_CODE, CountyCode, DistrictCode, SchoolCode,  
             everything())
    
  } else if (year_num %in% sprintf("%02d",seq(11, 17))) {
    
    ###' Rearrange row and column orders
    ###' Note that COUNTY, DISTRICT, SCHOOL names are missing from 1993-2006
    df <- df %>%
      arrange(CountyCode, DistrictCode, SchoolCode) %>% 
      select(AcademicYear, 
             CDS_CODE, CountyCode, DistrictCode, SchoolCode,  
             CountyName, DistrictName, SchoolName, 
             everything())
  }
  
  
  
  ###'######################################################################
  ###'
  ###' Level of data
  ###' 
  ###' D = District
  ###' S = School
  ###'
  ###'
  
  tabdf(df, Level)
  classmode(df, Level)
  
  df <- df %>%
    mutate(Level = recode(Level, "d" = "D", "s" ="S"))
  
  df <- df %>%
    mutate(Level = factor(Level, 
                          levels = c("D", "S"), 
                          labels = c("District", "School")))
  
  
  
  ###'######################################################################
  ###' 
  ###' Filter only Schools
  ###' 
  ###' => Now we want a school-level dataset
  ###' 
  
  tabdf(df, Level)
  
  df <- df %>%
    filter(Level == "School")
  
  
  
  ###'######################################################################
  ###'
  ###' Section
  ###' 
  ###' Corresponds to the sections on 
  ###' the County/District Information Form (CDIF) and 
  ###' School Information Form (SIF)
  ###' 
  ###' => Only School Level Data
  ###' 
  ###' 
  
  tabdf(df, Section)
  classmode(df, everything())
  
  
  ### Differed process by years
  if (year_num %in% sprintf("%02d", c(8))){
    
    df <- df %>%
      mutate(Section = recode_factor(Section, 
                                     "D" = "Education Options", 
                                     "E" = "Technology",
                                     "F" = "Educational Calendar",
                                     .default = NA_character_))
    
    
  } else if (year_num %in% sprintf("%02d", c(9))) {
    
    df <- df %>%
      mutate(Section = recode_factor(Section, 
                                     "D" = "Education Options", 
                                     "E" = "Technology",
                                     "F" = "Educational Calendar", 
                                     "G" = "Parental Exception Waivers", 
                                     "F" = "Bilingual Paraprofessionals", 
                                     .default = NA_character_))
    
    
  } else if (year_num %in% sprintf("%02d", c(10))) {
    
    df <- df %>%
      mutate(Section = recode_factor(Section, 
                                     "B" = "Education Options", 
                                     "C" = "Technology",
                                     "D" = "Educational Calendar", 
                                     "E" = "Parental Exception Waivers", 
                                     "F" = "Bilingual Paraprofessionals", 
                                     "I" = "High School Graduation Requirements (Charter)",
                                     .default = NA_character_))
    
    
  } else if (year_num %in% sprintf("%02d", c(11))) {
    
    df <- df %>%
      mutate(Section = recode_factor(Section, 
                                     "B" = "Education Options", 
                                     "C" = "Technology",
                                     "D" = "Educational Calendar", 
                                     "E" = "Parental Exception Waivers", 
                                     "F" = "Bilingual Paraprofessionals", 
                                     "H" = "High School Graduation Requirements (Charter)",
                                     "I" = "Increased Learning Time", 
                                     "J" = "School Year Minutes", 
                                     "K" = "Advanced Coursework/Dual Class Enrollment", 
                                     "L" = "Attendance Rates", 
                                     .default = NA_character_))
    
    
  } else if (year_num %in% sprintf("%02d", c(12, 13))) {
    
    df <- df %>%
      mutate(Section = recode_factor(Section, 
                                     "B" = "Education Options", 
                                     "C" = "Technology",
                                     "D" = "Educational Calendar", 
                                     "E" = "Parental Exception Waivers (prior year)", 
                                     "F" = "Bilingual Paraprofessionals", 
                                     "H" = "High School Graduation Requirements (Charter)",
                                     "I" = "Increased Learning Time (prior year)", 
                                     "J" = "School Year Minutes (prior year)", 
                                     "K" = "Advanced Coursework/Dual Class Enrollment (prior year)", 
                                     "L" = "Attendance Rates (prior year)", 
                                     "M" = "Truancy (prior year)", 
                                     .default = NA_character_))
    
    
  } else if (year_num %in% sprintf("%02d", c(14))) {
    
    df <- df %>%
      mutate(Section = recode_factor(Section, 
                                     "B" = "Education Options", 
                                     "D" = "Educational Calendar", 
                                     "E" = "Parental Exception Waivers (prior year)", 
                                     "I" = "Increased Learning Time (prior year)", 
                                     "J" = "School Year Minutes (prior year)", 
                                     "K" = "Advanced Coursework/Dual Class Enrollment (prior year)", 
                                     "L" = "Attendance Rates (prior year)", 
                                     "M" = "Truancy (prior year)", 
                                     .default = NA_character_))
    
    
  } else if (year_num %in% sprintf("%02d", c(15, 16))) {
    
    df <- df %>%
      mutate(Section = recode_factor(Section, 
                                     "B" = "Kindergarten Program Type", 
                                     "C" = "Truancy (prior year)", 
                                     "D" = "Educational Calendar", 
                                     "E" = "Parental Exception Waivers (prior year)", 
                                     "G" = "Increased Learning Time (prior year)", 
                                     "H" = "Advanced Coursework/Dual Class Enrollment (prior year)", 
                                     "I" = "Attendance Rates (prior year)", 
                                     "J" = "School Year Minutes (prior year)", 
                                     .default = NA_character_))
    
    
  } else if (year_num %in% sprintf("%02d", c(17))) {
    
    df <- df %>%
      mutate(Section = recode_factor(Section, 
                                     "B" = "Kindergarten Program Type", 
                                     "C" = "Truancy (prior year)", 
                                     "D" = "Educational Calendar", 
                                     "F" = "Increased Learning Time (prior year)", 
                                     "G" = "School Year Minutes (prior year)", 
                                     "H" = "Advanced Coursework/Dual Class Enrollment (prior year)", 
                                     "I" = "Attendance Rates (prior year)", 
                                     .default = NA_character_))
  }
  
  
  
  ###'######################################################################
  ###'
  ###' Row number, description, and values
  ###' 
  ###' => Leave as character. Convert when generating variables
  ###'
  ###'
  
  tabdf(df, RowNumber)
  tabdf(df, Value)
  
  # df <- df %>%
  #   mutate_at(.vars = c("RowNumber", "Value"), 
  #             .funs = as.numeric)
  
  
  
  ###'######################################################################
  ###'
  ###' Arrange rows and columns
  ###' 
  ###'
  
  if (year_num %in% sprintf("%02d",seq(8, 10))){
    
    df <- df %>%
      select(AcademicYear:SchoolCode, Level, 
             Section, RowNumber, Description, Value) %>%
      arrange(CountyCode, DistrictCode, SchoolCode, Section, RowNumber)
    
  } else if (year_num %in% sprintf("%02d",seq(11, 17))) {
    
    df <- df %>%
      select(AcademicYear:SchoolCode, 
             ends_with("Name"), Level, 
             Section, RowNumber, Description, Value) %>%
      arrange(CountyCode, DistrictCode, SchoolCode, Section, RowNumber)
    
  }
  
  
  
  ###'######################################################################
  ###' 
  ###' Save data objects
  ###' 
  ###' 
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Save the resulting dataset
  dualsave(df, paste0("CBEDS_School_Data_", year_num))
  
  
  
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

