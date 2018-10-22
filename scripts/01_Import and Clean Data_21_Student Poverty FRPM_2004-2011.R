
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - Student Poverty FRPM Data: 2004-2011
###' 
###' 
###' 20181007 JoonHo Lee 
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character/02_Student_Poverty_FRPM")


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

years <- sprintf("%02d",seq(4, 11))

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
  
  
  ### Import MS Excel file
  df <- read_excel(paste0("frpm", 
                          year_num, sprintf("%02d", as.numeric(year_num) + 1), 
                          ".xls"), 
                   sheet = 2, na = "N / A")
  
  classmode(df, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Rename variables
  ###'
  ###'
  
  ### Rename imported variables
  if (year_num %in% c("04")){
    
    names(df) <- c("CountyCode", "DistrictCode", "SchoolCode", 
                   "CharterNum", "FundingType", 
                   "DistrictName", "SchoolName", 
                   "Low_Grade", "High_Grade", 
                   "Enrollment", "Free_Meals", 
                   "Reduced_Price_Meals", 
                   "Total_FRPM", "PCT_FRPM")
    
  } else if (year_num %in% c("05")){
    
    names(df) <- c("CountyCode", "DistrictCode", "SchoolCode", 
                   "CharterNum", 
                   "DistrictName", "SchoolName", 
                   "Low_Grade", "High_Grade", 
                   "Enrollment", "Free_Meals", 
                   "Reduced_Price_Meals", 
                   "Total_FRPM", "PCT_FRPM")
    
  } else if (year_num %in% c("06", "07", "08", "09", "10")){
    
    names(df) <- c("CountyCode", "DistrictCode", "SchoolCode", 
                   "CharterNum", 
                   "CountyName", "DistrictName", "SchoolName", 
                   "Low_Grade", "High_Grade", 
                   "Enrollment", "Free_Meals", 
                   "Reduced_Price_Meals", 
                   "Total_FRPM", "PCT_FRPM")
    
  } else if (year_num %in% c("11")){
    
    names(df) <- c("CountyCode", "DistrictCode", "SchoolCode", 
                   "CharterNum", 
                   "CountyName", "DistrictName", "SchoolName", 
                   "Low_Grade", "High_Grade", 
                   "Enrollment", "Free_Meals", 
                   "Reduced_Price_Meals", 
                   "Total_FRPM", "PCT_FRPM", 
                   "Source")
  }
  
  classmode(df, everything())
  

  
  ###'######################################################################
  ###'
  ###' CDS Codes & District/School Names
  ###'
  ###'

  ### Convert to numeric
  df <- df %>%
    mutate_at(.vars = c("CountyCode", 
                        "DistrictCode", 
                        "SchoolCode"), 
              .funs = as.numeric)
  
  
  ### Reorder with CDS Names
  df <- df %>%
    select(ends_with("Code"), ends_with("Name"), everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Charter Number &  Funding Type
  ###' 
  ###' => Same information from public school listing "pubschls"
  ###'
  ###'
  
  classmode(df, everything())
  tabdf(df, CharterNum)
  
  
  if (year_num %in% c("04")){
    

    df <- df %>%
      mutate(CharterNum = as.numeric(CharterNum), 
             FundingType = factor(FundingType, 
                                  levels = c("L", "D", "N"), 
                                  labels = c("Locally Funded", 
                                             "Direct Funded", 
                                             "Not in the Funding Model")))
    
  } else if (year_num %in% c("05", "06", "07", "08", "09", "10")){
    
    df <- df %>%
      mutate(CharterNum = as.numeric(CharterNum))
    
  }
  
  
  
  ###'######################################################################
  ###' 
  ###' Grade: Low Grade, High Grade
  ###' 
  ###' => leave as they are. Not that useful information
  ###' 
  ###' 
  
  tabdf(df, Low_Grade)
  tabdf(df, High_Grade)
  
  
  
  ###'######################################################################
  ###' 
  ###' FRPM Variables
  ###' - Check variable types
  ###' - Convert percent scale: PCT_FRPM
  ###' 
  ###' 
  
  classmode(df, everything())
  
  df <- df %>%
    mutate(PCT_FRPM = 100*PCT_FRPM)
  
  
  
  ###'######################################################################
  ###'
  ###' Data Source
  ###' 
  ###' The source system for a given school's data.
  ###' 
  ###' - CARS = Consolidated Application and Reporting System
  ###' - CALPADS = California Longitudinal Pupil Achievement Data System
  ###'
  ###'

  if (year_num %in% c("11")){
    
    tabdf(df, Source)
    
  }
  
  
  
  ###'######################################################################
  ###' 
  ###' Academic Year
  ###' 
  ###' 
  
  df <- df %>%
    mutate(AcademicYear = as.numeric(year_num)) %>%
    select(AcademicYear, everything())

  
  
  ###'######################################################################
  ###' 
  ###' Save data objects
  ###' 
  ###' 
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Save the resulting dataset
  save(df, file = paste0("frpm", year_num, "_cleaned", ".rda"))
  write.dta(df, file = paste0("frpm", year_num, "_cleaned", ".dta"))
  
  
  
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
