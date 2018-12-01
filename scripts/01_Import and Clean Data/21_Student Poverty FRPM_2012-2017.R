
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - Student Poverty FRPM Data: 2012-2017
###' 
###' 
###' 20181008 JoonHo Lee 
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
  
  
  ### Import MS Excel file
  if (year_num %in% c("12", "13")){
    
    df <- read_excel(paste0("frpm", 
                            year_num, sprintf("%02d", as.numeric(year_num) + 1), 
                            ".xls"), 
                     sheet = 2, na = "NULL")

  } else if (year_num %in% c("13")){
    
    df <- read_excel(paste0("frpm", 
                            year_num, sprintf("%02d", as.numeric(year_num) + 1), 
                            ".xls"), 
                     sheet = 2, na = "N/A")
    
  } else if (year_num %in% c("14", "15", "16")){
    
    df <- read_excel(paste0("frpm", 
                            year_num, sprintf("%02d", as.numeric(year_num) + 1), 
                            ".xls"), 
                     sheet = 2, na = "N/A", skip = 1)
    
  } else if (year_num %in% c("17")){
    
    df <- read_excel(paste0("frpm", 
                            year_num, sprintf("%02d", as.numeric(year_num) + 1), 
                            ".xlsx"), 
                     sheet = 2, na = "N/A", skip = 1)
    
  }
  
  classmode(df, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Rename variables
  ###'
  ###'
  
  ### Rename imported variables
  if (year_num %in% c("12")){
    
    names(df) <- c("AcademicYear", 
                   "CountyCode", "DistrictCode", "SchoolCode", 
                   "CharterNum", 
                   "NSLP_Provision",
                   "Source", 
                   "CountyName", "DistrictName", "SchoolName", 
                   "Low_Grade", "High_Grade", 
                   "Enrollment_K-12", 
                   "N_Free_K-12", 
                   "PCT_Free_K-12", 
                   "N_FRPM_K-12_Undup", 
                   "PCT_FRPM_K-12", 
                   "Enrollment_5-17", 
                   "N_Free_5-17", 
                   "PCT_Free_5-17", 
                   "N_FRPM_5-17_Undup", 
                   "PCT_FRPM_5-17")
    
  } else if (year_num %in% c("13")){
    
    names(df) <- c("AcademicYear", 
                   "CountyCode", "DistrictCode", "SchoolCode",
                   "CountyName", "DistrictName", "SchoolName", 
                   "NSLP_Provision",
                   "CharterNum", "FundingType", 
                   "Low_Grade", "High_Grade",
                   "Enrollment_K-12", 
                   "N_Free_K-12_unadj",
                   "N_Free_K-12_adj", 
                   "PCT_Free_K-12_adj", 
                   "N_FRPM_K-12_unadj",
                   "N_FRPM_K-12_adj",
                   "PCT_FRPM_K-12_adj", 
                   "Enrollment_5-17", 
                   "N_Free_5-17_unadj",
                   "N_Free_5-17_adj", 
                   "PCT_Free_5-17_adj", 
                   "N_FRPM_5-17_unadj",
                   "N_FRPM_5-17_adj",
                   "PCT_FRPM_5-17_adj", 
                   "CALPADS_status")
    
  } else if (year_num %in% c("14", "15", "16", "17")){
    
    names(df) <- c("AcademicYear", 
                   "CountyCode", "DistrictCode", "SchoolCode",
                   "CountyName", "DistrictName", "SchoolName",
                   "DistrictType", "SchoolType", "EdOptionType", 
                   "NSLP_Provision",
                   "Charter", "CharterNum", "FundingType", "IRC", 
                   "Low_Grade", "High_Grade",
                   "Enrollment_K-12", 
                   "N_Free_K-12",
                   "PCT_Free_K-12", 
                   "N_FRPM_K-12",
                   "PCT_FRPM_K-12", 
                   "Enrollment_5-17", 
                   "N_Free_5-17",
                   "PCT_Free_5-17", 
                   "N_FRPM_5-17",
                   "PCT_FRPM_5-17", 
                   "CALPADS_status")
    
  }
  
  classmode(df, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Academic Year
  ###'
  ###'
  
  tabdf(df, AcademicYear)
  classmode(df, AcademicYear)
  
  df <- df %>%
    mutate(AcademicYear = as.numeric(substr(AcademicYear, 3, 4)))
  
  ### Drop NA rows
  df <- df %>%
    filter(!is.na(AcademicYear))

    

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
  
  classmode(df, everything())
  

  ### Reorder with CDS Names
  df <- df %>%
    select(AcademicYear, 
           ends_with("Code"), ends_with("Name"), 
           everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Type variables
  ###' 
  ###' => These will be merged from publischls data
  ###'
  ###'
   
  # classmode(df, everything())
  # tabdf(df, DistrictType)
  # tabdf(df, SchoolType)
  # tabdf(df, EdOptionType)
  
  
  
  ###'######################################################################
  ###'
  ###' Charter Number &  Funding Type
  ###' 
  ###' => Same information from public school listing "pubschls"
  ###'
  ###'
  
  # classmode(df, everything())
  # tabdf(df, CharterNum)
  # tabdf(df, FundingType)
  
  
  
  ###'######################################################################
  ###' 
  ###' Grade: Low Grade, High Grade
  ###' 
  ###' => leave as they are. Not that useful information
  ###' 
  ###' 
  
  # tabdf(df, Low_Grade)
  # tabdf(df, High_Grade)
  
  
  
  ###'######################################################################
  ###' 
  ###' NSLP Provision
  ###' 
  ###' Whether a school is on a National School Lunch Program (NSLP) 
  ###' provision 2 or 3 status.
  ###' 
  ###' 
  
  tabdf(df, NSLP_Provision)
  
  if (year_num %in% c("12", "13")){
    
    df <- df %>%
      mutate(NSLP_Provision = if_else(NSLP_Provision == "Y", 1, 0, 
                                      missing = NA_real_))
    
  }

  
  
  ###'######################################################################
  ###' 
  ###' FRPM Variables
  ###' - Check variable types
  ###' - Convert percent scale: PCT_FRPM
  ###' 
  ###' 
  
  classmode(df, everything())
  
  df <- df %>%
    mutate_at(.vars = names(df)[grepl("PCT_", names(df))], 
              .funs = function(x) x*100)
  
  
  
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
