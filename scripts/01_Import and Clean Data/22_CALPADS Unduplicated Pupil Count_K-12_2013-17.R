
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - CALPADS Unduplicated Pupil Count Source File (K–12): 2013-2017
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character/03_CALPADS_Unduplicated_Pupil_Count")


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

years <- sprintf("%02d",seq(13, 17))

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
  
  
  ### Assign filename
  filename <- paste0("cupc", year_num, sprintf("%02d", as.numeric(year_num) + 1))
  
  
  ### Import MS Excel file
  if (year_num %in% c("13")){
    
    df <- read_excel(paste0(filename, ".xls"), sheet = 3, na = "")
    
  } else if (year_num %in% c("14")){
    
    df <- read_excel(paste0(filename, ".xls"), sheet = 3, na = c("", "N/A"))
    
  } else if (year_num %in% c("15", "16")){
    
    df <- read_excel(paste0(filename, ".xls"), sheet = 3, na = c("", "N/A"), 
                     skip = 2)
    
  } else if (year_num %in% c("17")){
    
    df <- read_excel(paste0(filename, ".xlsx"), sheet = 3, na = c("", "N/A"), 
                     skip = 2)
    
  }
  
  classmode(df, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Rename variables
  ###'
  ###'
  
  ### Rename imported variables
  if (year_num %in% c("13")){
    
    names(df) <- c("AcademicYear", 
                   "CountyCode", "DistrictCode", "SchoolCode", 
                   "CountyName", "DistrictName", "SchoolName", 
                   "DistrictType", "SchoolType", 
                   "NSLP_Provision", 
                   "CharterNum", "FundingType", 
                   "Low_Grade", "High_Grade",
                   "Enrollment_K12",
                   "N_FRPM_Undup_unadj", 
                   "N_EL", 
                   "N_Foster", 
                   "N_Undup_Pupil_unadj", 
                   "N_Directly_Certfied", 
                   "N_Undup_Pupil_adj",
                   "N_Non_Juvenile_Hall", 
                   "N_Juvenile_Hall", 
                   "N_CALPADS_UPC", 
                   "CALPADS_Certified")
    
  } else if (year_num %in% c("14", "15", "16", "17")){
    
    names(df) <- c("AcademicYear", 
                   "CountyCode", "DistrictCode", "SchoolCode", 
                   "CountyName", "DistrictName", "SchoolName", 
                   "DistrictType", "SchoolType", "EdOptionType",  
                   "NSLP_Provision", 
                   "Charter", "CharterNum", "FundingType", "IRC", 
                   "Low_Grade", "High_Grade",
                   "Enrollment_K12",
                   "N_FRPM",
                   "N_Foster", 
                   "N_Homeless", 
                   "N_Migrant", 
                   "N_Directly_Certified",
                   "N_FRPM_Undup", 
                   "N_EL", 
                   "N_CALPADS_UPC", 
                   "CALPADS_Certified")
  }
  
  
  
  ###'######################################################################
  ###'
  ###' Academic Year
  ###'
  ###'
  
  tabdf(df, AcademicYear)
  classmode(df, AcademicYear)
  
  df <- df %>%
    mutate(AcademicYear = as.numeric(year_num))
  
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
  ###' => These will be merged from publischls data. Leave as they are.
  ###'
  ###'
  
  # tabdf(df, DistrictType)
  # tabdf(df, SchoolType)
  # tabdf(df, EdOptionType)
  
  
  
  ###'######################################################################
  ###' 
  ###' NSLP Provision
  ###' 
  ###' Whether a school is on a National School Lunch Program (NSLP) 
  ###' provision 2 or 3 status.
  ###' 
  ###' 
  
  tabdf(df, NSLP_Provision)
  
  if (year_num %in% c("13")){
    
    df <- df %>%
      mutate(NSLP_Provision = if_else(NSLP_Provision == "Y", 1, 0, 
                                      missing = NA_real_))
    
  }
  
  
  
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
  # tabdf(df, IRC)
  
  
  
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
  ###' Calculate Percentages
  ###' 
  ###' - Check variable types
  ###' - Generate PCT variables
  ###' 
  ###' 
  
  classmode(df, everything())
  
  if (year_num %in% c("13", "14", "15")){
    
    ### Copy multiple columns
    df_PCT <- df %>%
      select(names(df)[grepl("N_", names(df))])
    
    ### Rename variables
    names(df_PCT) <- gsub("N_", "PCT_", names(df_PCT))
    
    ### Calculate percentages
    df_PCT <- df_PCT %>%
      mutate_at(.vars = names(df_PCT)[grepl("PCT_", names(df_PCT))], 
                .funs = function(x) 100*(x/df$Enrollment_K12))
    
    ### Column bind with the original dataframe
    df <- cbind.data.frame(df, df_PCT)
  }
  
  
  
  ###'######################################################################
  ###' 
  ###' Delete "CALPADS_Certified"
  ###' 
  ###' 
  
  tabdf(df, CALPADS_Certified)
  
  df <- df %>%
    select(-CALPADS_Certified)
  
  
  
  ###'######################################################################
  ###' 
  ###' Save data objects
  ###' 
  ###' 
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Save the resulting dataset
  save(df, file = paste0(filename, "_cleaned",  ".rda"))
  write.dta(df, file = paste0(filename, "_cleaned", ".dta"))
  
  
  
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

