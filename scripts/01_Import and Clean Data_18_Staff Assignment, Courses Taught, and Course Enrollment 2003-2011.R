
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - Staff Assign + Courses Taught + Course Enrollment  
###' 
###'  2003-2011 (8 fiscal years, 2009-2010 is not available)
###' 
###'  
###' Just clean raw file - Manipulate variable later
###' 
###' 
###' 20170707 JoonHo Lee
###' 20180929 JoonHo Lee
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
###' Import AssignmentCodes Reference data: AssignmentCodes11before
###'
###'

setwd(data_dir)

load(file = "AssignmentCodes11before_cleaned.rda")

assign_codes_df <- df; rm(df)



###'######################################################################
###'
###' Loop over years
###'
###'

years <- c(sprintf("%02d",seq(3, 8)), sprintf("%02d",seq(10, 11)))

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
  if (year_num %in% c("10", "11")){
    
    ### Import tab delimited text file
    df <- read.delim(file = paste0("assign", year_num, ".txt"), 
                     header = TRUE, 
                     colClasses = "character")
    
    classmode(df, everything())
    
    
    ### Convert empty strings to NA
    df <- df %>% mutate_all(.funs = empty_as_na)
    
    
  } else if (year_num %in% c("03", "04", "05", "06", "07", "08")) {
    
    ### Import tab delimited text file
    df <- read.dbf(file = paste0("assign", year_num, ".dbf"), as.is = TRUE)
    
    classmode(df, everything())
    
    
    ### Convert empty strings to NA
    df <- df %>% mutate_all(.funs = empty_as_na)
    
  }
  
  

  ###'######################################################################
  ###'
  ###' Academic year 
  ###'
  ###'
  
  df <- df %>%
    mutate(AcademicYear = paste0(year_num, 
                                 sprintf("%02d",as.numeric(year_num) + 1)))
  
  tabdf(df, AcademicYear)
  classmode(df, AcademicYear)
  
  
  
  ###'######################################################################
  ###'
  ###' RecID: 
  ###' DO NOT convert to numeric
  ###' 
  ###' REC_ID => RecID for coherent variable name over years 
  ###'
  ###'
  
  names(df)[1] <- "RecID"
  
  tabdf(df, RecID)
  classmode(df, RecID)
  
  
  # df <- df %>%
  #   mutate(REC_ID = as.numeric(REC_ID))
  
  
  
  ###'######################################################################
  ###'
  ###' CDS code: 
  ###' 
  ###' Generate County, District, and School Codes
  ###'
  ###'
  
  ### Rename CDS_CODE if different
  names(df)[names(df) %in% c("CDS_CODE", "CDSCODE")] <- "CDS_CODE"
  
  
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
  
  
  ### Differed process by years
  if (year_num %in% c("10", "11")){
    
    ### Rename SCHOOL
    names(df)[names(df) %in% c("SCHOOL", "SCHOOOL")] <- "SCHOOL"
    
    
    ###' Rearrange row and column orders
    ###' Note that COUNTY, DISTRICT, SCHOOL names are missing from 1993-2006
    df <- df %>%
      arrange(CountyCode, DistrictCode, SchoolCode, RecID) %>% 
      select(AcademicYear, 
             CDS_CODE, CountyCode, DistrictCode, SchoolCode,  
             COUNTY, DISTRICT, SCHOOL, RecID,
             everything())
    
  } else if (year_num %in% c("03", "04", "05", "06", "07", "08")) {
    
    ###' Rearrange row and column orders
    ###' Note that COUNTY, DISTRICT, SCHOOL names are missing from 1993-2006
    df <- df %>%
      arrange(CountyCode, DistrictCode, SchoolCode, RecID) %>% 
      select(AcademicYear, 
             CDS_CODE, CountyCode, DistrictCode, SchoolCode, RecID,
             everything())
  }
  
  
  
  ###'######################################################################
  ###'
  ###' Assign_Code
  ###'
  ###'
  
  classmode(df, everything())
  
  df <- df %>% 
    rename(Assign_Code = ASN_CODE)
  
  
  
  ###'######################################################################
  ###'
  ###' ASN_PCT
  ###' PERC_TIME
  ###' FTE
  ###'
  ###' ASN_PCT = FTE => EstimatedFTE
  ###'
  
  ### Check distributions & Drop FTE (Same with ASN_PCT / not meaningful)
  listvars(df, RecID, ASN_PCT, PERC_TIME, FTE, nrow = 1000)
  tabdf(df, PERC_TIME)
  tabdf(df, FTE)
  df <- df %>%
    select(-FTE)
  
  
  ### Rename ASN_PCT & Convert to numeric
  df <- rename(df, EstimatedFTE = ASN_PCT)
  temp <- tabdf(df, EstimatedFTE)
  df <- df %>%
    mutate(EstimatedFTE = as.numeric(EstimatedFTE))
  
  
  ### Convert to numeric: PERC_TIME (meaningful except year 10, 11)
  df <- df %>%
    mutate(PERC_TIME = as.numeric(PERC_TIME))
  
  
  
  ###'######################################################################
  ###'
  ###' Assign Type
  ###'
  ###'
  
  ### Rename CTYPE to Assign_Type
  df <- df %>%
    rename(Assign_Type = CTYPE)
  
  tabdf(df, Assign_Type)
  
  
  ### Convert to factor
  df <- df %>%
    mutate(Assign_Type = factor(Assign_Type, 
                                levels = c("T", "A", "P"), 
                                labels = c("Teacher", 
                                           "Administrator", 
                                           "Pupil_Services")))
  
  tabdf(df, Assign_Type)
  
  
  ### Reorder variables
  df <- df %>%
    select(AcademicYear:EstimatedFTE, PERC_TIME, Assign_Type,
           GD_LEV, M_ENROLL, F_ENROLL, 
           everything())
  
  
  
  ###'######################################################################
  ###'
  ###' GradeLevelCode
  ###' 
  ###' < 2010, 2011 >
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
  ###' 13 = Multiple grades, no majority (Kindergarten through grade eight)
  ###' 14 = Multiple grades, no majority (nine through twelve)
  ###' US = Ungraded
  ###' Blank = No response
  ###'
  ###'
  ###' < 2003-2008 >
  ###' A = Kindergarten through grade three
  ###' B = Grade four
  ###' C = Grade five
  ###' D = Grade six
  ###' E = Grade seven
  ###' F = Grade eight
  ###' G = Grade nine
  ###' H = Grade ten
  ###' I = Grade eleven
  ###' J = Grade twelve
  ###' K = Multiple grades, no majority (grades kindergarten through eight)
  ###' L = Multiple grades, no majority (grades nine through twelve)
  ###' Blank = No response
  ###'
  ###'
  
  ### Rename variable
  df <- df %>%
    rename(GradeLevelCode = GD_LEV)
  
  
  ### Check distribution
  tabdf(df, GradeLevelCode)
  
  
  ### Differed process by years
  if (year_num %in% c("10", "11")){
    
    ### Convert to missing values other than K-12, Multiple grades (13-14)
    idx <- df$GradeLevelCode %in% c("KN", sprintf("%02d",seq(1, 14)))
    df$GradeLevelCode[!idx] <- NA
    
    
    ### Convert to factor
    df <- df %>%
      mutate(GradeLevelCode = factor(GradeLevelCode, 
                                     levels = c("KN", sprintf("%02d",seq(1, 14))), 
                                     labels = c("KN", sprintf("%02d",seq(1, 12)), 
                                                "Multiple (K-8)", 
                                                "Multiple (9-12)")))
    
  } else if (year_num %in% c("03", "04", "05", "06", "07", "08")) {
    
    ### Convert to missing values other than K-12, Multiple grades (13-14)
    idx <- df$GradeLevelCode %in% c(LETTERS[1:12], letters[1:12])
    df$GradeLevelCode[!idx] <- NA
    
    
    ### Convert to upper case
    df$GradeLevelCode <- toupper(df$GradeLevelCode)
    
    
    ### Convert to factor
    df <- df %>%
      mutate(GradeLevelCode = factor(GradeLevelCode, 
                                     levels = LETTERS[1:12], 
                                     labels = c("K-3", sprintf("%02d",seq(4, 12)), 
                                                "Multiple (K-8)", 
                                                "Multiple (9-12)")))
    
    tabdf(df, GradeLevelCode)
  }
  

  
  ###'######################################################################
  ###'
  ###' Enrollment: M_ENROLL, F_ENROLL
  ###'
  ###'
  
  ### Convert to numeric
  df <- df %>%
    mutate_at(.vars = c("M_ENROLL", "F_ENROLL"), 
              .funs = as.numeric)
  
  
  ### Rename variables and Generate Total Enrollment
  df <- df %>%
    rename(Enroll_male = M_ENROLL, 
           Enroll_female = F_ENROLL) %>%
    mutate(Enroll_total = Enroll_male + Enroll_female) 
  
  df <- df %>%
    select(AcademicYear:GradeLevelCode, 
           starts_with("Enroll_"), 
           everything())
  
  
  
  ###'######################################################################
  ###'
  ###' UC CSU Approved
  ###'
  ###'
  
  df <- df %>%
    rename(UC_CSU_Approved = UC_CSU)
  
  tabdf(df, UC_CSU_Approved)
  
  df$UC_CSU_Approved <- recode(df$UC_CSU_Approved, 
                               "Y" = 1, "y" = 1, "N" = 0, "n" = 0, 
                               .default = NA_real_)

  classmode(df, UC_CSU_Approved)
  
  
  
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
  
  ### Differed process by years
  
  if (year_num %in% c("10", "11")){
    
    ### NCLB Core
    df <- df %>%
      rename(NCLB_Core = NCLB_CORE)
    
    tabdf(df, NCLB_Core)
    
    df$NCLB_Core <- recode_factor(df$NCLB_Core, 
                                  "N" = "Non-Core", 
                                  "E" = "Core Elementary", 
                                  "S" = "Core Secondary", 
                                  .default = NA_character_)
    
    classmode(df, NCLB_Core)
    
    
    ### NCLB HQT
    tabdf(df, NCLB_HQT)
    classmode(df, everything())
    df <- df %>%
      mutate(NCLB_HQT = factor(NCLB_HQT))
    
  } else if (year_num %in% c("05", "06", "07", "08")) {
    
    ### NCLB Core
    df <- df %>%
      rename(NCLB_Core = NCLB_CORE)
    
    tabdf(df, NCLB_Core)
    
    df$NCLB_Core <- recode_factor(df$NCLB_Core, 
                                  "N" = "Non-Core", 
                                  "E" = "Core Elementary", 
                                  "S" = "Core Secondary", 
                                  .default = NA_character_)
    
    classmode(df, NCLB_Core)
    
    
    ### NCLB HQT
    tabdf(df, NCLB_HQT)
    classmode(df, everything())
    
    idx <- df$NCLB_HQT %in% c("Y", "H", "N")
    df$NCLB_HQT[!idx] <- NA
    
    df <- df %>%
      mutate(NCLB_HQT = factor(NCLB_HQT, 
                               levels = c("Y", "H", "N")))
    
  }
  
  
  
  ###'######################################################################
  ###'
  ###' Recode dummy variables
  ###' 
  ###' "DIST_LRN":  Distance learning         
  ###' "SPEC_ED": Special education         
  ###' "IND_STUDY": Independent study       
  ###' "INTINERANT": taught by itinerant teacher 
  ###'
  ###'
  
  # ### Check distributions
  # tabdf(df, DIST_LRN)
  # tabdf(df, SPEC_ED)
  # tabdf(df, IND_STUDY)
  # tabdf(df, INTINERANT)
  
  if (year_num %in% c("10", "11")){
    
    ### Rename variables
    names(df)[names(df) %in% c("DIST_LRN")] <- "Distance_Learning"
    names(df)[names(df) %in% c("SPEC_ED")] <- "Special_ED"
    names(df)[names(df) %in% c("IND_STUDY")] <- "Independent_Study"
    names(df)[names(df) %in% c("INTINERANT", "ITINERANT")] <- "Itinerant_Teacher"
    
    
    ### Recode to numeric
    df <- df %>%
      mutate_at(.vars = c("Distance_Learning", 
                          "Special_ED", 
                          "Independent_Study", 
                          "Itinerant_Teacher"), 
                .funs = function(x){recode_factor(x, "Y" = 1, "y" = 1, 
                                                  "N" = 0, "n" = 0, 
                                                  .default = NA_real_)})
    
    
    ### Check distributions
    tabdf(df, Distance_Learning)
    tabdf(df, Special_ED)
    tabdf(df, Independent_Study)
    tabdf(df, Itinerant_Teacher)
  }
  
  

  ###'######################################################################
  ###' 
  ###' Save data objects
  ###' 
  ###' 
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Save the resulting dataset
  save(df, file = paste0("assign", year_num, "_cleaned", ".rda"))
  write.dta(df, file = paste0("assign", year_num, "_cleaned", ".dta"))
  
  
  
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

