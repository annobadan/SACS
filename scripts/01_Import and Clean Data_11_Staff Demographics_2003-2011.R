
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
  
  
  ### Differed process by years
  if (year_num %in% c("09", "10", "11")){
    
    ### Import tab delimited text file
    df <- read.delim(file = paste0("paif", year_num, ".txt"), header = TRUE, 
                     colClasses = "character")
    
    classmode(df, everything())
    
    
    ### Convert empty strings to NA
    df <- df %>% mutate_all(.funs = empty_as_na)
    
  } else if (year_num %in% c("03", "04", "05", "06", "07", "08")) {
    
    ### Import tab delimited text file
    df <- read.dbf(file = paste0("paif", year_num, ".dbf"), as.is = TRUE)
    
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
      arrange(CountyCode, DistrictCode, SchoolCode, RecID) %>% 
      select(AcademicYear, 
             CDS_CODE, CountyCode, DistrictCode, SchoolCode,  
             COUNTY, DISTRICT, SCHOOL, RecID,
             everything())
    
  } else if (year_num %in% c("03", "04", "05", "06", "07", "08", "09")) {
    
    df <- df %>%
      arrange(CountyCode, DistrictCode, SchoolCode, RecID) %>% 
      select(AcademicYear, 
             CDS_CODE, CountyCode, DistrictCode, SchoolCode, RecID,
             everything())
  }

  
  ###'######################################################################
  ###' 
  ###' Convert variable names to uppercase
  ###' 
  ###' Applicable only for 2009
  ###' 
  ###' 
  
  if (year_num == "09"){
    
    ### Variables to convert to upper case
    varnames_vec <- c("gender", "ed_level", "ethnic_gp", 
                      "yrs_teach", "yrs_dist", "status")
    
    
    ### Get vars positions
    idx <- which(names(df) %in% varnames_vec)
    
    
    ### Convert to upper cases
    names(df)[idx] <- toupper(varnames_vec)
    
    
    ### Make sure
    names(df)
    
  }

  
  
  ###'######################################################################
  ###' 
  ###' Gender Code: factor labeling
  ###' 
  ###' 
  
  ### Assign consistent variable name
  df <- df %>%
    rename(GenderCode = GENDER)
  
  tabdf(df, GenderCode)
  
  
  ### Convert to factor
  df <- df %>%
    mutate(GenderCode = factor(GenderCode, 
                               levels = c("F", "M"), 
                               labels = c("female", "male")))
  
  classmode(df, GenderCode)
  
  

  ###'######################################################################
  ###' 
  ###' Education Level: Dummy for Master / Doctorate
  ###' 
  ###' << 2010 - 2017 >>
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
  ###' << 2003 - 2009 >>
  ###' 
  ###' 1 = Doctorate
  ###' 2 = Master's degree plus 30 or more semester hours
  ###' 3 = Master's degree
  ###' 4 = Bachelor's degree plus 30 or more semester hours
  ###' 5 = Bachelor's degree
  ###' 6 = Less than bachelor's degree
  ###' 7 - Not reported
  ###' 
  ###' 

  ### Assign consistent variable name
  df <- df %>%
    rename(EducationLevel = ED_LEVEL)
  
  tabdf(df, EducationLevel)
  classmode(df, EducationLevel)
  
  
  ### Differed process by years
  if (year_num %in% c("10", "11")){
    
    ### Recode missing values
    df$EducationLevel[df$EducationLevel == "N"] <- NA
    tabdf(df, EducationLevel)
    
    
    ### Generate factor variable
    df <- df %>%
      mutate(EducationLevel = factor(EducationLevel, 
                                     levels = c("A", "B", "C", "F", "Y", "U", 
                                                "M", "V", "S", "D")))
    
    tabdf(df, EducationLevel)
    
  } else if (year_num %in% c("03", "04", "05", "06", "07", "08", "09")) {
    
    ### Recode missing values
    df$EducationLevel[df$EducationLevel == 7] <- NA
    tabdf(df, EducationLevel)
    classmode(df, EducationLevel)
    
    
    ### Generate factor variable
    df <- df %>%
      mutate(EducationLevel = factor(EducationLevel, 
                                     levels = c("6", "5", "4", "3", "2", "1"), 
                                     labels = c("Less than Bachelor's", 
                                                "Bachelor's", 
                                                "Bachelor's Plus", 
                                                "Master's", 
                                                "Master's Plus", 
                                                "Doctorate")))
    
    tabdf(df, EducationLevel)
    
  }

  
  
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
  
  ### Assign consistent variable name
  df <- df %>%
    rename(EthnicGroup = ETHNIC_GP)
  
  tabdf(df, EthnicGroup)

  
  ### Differed process by years
  if (year_num %in% c("10", "11")){
    
    ### Recode missing values
    df$EthnicGroup[df$EthnicGroup == 0] <- NA  # Many missing values
    
    
    ### Factor labeling
    df <- df %>%
      mutate(EthnicGroup = factor(EthnicGroup, 
                                  levels = c(7, 5, 6, 2, 4, 3, 1, 9), 
                                  labels = c("White", "Hispanic/Latino", "Black", 
                                             "Asian", "Filipino", "Pacific Islander", 
                                             "American Indian/Alaska Native", 
                                             "Two or more races")))
    
    tabdf(df, EthnicGroup)
    
  } else if (year_num %in% c("09")) {
    
    ### Recode missing values
    df$EthnicGroup[df$EthnicGroup == 0] <- NA  # Many missing values
    
    
    ### Factor labeling
    df <- df %>%
      mutate(EthnicGroup = factor(EthnicGroup, 
                                  levels = c(7, 5, 6, 2, 4, 3, 1, 8), 
                                  labels = c("White", "Hispanic/Latino", "Black", 
                                             "Asian", "Filipino", "Pacific Islander", 
                                             "American Indian/Alaska Native", 
                                             "Two or more races")))
    
    tabdf(df, EthnicGroup)
    
  } else if (year_num %in% c("03", "04", "05", "06", "07", "08")) {
    
    ### Recode missing values
    df$EthnicGroup[df$EthnicGroup == 8] <- NA  # Many missing values
    
    
    ### Factor labeling
    df <- df %>%
      mutate(EthnicGroup = factor(EthnicGroup, 
                                  levels = c(7, 5, 6, 2, 4, 3, 1), 
                                  labels = c("White", "Hispanic/Latino", "Black", 
                                             "Asian", "Filipino", "Pacific Islander", 
                                             "American Indian/Alaska Native")))
    
    tabdf(df, EthnicGroup)
    
  }
  
  
  
  ###'######################################################################
  ###' 
  ###' Years Teaching / Years in district
  ###' 
  ###' 
  
  ### Assign consistent variable names
  df <- df %>%
    rename(YearsTeaching = YRS_TEACH, 
           YearsInDistrict = YRS_DIST)
  
  
  ### Convert to numeric
  df <- df %>%
    mutate_at(.vars = c("YearsTeaching", "YearsInDistrict"), 
              .funs = as.numeric)
  
  
  ### Check distributions
  tabdf(df, YearsTeaching)
  tabdf(df, YearsInDistrict)
  
  
  ### Convert negative integers to missing varlues
  df$YearsTeaching[df$YearsTeaching < 0] <- NA
  df$YearsInDistrict[df$YearsInDistrict < 0] <- NA
  
  
  
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
  
  ### Assign consistent name
  df <- df %>%
    rename(EmploymentStatusCode = STATUS)
  
  tabdf(df, EmploymentStatusCode)
  
  
  ### Recode wrongly classified categories 
  df$EmploymentStatusCode[df$EmploymentStatusCode == "l" 
                          & !is.na(df$EmploymentStatusCode)] <- "L"
  
  
  ### Recode missing values
  df$EmploymentStatusCode[!df$EmploymentStatusCode %in% c("L", "O", "P", "T")] <- NA
  
  
  ### Generate factor variable
  df <- df %>%
    mutate(EmploymentStatusCode = factor(EmploymentStatusCode, 
                                         levels = c("L", "P", "T", "O"), 
                                         labels = c("Long term substitute/temporary", 
                                                    "Probationary", 
                                                    "Tenured", 
                                                    "Other")))
  
  tabdf(df, EmploymentStatusCode)
  classmode(df, EmploymentStatusCode)
  
  
  
  ###'######################################################################
  ###' 
  ###' FTE-related variables:
  ###' 
  ###' Teacher, Administrator, Pupil Services
  ###' 
  ###' 
  
  ### Differed process by years
  if (year_num %in% c("10", "11")){
    
    ### Assign consistent variable names
    df <- df %>%
      rename(FTE_Teaching = PCTTEACH, 
             FTE_administrative = PCTADMIN, 
             FTE_PupilServices = PCTPUPIL)
    
    ### Convert to numeric
    df <- df %>%
      mutate_at(.vars = c("TEACH", "FTE_Teaching", 
                          "ADMIN", "FTE_administrative", 
                          "PUPIL", "FTE_PupilServices"), 
                .funs = as.numeric)
    
    classmode(df, everything())
    
    ### Drop FTE Indicators
    df <- df %>%
      select(-TEACH, -ADMIN, -PUPIL)
    
  } else if (year_num %in% c("09")) {
    
    ### Assign consistent variable names
    df <- df %>%
      rename(FTE_Teaching = TEACH, 
             FTE_administrative = ADMIN, 
             FTE_PupilServices = PUPIL)
    
    ### Convert to numeric
    df <- df %>%
      mutate_at(.vars = c("FTE_Teaching", 
                          "FTE_administrative", 
                          "FTE_PupilServices"), 
                .funs = function(x){as.numeric(x) * 100})
    
    classmode(df, everything())

  } else if (year_num %in% c("03", "04", "05", "06", "07", "08")) {
    
    ### Assign consistent variable names
    df <- df %>%
      rename(Dummy_Teaching = TEACH, 
             Dummy_administrative = ADMIN, 
             Dummy_PupilServices = PUPIL)
    
    ### Convert to numeric
    df <- df %>% 
      mutate_at(.vars = c("Dummy_Teaching", 
                          "Dummy_administrative", 
                          "Dummy_PupilServices"), 
                .funs = as.numeric)
    
    ### Clean other FTE-related variables
    df <- df %>% 
      mutate(F_P_TIME = factor(F_P_TIME, 
                               levels = c("F", "P"), 
                               labels = c("Full-time", 
                                          "Part-time")))
    tabdf(df, F_P_TIME)
    
    df <- df %>%
      mutate_at(.vars = c("PERC_TIME", "OVER_100"), 
                .funs = as.numeric)
    
    ### Reorder FTE-related variables 
    df <- df %>%
      select(AcademicYear:EmploymentStatusCode, 
             starts_with("Dummy_"), F_P_TIME, PERC_TIME, OVER_100, 
             everything())
  }
  
  
  
  ###'######################################################################
  ###'
  ###' Extract Staff Credential Records
  ###' 
  ###' 1) Authorization Type 
  ###' 2) Credential Type
  ###' 
  ###' => Extract as a long data format
  ###'
  ###'
 
  if (year_num %in% c("03", "04", "05", "06", "07", "08")){
    
    ### Define necessary variables names to select
    IDs <- c("AcademicYear", 
             "CountyCode", "DistrictCode", "SchoolCode", "RecID")
    
    varnames_Cred <- c("FULL_CRED", "UNIV_INT", "DIST_INT", "PRE_INT", 
                       "WAIVER", "PIP", "STSP", "EMERGENCY")
    
    varnames_Auth <- c("ELEM", "SEC", "GEN_SEC", "AGRI", "ART", "BUSINESS", 
                       "ENGLISH", "FOR_LANG", "HEALTH", "HOME_ECON", "IND_TECH", 
                       "LIFE_SCI", "MATH", "MUSIC", "PHYS_ED", "PHYS_SCI", 
                       "SOC_SCI", "VOC", "SPEC_ED", "READING", "BCC", "ELD", 
                       "SDAIE", "ADULT", "SPECSUBJ")
    
    AuthorizationType_Name <- c('Elementary/Self-Contained Classroom/Multiple Subject', 
                                'Secondary_any single subject', 
                                'Secondary General_all subjects', 
                                'Agriculture', 
                                'Art', 
                                'Business', 
                                'English', 
                                'Languages Other Than English', 
                                'Health Science', 
                                'Home Economics', 
                                'Industrial and Technology', 
                                'Life Science', 
                                'Mathematics', 
                                'Music', 
                                'Physical Education', 
                                'Physical Science', 
                                'Social Science', 
                                'Career Technical Education/Vocational', 
                                'Special Education',
                                'Reading Specialist/Certificate', 
                                'Bilingual', 
                                'English Language Development (ELD)', 
                                'Specially Designed Academic Instruction in English (SDAIE)', 
                                'Adult Education', 
                                'Special Designated Subjects')
    
    varnames_Auth_df <- data.frame(varnames_Auth, AuthorizationType_Name, 
                                   stringsAsFactors = FALSE)
    
    
    ### Extract and covert to long data format
    df_Cred <- df[names(df) %in% IDs |
                    names(df) %in% varnames_Cred | 
                    names(df) %in% varnames_Auth]
    
    df_Cred_long <- df_Cred %>%
      gather(key, value, varnames_Auth) %>%
      arrange(CountyCode, DistrictCode, SchoolCode, RecID) %>%
      filter(value == TRUE) %>%
      select(-value) %>% 
      left_join(varnames_Auth_df, by = c("key" = "varnames_Auth")) %>%
      rename(AuthorizationType_Key = key) %>%
      mutate_at(.vars = names(df)[names(df) %in% varnames_Cred], 
                .funs = as.numeric)
  
    
    ### Save the resulting dataset
    setwd(data_dir)
    save(df_Cred_long, file = paste0("StaffCred", year_num, "_cleaned", ".rda"))
    write.dta(df_Cred_long, file = paste0("StaffCred", year_num, "_cleaned", ".dta"))
    
    
    ### Drop Cred/Auth variables from df 
    idx <- !names(df) %in% c(varnames_Cred, varnames_Auth)
    df <- df[, idx]
    
  } 
  
  
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



