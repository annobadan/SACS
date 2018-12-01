
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
###'  2003-2007 (5 fiscal years)
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

years <- sprintf("%02d", seq(4, 7))

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
  df <- read.dbf(file = paste0("cdifcg", year_num, ".dbf"), as.is = TRUE)
  
  classmode(df, everything())
  
  
  ### Convert empty strings to NA
  df <- df %>% mutate_all(.funs = empty_as_na)

  
  
  ###'######################################################################
  ###'
  ###' Academic Year
  ###'
  
  classmode(df, everything())
  
  df <- df %>%
    mutate(AcademicYear = paste0(year_num, 
                                 sprintf("%02d", as.numeric(year_num) + 1)))
  
  
  
  ###'######################################################################
  ###'
  ###' CD Code:
  ###' 
  ###' Split DistrictCode into "CountyCode" and "DistrictCode"
  ###' 
  ###'
  
  ### Rename CD_CODE
  idx <- names(df) %in% c("CDS_CODE", "CD_CODE")
  names(df)[idx] <- "CD_CODE"
  
  
  ### Check number of characters: should be 7
  table(nchar(df$CD_CODE))
  
  
  ### Substring CountyCode, DistrictCode
  tabdf(df, CD_CODE)
  df <- df %>%
    mutate(CountyCode = substr(CD_CODE, start = 1, stop = 2), 
           DistrictCode = substr(CD_CODE, start = 3, stop = 7))
  
  
  ### Convert to numeric
  df <- df %>%
    mutate_at(.vars = c("CountyCode", "DistrictCode"), 
              .funs = as.numeric)
  
  tabdf(df, CountyCode)
  tabdf(df, DistrictCode)

  
  ### Rearrange row and column
  df <- df %>% 
    select(AcademicYear, CD_CODE, CountyCode, DistrictCode, 
           everything()) %>%
    arrange(CountyCode, DistrictCode)

  
  
  ###'######################################################################
  ###'
  ###' Estimated Teacher Hires
  ###'
  ###'
  
  ### Select only applicable variables
  df_hires <- df %>%
    select(AcademicYear:DistrictCode, 
           AGR3:OTH3)
  
  
  ### Gather to long data format
  df_hires <- df_hires %>%
    gather(Description, Value, AGR3:OTH3)
  
  
  ### Tidy up the long data & Add tag
  df_hires <- df_hires %>%
    arrange(CountyCode, DistrictCode, Description) %>%
    mutate(Section = "Estimated Teacher Hires")
  
  
  ### Recode subjects
  subj_vec1 <- c("AGR3", "ART3", "ENG3", "BUS3", "LAN3", "ECO3", 
                 "LIFE3", "MATH3", "MUS3", "PE3", "PHY3", "READ3", 
                 "SOC3", "TRADES3", "SPEC3", "BIL3", "SELF3", "OTH3" )
  subj_vec2 <- c("Agriculture", "Art", "English", 
                 "Business", "Foreign Language", "Home economics", 
                 "Life Science", "Mathematics", "Music", 
                 "Physical Education, Health and Dance", "Physical Science", 
                 "Reading", "Social Science/Studies", "Trades and Industrial Arts", 
                 "Special Educatoin", "Bilingual Education", "Self-Contained Classes", 
                 "Other Specializations")
  
  subj_df <- data.frame(subj_vec1, subj_vec2)
  
  df_hires <- df_hires %>%
    left_join(subj_df, by = c("Description" = "subj_vec1")) %>%
    select(-Description) %>%
    rename(Description = subj_vec2) %>%
    select(AcademicYear:DistrictCode, Section, Description, Value)
  
  
  
  
  ###'######################################################################
  ###'
  ###' High School Graduation Requirements
  ###'
  ###'
 
  ### Select only applicable variables
  df_grad <- df %>%
    select(AcademicYear:DistrictCode, 
           ENGLISH:LAB_SCI)
  
  
  ### Gather to long data format
  df_grad <- df_grad %>%
    gather(Description, Value, ENGLISH:LAB_SCI)
  
  
  ### Tidy up the long data & Add tag
  df_grad <- df_grad %>%
    arrange(CountyCode, DistrictCode, Description) %>%
    mutate(Section = "High School Graduation Requirements")
  
  
  ### Recode subjects
  start <- which(names(df) == "ENGLISH")
  end <- which(names(df) == "LAB_SCI")
  
  subj_vec1 <- c("ENGLISH", "VISUAL", "FOR_ARTS", "PERFARTS", "HEALTH", "HISTORY", 
                 "MATH", "PHYS_ED", "SCIENCE", "COMM_SERV", "ELECTIVES", "OTHER", 
                 "TOT_UNITS", "COURSEUNIT", "ALGEBRA1", "GEOMETRY", "ALGEBRA2", 
                 "STATS", "LAB_SCI")
  subj_vec2 <- c("English", "Visual and Performing Arts (VPA)", 
                 "Foreign Language (FL)", 
                 "Either a FL or VPA", 
                 "Health", "History-Social Science", "Mathematics", 
                 "Physical Education", "Science", "Community Service", 
                 "Electives", "Other Subjects", "Total Units", "One-year Course Unit", 
                 "Algebra I or Integrated Mathematics I", 
                 "Geometry or Integrated Mathematics II", 
                 "Algebra II or Integrated Mathematics III", 
                 "Probability and Statistics or Data Analysis", 
                 "Laboratory Science")
    
  subj_df <- data.frame(subj_vec1, subj_vec2)
  
  df_grad <- df_grad %>%
    left_join(subj_df, by = c("Description" = "subj_vec1")) %>%
    select(-Description) %>%
    rename(Description = subj_vec2) %>%
    select(AcademicYear:DistrictCode, Section, Description, Value)
  
  
  
  ###'######################################################################
  ###' 
  ###' Bind rows and arrange
  ###' 
  ###' 

  df_long <- bind_rows(df_hires, df_grad) %>%
    arrange(CountyCode, DistrictCode, Section, Description)
  
  
  
  ###'######################################################################
  ###' 
  ###' Save data objects
  ###' 
  ###' 
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Save the resulting dataset
  save(df_long, file = paste0("CBEDS_District_Data_", year_num, ".rda"))
  write.dta(df_long, file = paste0("CBEDS_District_Data_", year_num, ".dta"))
  
  
  
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


