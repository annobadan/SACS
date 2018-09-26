
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - Staff Credential Record
###' 
###' 2012-13, 2013-14, 2014-15, 2015-16, 2016-17, 2017-18
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
library(ldat)
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
  
  
  ### Import tab delimited text file
  df <- read.delim(file = paste0("StaffCred", year_num, ".txt"), 
                   header = TRUE, 
                   colClasses = "character")
  
  classmode(df, everything())
  
  
  ### Convert empty strings to NA
  df <- df %>% mutate_all(funs(empty_as_na)) 
  
  
  
  ###'######################################################################
  ###'
  ###' Academic year 
  ###'
  
  ### Assign variable name in case "AcademicYear" is not imported
  names(df)[1] <- "AcademicYear"
  
  
  ###' Validate data structure
  ###' FY1314 has duplicated cases, and unnecessary cases of FY1213 data 
  tabdf(df, AcademicYear)
  idx <- which(df$AcademicYear == "AcademicYear")
  df[c(idx - 1, idx, idx + 1), ]
  
  
  ### For FY1314, filter only AcademicYear == 1314
  if (year_num == "13") {
    
    df <- df %>%
      filter(AcademicYear == "1314") %>%
      distinct(AcademicYear, RecID, CredentialType, AuthorizationType, FileCreated)
    
  }
  
  
  ### Convert to numeric
  df <- df %>%
    mutate(AcademicYear = as.numeric(AcademicYear))
  
  
  
  ###'######################################################################
  ###'
  ###' RecID
  ###'
  
  tabdf(df, RecID)
  
  df <- df %>%
    mutate(RecID = as.numeric(RecID))
  
  
  
  ###'######################################################################
  ###'
  ###' CredentialType
  ###' 
  ###' 10	Full credential
  ###' 20	University Intern
  ###' 30	District Intern
  ###' 40	Waiver
  ###' 50	Provisional Internship Permit
  ###' 60	Short-term Staff Permit
  ###' 70	Child Development or Children’s Center Permit
  ###' 80	Emergency or Long-Term Emergency Permits
  ###' 85	Limited Assignment Teaching Permit
  ###' 90	Certificate of Clearance
  ###' 95	Activity Supervisor Clearance Certificate
  ###' 
  ###' 
  
  ### Generate reference table
  CredentialType <- c(10, 20, 30, 40, 50, 60, 70, 80, 85, 90, 95)
  CredentialType_Name <- c('Full credential', 'University Intern', 'District Intern', 
                           'Waiver', 'Provisional Internship Permit', 'Short-term Staff Permit', 
                           'Child Development or Children’s Center Permit', 
                           'Emergency or Long-Term Emergency Permits', 
                           'Limited Assignment Teaching Permit', 
                           'Certificate of Clearance', 
                           'Activity Supervisor Clearance Certificate')
  
  cred_table <- data.frame(CredentialType, CredentialType_Name)
  
  
  ### Merge to the original dataset
  classmode(df, CredentialType)
  
  df <- df %>%
    mutate(CredentialType = as.numeric(CredentialType)) %>%
    left_join(cred_table, by = c("CredentialType")) %>%
    select(AcademicYear, RecID, starts_with("CredentialType"), everything())
  
  
  
  ###'######################################################################
  ###'
  ###' AuthrizationType
  ###'
  ###' 100	Elementary/Self-Contained Classroom/Multiple Subject
  ###' 107	Secondary – any single subject
  ###' 110	Agriculture
  ###' 120	Art
  ###' 130	Biology
  ###' 140	Biology (specialized)
  ###' 150	Business
  ###' 160	Chemistry
  ###' 170	Chemistry (specialized)
  ###' 180	English
  ###' 190	Foundational-Level General Science
  ###' 200	Foundational-Level Mathematics
  ###' 210	Geoscience
  ###' 220	Geoscience (specialized)
  ###' 230	Health Science
  ###' 240	Home Economics
  ###' 250	Industrial and Technology
  ###' 260	Languages Other Than English
  ###' 270	Life Science
  ###' 280	Mathematics
  ###' 290	Music
  ###' 300	Physical Education
  ###' 310	Physical Science
  ###' 320	Physics
  ###' 330	Physics (specialized)
  ###' 340	Social Science
  ###' 350	Career Technical Education/Vocational
  ###' 360	Adult Education
  ###' 370	English Language Development (ELD) ONLY
  ###' 375	English Language Development (ELD) AND Specially Designed Academic Instruction in English (SDAIE)
  ###' 380	Primary Language Instruction (BCLAD or equivalents) and SDAIE and ELD
  ###' 390	Reading Specialist/Certificate
  ###' 400	Special Designated Subjects
  ###' 410	Special Education
  ###' 420	Specially Designed Academic Instruction in English (SDAIE) ONLY
  ###' 999	No authorization found for specified credential and time period
  ###'
  ###'
  
  ### Generate reference table
  AuthorizationType <- c(100, 107, 110, 120, 130, 140, 150, 160, 170, 180, 190, 
                         200, 210, 220, 230, 240, 250, 260, 270, 280, 290, 
                         300, 310, 320, 330, 340, 350, 360, 370, 375, 380, 390, 
                         400, 410, 420, 999)
  
  AuthorizationType_Name <- c('Elementary/Self-Contained Classroom/Multiple Subject', 
                              'Secondary_any single subject', 
                              'Agriculture', 
                              'Art', 
                              'Biology', 'Biology (specialized)', 
                              'Business', 
                              'Chemistry', 'Chemistry (specialized)', 
                              'English', 
                              'Foundational-Level General Science', 
                              'Foundational-Level Mathematics', 
                              'Geoscience', 'Geoscience (specialized)', 
                              'Health Science', 
                              'Home Economics', 
                              'Industrial and Technology', 
                              'Languages Other Than English', 
                              'Life Science', 
                              'Mathematics', 
                              'Music', 
                              'Physical Education', 
                              'Physical Science', 
                              'Physics', 
                              'Physics (specialized)', 
                              'Social Science', 
                              'Career Technical Education/Vocational', 
                              'Adult Education', 
                              'English Language Development (ELD) ONLY', 
                              'English Language Development (ELD) AND Specially Designed Academic Instruction in English (SDAIE)', 
                              'Primary Language Instruction (BCLAD or equivalents) and SDAIE and ELD', 
                              'Reading Specialist/Certificate', 
                              'Special Designated Subjects', 
                              'Special Education', 
                              'Specially Designed Academic Instruction in English (SDAIE) ONLY', 
                              'No authorization found for specified credential and time period')
  
  autho_table <- data.frame(AuthorizationType, AuthorizationType_Name)
  
  
  ### Merge to the original dataset
  classmode(df, AuthorizationType)
  
  df <- df %>%
    mutate(AuthorizationType = as.numeric(AuthorizationType)) %>%
    left_join(autho_table, by = c("AuthorizationType")) %>%
    select(AcademicYear, RecID, starts_with("AuthorizationType"), everything())
  

  
  ###'######################################################################
  ###' 
  ###' Save data objects
  ###' 
  ###' 
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Save the resulting datasets
  save(df, file = paste0("StaffCred", year_num, "_cleaned", ".rda"))
  write.dta(df, file = paste0("StaffCred", year_num, "_cleaned", ".dta"))
  
  
  
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
