
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - California Assessment of Student Progress and Performance (CAASPP)
###' 
###'   Smarter Balanced Assessments (SBA)
###'   
###'   
###' 2014-2015 is our Baseline Year for the Smarter Balanced Assessment Consortium (SBAC) 
###' state testing in ELA and Math (LCAP Goals #11 and #12 – pages 12-13)
###'  
###' 
###' 20181026 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Statewide_Student_Assessment/CAASPP/Smarter Balanced Assessments")


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
###' Loop over 
###' 
###' (1) years: Academic Year 1415-1718
###'
###'     Each school selects its own testing window. 
###'     Testing can begin as early as January and 
###'     continue through the last day of school.
###'
###' (2) All students file & All subgroups file
###'
###'

years <- sprintf("%02d", seq(14, 17))

filetype <- c("All_Students", "All_Subgroups")


for (i in seq_along(years)){  # (1) Loop over years
  for (j in seq_along(filetype)){   # (2) Loop over filetype
    
    
    ###'######################################################################
    ###'
    ###' Assign year number & filetype
    ###' 
    ###' 
    
    year_num <- years[i]
    
    filetype_char <- filetype[j]
    
    
    
    ###'######################################################################
    ###'
    ###' Import raw csv file
    ###'
    ###'
    
    ### Set data containing working directory
    setwd(data_dir)
    
    
    ### Assign filenames
    filename <- paste0("sb_ca", 
                       year_num,  
                       sprintf("%02d", as.numeric(year_num) + 1), 
                       "_", 
                       filetype_char)
    
    
    ### Import the statewide research data file
    df <- read.csv(file = paste0(filename, ".csv"), 
                   header = TRUE, colClasses = "character", 
                   na.strings = c("*"))
    
    classmode(df, everything())
    
    
    ### Import subgroup information
    df_subgrp <- read.csv(file = "Subgroups.csv", 
                          header = TRUE, colClasses = "character", 
                          na.strings = c("*"))
    
    df_subgrp <- df_subgrp %>% 
      select(-X) %>%
      mutate(ID_subgrp = as.numeric(ID_subgrp))
    
    classmode(df_subgrp, everything())
    
    
    ### Convert empty strings to NA
    df <- df %>% mutate_all(funs(empty_as_na)) 
    
    
    
    ###'######################################################################
    ###'
    ###' Assign variable names
    ###'
    ###'
    
    classmode(df, everything())
    
    varnames <- c("CountyCode", "DistrictCode", "SchoolCode", 
                  "Filler", 
                  "TestYear", "Subgroup", "TestType", 
                  "N_CAASPP_Enrollment", "N_Tested_Entity", "N_Tested_Subgroup", 
                  "Grade", "TestID", 
                  "N_CAASPP_Enrollment_Reported", "N_Tested", 
                  "Mean_Scale_Score", 
                  "PCT_Exceeded", 
                  "PCT_Met", 
                  "PCT_Met_and_Above", 
                  "PCT_Nearly_Met", 
                  "PCT_Not_Met", 
                  "N_with_Scores", 
                  "PCT_Above_Area1", 
                  "PCT_At_Near_Area1", 
                  "PCT_Below_Area1", 
                  "PCT_Above_Area2", 
                  "PCT_At_Near_Area2", 
                  "PCT_Below_Area2",
                  "PCT_Above_Area3", 
                  "PCT_At_Near_Area3", 
                  "PCT_Below_Area3",
                  "PCT_Above_Area4", 
                  "PCT_At_Near_Area4", 
                  "PCT_Below_Area4")
    
    if (year_num %in% c("14", "15")){
      
      names(df) <- varnames
      
    } else if (year_num %in% c("16", "17")){
      
      names(df) <- varnames[!varnames == "N_CAASPP_Enrollment"]
      
    }
    
    
    
    ###'######################################################################
    ###'
    ###' CDS code 
    ###' 
    ###' 
    
    ### Check distributions
    tabdf(df, CountyCode)
    tabdf(df, DistrictCode)
    tabdf(df, SchoolCode)
    
    
    ### Convert character to numeric
    df <- df %>%
      mutate_at(.vars = c("CountyCode", "DistrictCode", "SchoolCode"), 
                .funs = as.numeric)
    
    classmode(df, everything())
    
    
    ### Delete Filler column
    df <- df %>% select(-Filler)
    
    
    
    
    ###'######################################################################
    ###'
    ###' Test related variables:
    ###' 
    ###' TestYear, TestType, TestID
    ###'
    ###'
    
    ### Check distributions
    tabdf(df, TestYear)
    tabdf(df, TestType)
    tabdf(df, TestID)
    
    
    ### Convert to numeric or factor
    df <- df %>%
      select(-TestType) %>% 
      mutate(TestYear = as.numeric(TestYear), 
             TestID = as.numeric(TestID), 
             TestID = factor(TestID, 
                             levels = c(1:4), 
                             labels = c("SBA_ELA", 
                                        "SBA_Math", 
                                        "CAA_ELA", 
                                        "CAA_Math")))
    
    
    
    ###'######################################################################
    ###'
    ###' Grade
    ###'
    ###'
    
    tabdf(df, Grade)
    
    df <- df %>%
      mutate(Grade = as.numeric(Grade)) %>%
      mutate(Grade = factor(Grade, 
                            levels = c(3:8, 11, 13), 
                            labels = c(paste0("GR", c(3:8, 11)), "GR_All")))
    
    
    
    ###'######################################################################
    ###'
    ###' Subgroup
    ###'
    ###'
    
    names(df_subgrp)
    
    tabdf(df, Subgroup)
    
    df <- df %>%
      mutate(Subgroup = as.numeric(Subgroup)) %>%
      left_join(df_subgrp, by = c("Subgroup" = "ID_subgrp"))
    
    
    
    ###'######################################################################
    ###'
    ###' Convert to numeric
    ###'
    ###'
    
    ### Variables to convert
    names(df)
    to_numeric <- c(names(df)[grepl("N_", names(df))], 
                    "Mean_Scale_Score", 
                    names(df)[grepl("PCT_", names(df))])
    
    to_numeric <- to_numeric[to_numeric %in% names(df)]
    
    
    ### Convert selected columns to numeric
    df[, to_numeric] <- df[, to_numeric] %>% 
      mutate_all(.funs = as.numeric)
    
    classmode(df, everything())
    
    
    
    ###'######################################################################
    ###'
    ###' Rearrange rows and columns
    ###'
    ###'
    
    df <- df %>%
      select(ends_with("Code"), 
             starts_with("Test"), 
             Grade, Subgroup, Subgrp1, Subgrp2, 
             starts_with("N_"), 
             Mean_Scale_Score, 
             starts_with("PCT_")) %>%
      arrange(CountyCode, DistrictCode, SchoolCode, 
              Grade, TestID, Subgroup)
    
    
    
    ###'######################################################################
    ###' 
    ###' Save data objects
    ###' 
    ###' 
    
    ### Set data containing working directory
    setwd(data_dir)
    
    
    ### Save the resulting dataframe
    save(df, file = paste0(filename, "_cleaned", ".rda"))
    write.dta(df, file = paste0(filename, "_cleaned", ".dta"))
    
    
    ###'######################################################################
    ###' 
    ###' Print the progress  
    ###' 
    
    cat(paste0("Year ", years[i], " completed", "\n"))

  }  # End of loop over filetype
} # End of loop over years
