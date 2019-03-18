
###'######################################################################
###'
###' Generate Variables
###' 
###' Course-related Properties: 
###' 
###' => Load (CoursesTaught + CourseEnrollment + StaffAssign + StaffDemoFTE)
###'  
###'    and generate regression predictors for CourseEnrollment
###'
###' (1) Teacher-related predictors
###' 
###' (2) Course-related predictors
###' 
###' 
###' 20181216 JoonHo Lee
###' 20181228 JoonHo Lee - Update with Master_above and Unified subject name and Category
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
library(foreign)
library(broom)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Loop over years
###'
###'

years <- sprintf("%02d", seq(12, 17))


for (i in seq_along(years)) {
  
  ###'######################################################################
  ###'
  ###' Assign year number
  ###' 
  ###' 
  
  year_num <- years[i]
  
  
  
  ###'######################################################################
  ###'
  ###' Import precleaned dataset
  ###'
  ###'
  
  setwd(data_dir)
  
  load(file = paste0("CoursesTaught", year_num, 
       "_merged with CourseEnrollment_StaffAssign_StaffDemoFTE.rda"))
  
  df <- df_to_save; rm(df_to_save)
  
  
  
  ###'######################################################################
  ###'
  ###' (1) The number of courses within class
  ###' 
  ###' ClassID and CourseCode
  ###' 
  ###' - Courses are nested within a class
  ###' - Get the summary statistics of the number of courses within a class
  ###' 
  ###' => there are about 2-3% of multi-course classes. 
  ###'    Need to consider this when generating variables 
  ###'
  ###'
  
  ### Summarize the count of courses within class
  df_temp <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, Assign_Subject, ClassID) %>%
    summarise(N_Courses = n_distinct(CourseCode)) 
  
  
  ### Number of courses by subject
  names(df_temp)
  
  subj_vec <- unique(df_temp$Assign_Subject)
  
  df_collect <- tibble()
  
  for (j in seq_along(subj_vec)) {
    
    # Extract only subset of data (by subject)
    temp <- df_temp %>%
      filter(Assign_Subject == subj_vec[j])
    
    # Calculate the frequencey and percentages
    tbl_subj <- tabdf(temp, N_Courses)
    
    # Add a subject name column
    tbl_subj <- tbl_subj %>%
      mutate(Subject = subj_vec[j]) %>%
      select(Subject, everything())
    
    # Append to the df_collect
    df_collect <- bind_rows(df_collect, tbl_subj)
    
  }
  
  ### Save the table as csv file format
  setwd(work_dir)
  write.csv(df_collect, 
            file = paste0("tables/Number of courses within class ", year_num, ".csv"))
  
  
  
  ###'######################################################################
  ###' 
  ###' (2) Teacher's Race/Ethnicity
  ###' 
  ###' - White, Latino, Black, Asian Teacher dummies
  ###' 
  ###' 
  
  df <- df %>%
    mutate(Teacher_White = if_else(EthnicGroup == "White", 1, 0, 
                                   missing = NA_real_), 
           Teacher_Latino = if_else(EthnicGroup == "Hispanic/Latino", 1, 0, 
                                    missing = NA_real_), 
           Teacher_Black = if_else(EthnicGroup == "Black", 1, 0, 
                                   missing = NA_real_), 
           Teacher_Asian = if_else(EthnicGroup == "Asian", 1, 0, 
                                   missing = NA_real_), 
           Teacher_Other = if_else(EthnicGroup %in% c("Filipino", 
                                                      "Pacific Islander", 
                                                      "American Indian/Alaska Native", 
                                                      "Two or more races"), 1, 0, 
                                   missing = NA_real_))
  
  tabdf(df, EthnicGroup)
  
  
  
  ###'######################################################################
  ###' 
  ###' (3) Teacher's Education Level: Master or above vs. Bachelor or below 
  ###'  
  ###' < Codebook >
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
  
  ### Generate MasDoc factor variable
  tabdf(df, EducationLevel)
  
  df$Master_vs_Bachelor <- NA
  df$Master_vs_Bachelor[df$EducationLevel %in%
                          c("A", "B", "C", "F", "Y", "U")] <- "Bachelor_Below"
  df$Master_vs_Bachelor[df$EducationLevel %in% 
                          c("M", "V", "S", "D")] <- "Master_Above"
  
  df$Master_vs_Bachelor <- factor(df$Master_vs_Bachelor)
  
  df <- df %>%
    mutate(Master_vs_Bachelor = relevel(Master_vs_Bachelor, "Bachelor_Below"))
  
  tabdf(df, Master_vs_Bachelor)
  
  
  ### Generate Master_above dummy indicator
  df <- df %>%
    mutate(Master_Above = if_else(Master_vs_Bachelor == "Master_Above", 1, 0, 
                                  missing = NA_real_))
  
  
  
  ###'######################################################################
  ###' 
  ###' (4) Novice Teachers & New in district teachers
  ###' 
  ###' 
  ###' New teachers (YearsTeaching == 1):
  ###' The percent of teachers with no prior teaching experience
  ###' 
  ###' New teachers in district (YearsInDistrict == 1)
  ###' 
  ###' Note: The first year of service is counted as 1 year.
  ###'
  ###'

  ### Generate factors (1) New Teaching (YearsTeaching <= 1)
  df <- df %>%
    mutate(NewTeaching = recode(YearsTeaching, 
                                "0" = "1", "1" = "1", 
                                .default = "0", .missing = NA_character_), 
           NewTeaching = factor(NewTeaching))
  
  listvars(df, YearsTeaching, NewTeaching, nrow = 300)
  tabdf(df, YearsTeaching)
  tabdf(df, NewTeaching)
  
  
  ### Generate factors (2) New in District (YearsInDistrict <= 1)
  df <- df %>%
    mutate(NewInDistrict = recode(YearsInDistrict, 
                                  "0" = "1", "1" = "1", 
                                  .default = "0", .missing = NA_character_), 
           NewInDistrict = factor(NewInDistrict))
  
  listvars(df, YearsInDistrict, NewInDistrict, nrow = 300)
  tabdf(df, YearsInDistrict)
  tabdf(df, NewInDistrict)
  
  
  
  ###'######################################################################
  ###'
  ###' (5) Tenured Teachers vs. Non-Tenured Teachers
  ###'
  ###' - EmploymentStatusCode
  ###' 
  ###'
  
  tabdf(df, EmploymentStatusCode)
  
  df <- df %>%
    mutate(Teacher_Tenured = if_else(EmploymentStatusCode == "Tenured", 1, 0, 
                                     missing = NA_real_))
  
  
  
  ###'######################################################################
  ###'
  ###' (6) Unified Subject Category
  ###'
  ###'
  
  setwd(data_dir)
  
  df_unified <- read.csv(file = "AssignmentCodes_Subject_Naming_StaffAssign1217_Edited.csv")
  
  names(df_unified)
  
  tabdf(df, .merge)
  
  
  ### Merge!
  df <- df %>%
    dplyr::select(-.merge) %>%
    full_join_track(df_unified, by = c("Assign_Subject"), .merge = TRUE)
  
  
  ### Check unmerged cases
  tabdf(df, .merge)
  
  temp_right <- df %>%
    filter(.merge == "right_only")
  
  temp_left <- df %>%
    filter(.merge == "left_only")
  
  
  ### Drop unmerged cases (right only)
  df <- df %>%
    filter(.merge != "right_only")
  
  
  
  
  ###'######################################################################
  ###'
  ###' Rearrange columns & save the resulting dataframe 
  ###'
  ###'
  
  df <- df %>%
    select(CountyCode:EthnicGroup, 
           Teacher_White, Teacher_Latino, Teacher_Black, Teacher_Asian, Teacher_Other, 
           EducationLevel, Master_vs_Bachelor, Master_Above, 
           YearsTeaching, YearsInDistrict, NewTeaching, NewInDistrict, 
           EmploymentStatusCode, Teacher_Tenured, 
           StaffType:Assign_Subject, Subject_Category, 
           everything())
  
  names(df)
  
  setwd(data_dir)
  dualsave(df, 
           paste0("CoursesTaught", year_num, 
                  "_merged with CourseEnrollment_StaffAssign_StaffDemoFTE"))
  

  
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



