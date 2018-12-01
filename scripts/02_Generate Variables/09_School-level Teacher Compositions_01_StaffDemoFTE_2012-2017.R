
###'######################################################################
###'
###' Generate Variables
###' 
###' School-level Teacher Compositions 2012-2017
###' 
###' - with "Staff_Demo_FTE" data 
###' 
###' 
###' 20181002 JoonHo Lee
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
library(foreign)
library(rlang)


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
  ###' Import cleaned data files
  ###'
  ###'
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Staff School FTE
  load(file = paste0("Staff_Demo_FTE", year_num, "_merged", ".rda"))
  df <- df_demo_fte; rm(df_demo_fte)
  classmode(df, everything())
  
  
  ### Prepare dataset filtering only "Teacher"
  tabdf(df, StaffType)
  
  df_Teacher <- df %>%
    filter(StaffType == "Teacher")
  
  tabdf(df_Teacher, StaffType)
  
  
  
  ###'######################################################################
  ###' 
  ###' Data frame #1. "df_StaffType" 
  ###' Total Count/Percent of staffs (RecID) within school
  ###' 
  ###' 
  ###' (1) Total (unduplicated) Count of Staffs within school
  ###' 
  ###' (2) Total Count/Percent of "Teachers" within school
  ###' 
  ###' (3) Total Count/Percent of "Administrators" within school
  ###' 
  ###' (4) Total Count/Percent of "Pupil Services" within school
  ###' 
  ###' 
  
  ### Check distribution
  tabdf(df, StaffType)
  
  
  ### Generate school composition table
  levels(df$StaffType)
  levels_to_replace <- c("Teacher", "Admin", "Pupil_Serv")
  
  df_StaffType <- school_composition(df, 
                                     var_to_count = RecID, 
                                     factor = StaffType, 
                                     levels_to_replace = levels_to_replace, 
                                     table_name = "Staff Type", 
                                     year = year_num)
  
  
  
  ###'######################################################################
  ###' 
  ###' Data frame #2. "df_JobClassification" 
  ###' Total Count/Percent of staffs (RecID) within school
  ###' 
  ###' - Teacher
  ###' - Administrator
  ###' - Pupil services
  ###' - Charter Non-certificated Teacher
  ###' - Non-certificated Administrator
  ###' - Itinerant or Pull-out/Push-in Teacher
  ###'       
  ###' 
  
  ### Check distribution
  tabdf(df, JobClassification)
  
  
  ### Generate school composition table
  levels(df$JobClassification)
  levels_to_replace <- c("Teacher", "Admin", "Pupil_Serv", 
                         "Charter_Teacher_NonCert", "Admin_NonCert", 
                         "Itinerant")
  
  df_JobClassification <- school_composition(df, 
                                             var_to_count = RecID, 
                                             factor = JobClassification,
                                             levels_to_replace = levels_to_replace,
                                             table_name = "Job Classification", 
                                             year = year_num)
  
  
  
  ###'######################################################################
  ###' 
  ###' Data frame #3. "df_Teacher_Gender" 
  ###' 
  ###' Teacher Gender composition
  ###' 
  ###'   
  
  ### Check distribution
  tabdf(df_Teacher, GenderCode)
  
  
  ### Generate school composition table
  levels(df_Teacher$GenderCode)
  levels_to_replace <- c("female", "male")  
  
  df_Teacher_Gender <- school_composition(df_Teacher, 
                                          var_to_count = RecID, 
                                          factor = GenderCode,
                                          levels_to_replace = levels_to_replace, 
                                          table_name = "Teacher Gender", 
                                          year = year_num)
  
  
  
  ###'######################################################################
  ###' 
  ###' Data frame #4. "df_Teacher_Race" 
  ###' 
  ###' Teacher Race/Ethnicity composition
  ###' 
  ###'   
  
  ### Check distribution
  tabdf(df_Teacher, EthnicGroup)
  
  
  ### Generate school composition table
  levels(df_Teacher$EthnicGroup) 
  levels_to_replace <- c("White", "Latino", "Black", "Asian", 
                         "Filipino", "Pacific", "Native", "Two_more")
  
  
  df_Teacher_Race <- school_composition(df_Teacher, 
                                        var_to_count = RecID, 
                                        factor = EthnicGroup, 
                                        levels_to_replace = levels_to_replace,
                                        table_name = "Teacher Race/Ethnicity", 
                                        year = year_num)
  
  
  
  ###'######################################################################
  ###' 
  ###' Data frame #5. "df_Teacher_Education" 
  ###' 
  ###' Teacher composition of Educational Level
  ###' 
  ###' Dummy for Master / Doctorate
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
  
  ### Check distribution
  tabdf(df_Teacher, EducationLevel)
  
  
  ###' Generate factor variable
  ###' The percent of certified teachers with no more than a Bachelor's degree
  df_Teacher$Bachelor_and_Below <- NA
  df_Teacher$Bachelor_and_Below[df_Teacher$EducationLevel %in% 
                                  c("A", "B", "C", "F", "Y", "U")] <- "No more than Bachelor's"
  df_Teacher$Bachelor_and_Below[df_Teacher$EducationLevel %in% 
                                  c("M", "V", "S", "D")] <- "Master's and above"
  
  df_Teacher$Bachelor_and_Below <- factor(df_Teacher$Bachelor_and_Below)
  
  tabdf(df_Teacher, Bachelor_and_Below)
  
  
  ### Generate school composition table
  levels(df_Teacher$Bachelor_and_Below)
  levels_to_replace <- c("Master_Plus", "Bachelor_Minus")
  
  df_Teacher_Education <- school_composition(df_Teacher, 
                                             var_to_count = RecID, 
                                             factor = Bachelor_and_Below, 
                                             levels_to_replace = levels_to_replace,
                                             table_name = "Teacher Education-level", 
                                             year = year_num)
  
  
  
  ###'######################################################################
  ###' 
  ###' Data frame #6. "df_Teacher_Years" 
  ###' 
  ###' 
  ###' Within school summary of 
  ###' (1) Years of Teaching
  ###' (2) Years in District
  ###' 
  ###' 
  
  ### Check distributions
  tabdf(df_Teacher, YearsTeaching)
  tabdf_plot(df_Teacher, YearsTeaching, limits = c(0, 60))
  
  tabdf(df_Teacher, YearsInDistrict)
  tabdf_plot(df_Teacher, YearsInDistrict, limits = c(0, 50))
  
  
  ### Drop duplicates based on RecID
  df_Teacher_Unduplicated <- df_Teacher %>%
    distinct(CountyCode, DistrictCode, SchoolCode, 
             CountyName, DistrictName, SchoolName, RecID, 
             .keep_all = TRUE)
  
  
  ### Generate school-level summary statistics
  df_Teacher_YearsTeaching <- school_summarize(df_Teacher_Unduplicated, 
                                               YearsTeaching, 
                                               "Years Teaching", 
                                               year = year_num)
  
  df_Teacher_YearsInDistrict <- school_summarize(df_Teacher_Unduplicated, 
                                                 YearsInDistrict, 
                                                 "Years in District", 
                                                 year = year_num)
  
  
  ### Bind rows
  df_Teacher_Years <- bind_rows(df_Teacher_YearsTeaching, 
                                df_Teacher_YearsInDistrict) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, table)
  
  

  
  
  ###'######################################################################
  ###' 
  ###' Data frame #7. "df_Teacher_New" 
  ###' 
  ###' 
  ###' Count/Percent of New teachers (YearsTeaching == 1):
  ###'   The percent of teachers with no prior teaching experience
  ###' 
  ###' Count/Percent of New teachers in district (YearsInDistrict == 1)
  ###' 
  ###' Note: The first year of service is counted as 1 year.
  ###'
  ###'
  
  ### Generate factors (1) New Teaching (YearsTeaching <= 1)
  df_Teacher <- df_Teacher %>%
    mutate(NewTeaching = recode(YearsTeaching, 
                                "0" = "1", "1" = "1", 
                                .default = "0", .missing = NA_character_), 
           NewTeaching = factor(NewTeaching))
  
  listvars(df_Teacher, YearsTeaching, NewTeaching, nrow = 300)
  tabdf(df_Teacher, YearsTeaching)
  tabdf(df_Teacher, NewTeaching)
  
  
  ### Generate factors (2) New in District (YearsInDistrict <= 1)
  df_Teacher <- df_Teacher %>%
    mutate(NewInDistrict = recode(YearsInDistrict, 
                                  "0" = "1", "1" = "1", 
                                  .default = "0", .missing = NA_character_), 
           NewInDistrict = factor(NewInDistrict))
  
  listvars(df_Teacher, YearsInDistrict, NewInDistrict, nrow = 300)
  tabdf(df_Teacher, YearsInDistrict)
  tabdf(df_Teacher, NewInDistrict)
  
  
  ### Generate school composition table
  levels(df_Teacher$NewTeaching)
  levels(df_Teacher$NewTeaching)
  levels_to_replace <- c("Not_New", "New")
  
  df_Teacher_NewTeaching <- school_composition(df_Teacher, 
                                               var_to_count = RecID, 
                                               factor = NewTeaching, 
                                               levels_to_replace = levels_to_replace,
                                               table_name = "New Teaching", 
                                               year = year_num)
  
  df_Teacher_NewInDistrict <- school_composition(df_Teacher, 
                                                 var_to_count = RecID, 
                                                 factor = NewInDistrict, 
                                                 levels_to_replace = levels_to_replace,
                                                 table_name = "New in District", 
                                                 year = year_num)
  
  df_Teacher_New <- bind_rows(df_Teacher_NewTeaching, 
                              df_Teacher_NewInDistrict) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, table)
  
  
  
  ###'######################################################################
  ###'
  ###' Data frame #8. "df_Teacher_EmployStatus" 
  ###'
  ###' Employment Status
  ###'
  ###'
  
  ### Check distribution
  tabdf(df_Teacher, EmploymentStatusCode)
  
  
  ### Generate school composition table
  levels(df_Teacher$EmploymentStatusCode)
  levels_to_replace <- c("Longterm_Sub", "Probationary", 
                         "Tenured", "Other")
  
  df_Teacher_EmployStatus <- school_composition(df_Teacher, 
                                                var_to_count = RecID, 
                                                factor = EmploymentStatusCode,
                                                levels_to_replace = levels_to_replace,
                                                table_name = "Employment Status", 
                                                year = year_num)
  
  
  
  ###'######################################################################
  ###'
  ###' Data frame #9. "df_FTE" 
  ###' 
  ###' FTE: School-level summary for "All Staffs"
  ###' FTE: School-level summary for only "Teachers"
  ###' 
  ###' Indicator for: 
  ###' 
  ###' "How many staffs are employed as part-time?"  <- this one
  ###' "How many teachers are employed as part-time?"  <- this one
  ###'
  ###'
  
  ### Get RecID level summation
  df_FTE_All_Staffs <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, 
             CountyName, DistrictName, SchoolName, 
             RecID) %>%
    summarise(FTE_sum = sum(FTE, na.rm = TRUE))
  
  df_FTE_Teacher_Only <- df_Teacher %>%
    group_by(CountyCode, DistrictCode, SchoolCode, 
             CountyName, DistrictName, SchoolName, 
             RecID) %>%
    summarise(FTE_sum = sum(FTE, na.rm = TRUE))
  
  
  ### Generate school-level summary statistics
  df_FTE_Staff <- school_summarize(df_FTE_All_Staffs, 
                                   FTE_sum, 
                                   "FTE_All Staffs", 
                                   year = year_num)
  
  df_FTE_Teacher <- school_summarize(df_FTE_Teacher_Only, 
                                     FTE_sum, 
                                     "FTE_Teachers", 
                                     year = year_num)
  
  df_FTE <- bind_rows(df_FTE_Staff, 
                      df_FTE_Teacher) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, table)

  
  
  ###'######################################################################
  ###' 
  ###' Save data objects
  ###' 
  ###' 
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Collect and save all the resulting dataframes as list
  list_collect_df <- list(df_StaffType,
                          df_JobClassification,
                          df_Teacher_Gender,
                          df_Teacher_Race,
                          df_Teacher_Education,
                          df_Teacher_Years,
                          df_Teacher_New,
                          df_Teacher_EmployStatus,
                          df_FTE)
  
  names(list_collect_df) <- c("df_StaffType", 
                              "df_JobClassification",
                              "df_Teacher_Gender",
                              "df_Teacher_Race",
                              "df_Teacher_Education",
                              "df_Teacher_Years",
                              "df_Teacher_New",
                              "df_Teacher_EmployStatus",
                              "df_FTE")
  
  save(list_collect_df, 
       file = paste0("list_school-level_teacher_composition_", year_num, ".rda"))
  
  
  ### Save individual dataframes within the list
  for (i in seq_along(names(list_collect_df))){
    
    ### Extract the name of data frame => add fiscal year
    filename <- paste0(names(list_collect_df)[i], year_num)
    
    ### Extract dataframe
    df_save <- list_collect_df[[i]]
    
    ### Save the resulting dataframe
    save(df_save, file = paste0(filename, ".rda"))
    write.dta(df_save, file = paste0(filename, ".dta"))
    
  }

    

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
