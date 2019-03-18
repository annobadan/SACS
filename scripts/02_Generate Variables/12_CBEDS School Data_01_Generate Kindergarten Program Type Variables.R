
###'######################################################################
###'
###' Generate Variables 
###' 
###' - CBEDS Data about Schools & Districts:
###' 
###' (1) Kindergarten Program Type
###' 
###'     for 2015-16, 2016-17, 2017-18
###' 
###' 
###' 20190210 JoonHo Lee
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

years <- sprintf("%02d",seq(15, 17))

meta_list <- list()


for (i in seq_along(years)) {
  
  ###'######################################################################
  ###'
  ###' Assign year number
  ###' 
  ###' 
  
  year_num <- years[i]
  
  
  
  ###'######################################################################
  ###'
  ###' Load the cleaned dataset
  ###'
  ###'
  
  setwd(data_dir)
  
  load(paste0("CBEDS_School_Data_", year_num, ".rda"))
  
  df <- df_to_save; rm(df_to_save)
  
  
  
  ###'######################################################################
  ###'
  ###' Filter only "Kindergarten Program Type" Section
  ###'
  ###'
  
  tabdf(df, Section)
  
  df_kinder <- df %>%
    filter(Section == "Kindergarten Program Type")
  
  
  
  ###'######################################################################
  ###'
  ###' Tidy-up values
  ###'
  ###'
  
  ### Check a rownumber for each category
  classmode(df_kinder, everything())
  
  tabdf(df_kinder, Value)  # All true. Useless column.

  tbl_Description <- df_kinder %>%
    group_by(Description) %>%
    summarise(Numbers = paste(RowNumber, collapse = ","), 
              Times = length(Numbers))
  
  
  ### Covert RowNumber to numeric
  tabdf(df_kinder, RowNumber)
  
  df_kinder <- df_kinder %>%
    mutate(RowNumber = as.numeric(RowNumber))
  
  classmode(df_kinder, RowNumber)
  
  
  ### Generate two variables: Kindergarten_Type, Program_Type
  df_kinder <- df_kinder %>%
    mutate(
      Kindergarten_Type = case_when(
        RowNumber %in% c(1, 2, 3, 4) ~ "Kindergarten", 
        RowNumber %in% c(5, 6, 7, 8) ~ "Transitional Kindergarten", 
        TRUE ~ NA_character_), 
      Program_Type = case_when(
        RowNumber %in% c(1, 5) ~ "Full-day", 
        RowNumber %in% c(2, 6) ~ "Part-day", 
        RowNumber %in% c(3, 7) ~ "Full-and-Part-day", 
        RowNumber %in% c(4, 8) ~ "None", 
        TRUE ~ NA_character_)
  )
  
  
  ### Remove unnecessary variables
  classmode(df_kinder, everything())
  
  df_kinder <- df_kinder %>%
    select(-CDS_CODE, -Level, -Section, -Description, -Value)
  
  
  
  ###'######################################################################
  ###'
  ###' Convert to wide format
  ###'
  ###'
  
  df_wide <- df_kinder %>%
    select(-RowNumber) %>%
    spread(key = Kindergarten_Type, value = Program_Type) 
  
  names(df_wide)[names(df_wide) == "Transitional Kindergarten"] <- "Transitional_K" 
  

  
  ###'######################################################################
  ###' 
  ###' Save data objects
  ###' 
  ###' 
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Save the resulting dataset
  dualsave(df_wide, paste0("CBEDS_School_Data_", 
                           "01_Kindergarten_Program_Type_",  
                           year_num))
  
  
  ###'######################################################################
  ###' 
  ###' Collect within a list
  ###' 
  ###' 
  
  meta_list[[i]] <- df_wide
  
  
  
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



###'######################################################################
###'
###' Append the list into a dataframe
###'
###'

df_bind <- bind_rows(meta_list) %>%
  mutate(Year = as.numeric(substr(AcademicYear, 1, 2)) + 2000) %>%
  arrange(CountyCode, DistrictCode, SchoolName, Year)

names(df_bind)

df_bind <- df_bind %>%
  select(CountyCode, CountyName, 
         DistrictCode, DistrictName, 
         SchoolCode, SchoolName, 
         AcademicYear, Year, 
         Kindergarten, Transitional_K)


### Save the resulting dataset
setwd(data_dir)
dualsave(df_bind, paste0("CBEDS_School_Data_", 
                         "01_Kindergarten_Program_Type_",  
                         "3-year-long"))

