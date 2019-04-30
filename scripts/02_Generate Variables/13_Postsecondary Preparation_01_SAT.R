
###'######################################################################
###'
###' Task    : Postsecondary Preparation 01. SAT
###'           
###' Category: Generate Variables 
###' 
###' Data    : Postsecondary Preparation SAT data 2001-2002 ~ 2017-2018
###' 
###' Date    : 2019-04-26
###' 
###' Author  : JoonHo Lee (joonho@berkeley.edu)
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
data_dir <- c("D:/Data/LCFF/School_Performance/Postsecondary_Preparation/SAT")


### Call libraries
library(tidyverse)
library(readxl)
library(Hmisc)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Append only School-level data
###'
###'

### Set an empty list to collect data frames
meta_list <- list()


### Loop over years: 9899-1718 
years <- c(sprintf("%02d", seq(98, 99)), sprintf("%02d", seq(0, 17))) 

for (i in seq_along(years)) {
  
  
  ###'######################################################################
  ###'
  ###' Assign year number
  ###'
  ###'
  
  year_num <- years[i]
  
  
  
  ###'######################################################################
  ###'
  ###' Import the cleaned .rda file
  ###'
  ###'
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Assign filenames
  if (year_num != c("17")){
    
    filename <- paste0("sat", years[i], years[i + 1]) 
    
  } else if (year_num == c("17")){
    
    filename <- paste0("sat", year_num, as.numeric(year_num) + 1)
    
  }
  
  
  ### Import .rda file
  load(file = paste0(filename, "_cleaned.rda"))
  classmode(df, everything())
  
  
  ###'######################################################################
  ###'
  ###' Add a Year variable
  ###'
  ###'
  
  df <- df %>%
    mutate(Year = ifelse(as.numeric(year_num) < 20, 
                         as.numeric(year_num) + 2000, 
                         as.numeric(year_num) + 1900)) %>%
    dplyr::select(Year, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Recode missing values in N_TestTakers & PCT_TestTakers with zero
  ###'
  ###'
  
  ### Check the distribution
  nrow(df)
  tabdf(df, N_TestTakers)
  
  
  ### Index missing values and convert them to zero
  idx <- is.na(df$N_TestTakers)
  df$N_TestTakers[idx] <- 0
  
  
  
  ###'######################################################################
  ###'
  ###' Directly calculate the PCT_TestTakers
  ###'
  ###'
  
  df <- df %>%
    mutate(PCT_TestTakers = 100*(N_TestTakers/N_Enroll_GR12))
  
  
  
  ###'######################################################################
  ###'
  ###' Re-Order variables
  ###'
  ###'
  
  df <- df %>%
    dplyr::select(Year,  
                  contains("Code"), 
                  contains("Name"), 
                  N_Enroll_GR12, N_TestTakers, PCT_TestTakers, 
                  everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Save as .rda & .dta file format
  ###'
  ###'
  
  setwd(data_dir)
  
  dualsave(df, paste0(filename, "_managed"))
  
  
  
  ###'######################################################################
  ###'
  ###' Embed into the list
  ###'
  ###'
  
  meta_list[[i]] <- df
  
  
  
  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("Year ", years[i], " completed", "\n"))
  
  
}  # End of loop over years



###'######################################################################
###'
###' Append!
###'
###'

### Check variable names over years
lapply(meta_list, names)


### Append!
df_bind <- bind_rows(meta_list) %>%
  arrange(CountyCode, DistrictCode, SchoolCode, Year) %>%
  dplyr::select(ReportType, 
                contains("Code"), 
                contains("Name"), 
                Year, 
                everything())


### Save the resulting dataframe
setwd(data_dir)
dualsave(df_bind, "SAT_Longitudinal_98-17")

