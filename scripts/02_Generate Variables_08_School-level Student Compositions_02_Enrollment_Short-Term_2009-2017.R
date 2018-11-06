
###'######################################################################
###'
###' Generate Variables
###' 
###' School-level Student Compositions 
###' 
###' - with Enrollment Data: Primary & Short-Term Enrollment (2009-2017)
###' 
###' - Generate the % of short-term enrollment => Proxy for student mobility 
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character/01_Enrollment_Primary_and_Short_Term")


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

years <- c(sprintf("%02d",seq(09, 17)))


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
  
  
  ### Import cleaned enrollment data
  load(file = paste0("enrps", year_num, "_cleaned", ".rda"))
  classmode(df, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Dataframe. Enrollment Type
  ###' 
  ###' by Primary vs. Combined
  ###' 
  ###' P = Primary Enrollment
  ###' 
  ###' The student’s name appears on a register, roll, or list, 
  ###' the student is currently attending (or intends to attend) 
  ###' the educational service institution (ESI), 
  ###' or is responsible for the students instruction 
  ###' (students attending NPS schools).
  ###'     
  ###' C = Combined Enrollment
  ###' 
  ###' The combined enrollment of primary and short-term students. 
  ###' Short-term enrollment is defined as 
  ###' when the student’s name appears on a register, roll, or list, 
  ###' the student is currently attending the educational service institution, 
  ###' and receives or will receive the majority of their instruction 
  ###' at the institution for less than 30 days.
  ###'
  ###'
  
  tabdf(df, ENR_TYPE)
  classmode(df, ENR_TYPE)
  levels(df$ENR_TYPE)
  levels_to_replace <- c("primary", "combined")
  
  df_ENR_TYPE <- school_composition_sum(df, 
                                        var_to_sum = ENR_TOTAL, 
                                        factor = ENR_TYPE, 
                                        levels_to_replace = levels_to_replace, 
                                        table_name = "Type of Enrollment", 
                                        year = year_num)
  
  ### Calculate short-term enrollment
  names(df_ENR_TYPE)
  df_ENR_TYPE <- df_ENR_TYPE %>%
    select(-PCT_primary, -PCT_combined) %>%
    mutate(N_shortterm = N_combined - N_primary, 
           PCT_shortterm = (N_shortterm/N_combined)*100)
  
  tabdf(df_ENR_TYPE, N_shortterm)
  tabdf(df_ENR_TYPE, PCT_shortterm)
  
  
  ###'######################################################################
  ###' 
  ###' Save data objects
  ###' 
  ###' 
  
  ### Set data containing working directory
  setwd(data_dir)
  
  
  ### Save the resulting dataframe
  save(df_ENR_TYPE, file = paste0("enr_short_term", year_num, ".rda"))
  write.dta(df_ENR_TYPE, file = paste0("enr_short_term", year_num, ".dta"))
    
  
  
  
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
###' Generate Longitudinal Dataset
###' 
###' Short-term enrollment 2009-2017
###'
###'

### Set data containing working directory
setwd(data_dir)


### Prepare empty list to collect data frames
meta_list <- list()


### Import the cleaned dataframes and embed into list
years <- c(sprintf("%02d",seq(9, 17)))

for (i in seq_along(years)){
  
  ### Assign year
  year_num <- years[i]
  
  ### Load the cleaned datafile
  load(file = paste0("enr_short_term", year_num, ".rda"))
  
  ### Embed in the list
  meta_list[[i]] <- df_ENR_TYPE
  
}


### Bind rows!
df_bind <- bind_rows(meta_list)


### Arrange by school
df_bind <- df_bind %>%
  mutate(AcademicYear = AcademicYear + 2000) %>%
  arrange(CountyCode, DistrictCode, SchoolCode, 
          table, AcademicYear)


### Save the resulting dataframe
setwd(data_dir)
save(df_bind, file = paste0("enr_short_term", "_0917", ".rda"))
write.dta(df_bind, file = paste0("enr_short_term", "_0917", ".dta"))




