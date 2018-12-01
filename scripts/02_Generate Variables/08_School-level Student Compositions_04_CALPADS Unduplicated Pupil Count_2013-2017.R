
###'######################################################################
###'
###' Generate Variables
###' 
###' School-level Student Compositions 
###' 
###' (4) CALPADS Unduplicated Pupil Count
###' 
###' 
###' 20181104 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character/03_CALPADS_Unduplicated_Pupil_Count")


### Call libraries
library(tidyverse)
library(foreign)
library(rlang)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Collect dataframes as list
###'
###'

setwd(data_dir)

years <- sprintf("%02d", seq(13, 17))

temp_list <- list()


for (i in seq_along(years)){
  
  # Assign year number
  year_num <- paste0(years[i], as.numeric(years[i]) + 1)
  
  # Load dataframe
  load(file = paste0("cupc", year_num, "_cleaned", ".rda"))
  
  # Embed in the prepared list
  temp_list[[i]] <- df
  
}



###'######################################################################
###'
###' Choose variable list 
###' 
###' The Unduplicated Pupil Count (UPC) of 
###' 
###' (1) free or reduced price meal (FRPM) eligibility, 
###' (2) English learner (EL), and 
###' (3) foster youth data 
###' 
###' from the California Longitudinal Pupil Achievement Data System (CALPADS).
###' 
###' 

### Check out variable lists
lapply(temp_list, names)
lapply(temp_list, function(x) length(names(x)))
 

### Select only necessary & consistent variables from each year
select_vars <- function(df){
  
  # Select only necessary variables
  vars <- c("AcademicYear", 
            "CountyCode", "DistrictCode", "SchoolCode", 
            "Enrollment_K12", 
            "N_FRPM", "N_FRPM_Undup", "N_FRPM_Undup_unadj", 
            "N_EL", "N_Foster", 
            "N_Homeless", "N_Migrant", 
            "N_CALPADS_UPC")
  
  df <- df[, names(df) %in% vars]
  
  # Convert variable name for 2013-14 file
  names(df) <- gsub("_unadj", "", names(df))
  
  return(df)
  
}

temp_list_subset <- lapply(temp_list, select_vars)
lapply(temp_list_subset, names)
lapply(temp_list_subset, function(x) length(names(x)))



###'######################################################################
###'
###' Collapse list into dataframe
###'
###'

### Bind rows!
df_bind <- bind_rows(temp_list_subset)


### Reorder rows and columns
df_bind <- df_bind %>%
  mutate(AcademicYear = AcademicYear + 2000) %>%
  select(ends_with("Code"), AcademicYear, 
         Enrollment_K12, 
         N_FRPM, N_FRPM_Undup, N_EL, N_Foster, 
         N_Homeless, N_Migrant, N_CALPADS_UPC) %>%
  arrange(CountyCode, DistrictCode, SchoolCode, AcademicYear)



###'######################################################################
###'
###' Generate Percent variables
###'
###'

### Copy multiple columns
df_PCT <- df_bind %>%
  select(names(df_bind)[grepl("N_", names(df_bind))])


### Rename variables
names(df_PCT) <- gsub("N_", "PCT_", names(df_PCT))


### Calculate percentages
df_PCT <- df_PCT %>%
  mutate_at(.vars = names(df_PCT)[grepl("PCT_", names(df_PCT))], 
            .funs = function(x) 100*(x/df_bind$Enrollment_K12))


### Column bind with the original dataframe
df_bind <- cbind.data.frame(df_bind, df_PCT)



###'######################################################################
###'
###' Rename FRPM and EL to FRPM_CALPADS and EL_CALPADS
###' 
###' To facillitate merge with other dataframes
###'
###'

names(df_bind) <- gsub("FRPM", "FRPM_CALPADS", names(df_bind))

names(df_bind) <- gsub("EL", "EL_CALPADS", names(df_bind))

names(df_bind)



###'######################################################################
###'
###' Save the resulting longitudinal dataset
###'
###'

setwd(data_dir)

dualsave(df_bind, "CALPADS_UPC_2013_2017")




