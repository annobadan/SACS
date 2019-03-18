
###'######################################################################
###'
###' Generate Variables
###' 
###' Generate meaningful school-level FACTORS for plotting
###' 
###' 
###' => School-level Public K-12 Characteristics
###' 
###' 
###' 20181008 JoonHo Lee
###' 20181218 JoonHo Lee - Update with merged Public K-12 characters
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character")


### Call libraries
library(tidyverse)
library(foreign)
library(rlang)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import the merged dataset
###'
###'

### Public K-12 Characteristics Data
setwd(data_dir)
load(file = "df_Public_K12_Characters_2003_2017.rda")
df <- df_to_save


###'######################################################################
###'
###' Traditional (EdOps): Filter only for Traditional Schools
###' 
###' The final data set and analysis thus reflects "traditional schools"
###' in elementary, high school and unified school districts 
###' that have been in continous operation in California from 2003-2017
###' 
###' < Remove the following types of schools >
###' 
###' - Alternative School of Choice
###' - Community Day School
###' - Continuation School
###' - County Community School
###' - District Special Education Consortia School
###' - Home and Hospital
###' - Juvenile Court School
###' - Opportunity School
###' - Special Education School
###' - State Special School
###' - Youth Authority School
###' 
###' 

classmode(df, everything())


### Check distribution
tabdf(df, EdOps)


### Generate a filter only for Traditional schools
df <- df %>% 
  mutate(Traditional = if_else(EdOps == "Traditional", 1, 0, 
                               missing = NA_real_))



###'######################################################################
###'
###' Traditional_DOC: District Type
###'
###' The final data set and analysis thus reflects traditional schools
###' in "elementary, high school and unified school districts" 
###' that have been in continuous operation in California from 2003-2017
###' 
###' <Remove the following district type >
###' 
###' - County Office of Education (COE)
###' - State Board of Education
###' - Statewide Benefit Charter
###'
###'

### Check distribution
tabdf(df, DOC)
classmode(df, DOC)


### Filter for only Elementary, High, and Unified School Districts
df <- df %>%
  mutate(Traditional_DOC = as.character(DOC)) %>%
  mutate(Traditional_DOC = if_else(DOC == "Elementary School District" | 
                                     DOC == "High School District" |
                                     DOC == "Unified School District", DOC, NA_integer_))

tabdf(df, Traditional_DOC)



###'######################################################################
###'
###' Traditional_SOC: School Type
###' 
###' < Remove the following school types >
###' 
###' - Adult Education Centers
###' - Preschool
###'
###'

### Check distribution
tabdf(df, SOC)
classmode(df, SOC)


### Filter for only traditional school types
SOC_bins <- c("Elementary School", 
              "Intermediate/Middle/Junior High", 
              "High School", 
              "K-12 Schools")


df <- df %>%
  mutate(Traditional_SOC = recode_factor(SOC, 
         "Elemen Schools In 1 School Dist. (Public)" = "Elementary School", 
         "Elementary Schools (Public)" = "Elementary School", 
         "High Schools (Public)" = "High School", 
         "High Schools In 1 School Dist. (Public)" = "High School", 
         "Intermediate/Middle Schools (Public)" = "Intermediate/Middle/Junior High", 
         "Junior High Schools (Public)" = "Intermediate/Middle/Junior High", 
         "K-12 Schools (Public)" = "K-12 Schools")) %>%
  mutate(Traditional_SOC = if_else(Traditional_SOC %in% SOC_bins, 
                                   Traditional_SOC, NA_integer_))

tabdf(df, Traditional_SOC)



###'######################################################################
###'
###' Poverty Measure: Category variables (Quintile)
###' 
###' : 15-year mean vs. year-specific quintile?
###'   
###'   => First Go with 15-year mean among the following two options
###'   
###' 
###' (1) Quintiles based on 15-year mean 
###' 
###' - Bin1_PCT_Free
###' - Bin1_PCT_FRPM
###' - Bin1_PCT_White
###' - Bin1_PCT_Latino
###' - Bin1_PCT_Black
###' 
###' 
###' (2) Quintiles based on year-specific quintiles 
###' 
###' - Bin2_PCT_Free
###' - Bin2_PCT_FRPM
###' - Bin2_PCT_White
###' - Bin2_PCT_Latino
###' - Bin2_PCT_Black
###' 
###'

### Generate table for 15-year mean & quintiles

bin_vars <- c("PCT_Free", "PCT_FRPM", 
              "PCT_White", "PCT_Latino", "PCT_Black")

for (i in seq_along(bin_vars)){
  
  # Assign 
  df_temp <- df
  idx <- which(names(df_temp) == bin_vars[i])
  names(df_temp)[idx] <- "variable"
  
  # Calculate 15-year means: 2003-2017
  df_mean <- df_temp %>%
    group_by(CountyCode, DistrictCode, SchoolCode) %>% 
    summarise(mean_value = mean(variable, na.rm = TRUE)) %>%
    ungroup()
  
  # Calculate quintiles
  df_mean <- df_mean %>%
    mutate(bin = ntile(mean_value, 5), 
           bin = factor(bin, 
                        levels = c(1:5), 
                        labels = paste0("Quintile ", 1:5)))
  
  # Rename generated variables
  idx_mean <- which(names(df_mean) == "mean_value")
  idx_bin <- which(names(df_mean) == "bin")
  
  names(df_mean)[idx_mean] <- paste0(bin_vars[i], "_15mean")
  names(df_mean)[idx_bin] <- paste0(bin_vars[i], "_15bin")
  
  # Merge to the original dataset
  df <- df %>%
    left_join(df_mean, by = c("CountyCode", "DistrictCode", "SchoolCode"))
}

df_names <- data.frame(names(df))



###'######################################################################
###'
###' Save the resulting data
###'
###'

### Rearrange column order
df <- df %>%
  dplyr::select(CDSCode:Longitude, Traditional:PCT_Black_15bin, everything())


### Save the resulting file
setwd(data_dir)
dualsave(df, file = "df_Public_K12_Characters_2003_2017_with_filter")


