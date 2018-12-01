
###'######################################################################
###'
###' Generate Variables
###' 
###' School-level Student Compositions 
###' 
###' (9) Graduates - Extract only "PCT_UC_GRADS out of GRADS" 
###' 
###' - GRADS:   
###'   Number of twelfth-grade graduates. 
###'   This data includes summer graduates and 
###'   does not include students with high school equivalencies 
###'   (i.e., General Educational Development (GED) test or 
###'   California High School Proficiency Examination (CHSPE)).
###'   
###' - UC_GRADS:
###'   Number of twelfth-grade graduates who also completed all courses 
###'   required for entry into the University of California (UC) and/or 
###'   California State University (CSU) with a grade "C" or better. 
###'   This data includes summer graduates and 
###'   does not include students with high school equivalencies 
###'   (i.e., GED or CHSPE).
###' 
###' 
###' 20181108 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character/08_Graduate")


### Call libraries
library(tidyverse)
library(foreign)
library(rlang)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Load managed dropouts data
###' 
###' - Contains Enrollment by Grade data
###'
###'

### Set data containing working directory
data_dir_enr <- c("D:/Data/LCFF/Public_K-12_Character/07_Dropout")
setwd(data_dir_enr)


### Call the premanaged dataframe
load(file = "dropouts_managed_wide_1991_2016.rda")
df_enr <- df_to_save



###'######################################################################
###'
###' Loop over years
###'
###' 

setwd(data_dir)

years <- c(sprintf("%02d", seq(92, 99)), sprintf("%02d", seq(0, 16)))

CDS_vars <- c("CountyCode", "DistrictCode", "SchoolCode")

meta_list <- list()


for (i in seq_along(years)){
  
  
  ###'######################################################################
  ###'
  ###' Load dataset
  ###'
  ###'
  
  year_num <- years[i]
  
  year_acad <- if_else(as.numeric(year_num) > 90, 
                       1900 + as.numeric(year_num), 
                       2000 + as.numeric(year_num))
  
  load(file = paste0("grads", year_num, "_cleaned", ".rda"))
  
  
  
  ###'######################################################################
  ###'
  ###' Delete unnecessary variables
  ###'
  ###'
  
  names(df)
  
  df <- df %>%
    select(-YEAR, -CDS_CODE)
  
  
  
  ###'######################################################################
  ###'
  ###' Dataframe #1. School-level aggregate
  ###'
  ###'

  df_sch <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode) %>%
    summarise(N_GRADS = sum(GRADS, na.rm = TRUE), 
              N_UC_GRADS = sum(UC_GRADS, na.rm = TRUE)) %>%
    mutate(PCT_UC_GRADS = 100*(N_UC_GRADS/N_GRADS))
  
  
  
  ###'######################################################################
  ###'
  ###' Dataframe #2. Aggregated by Gender group
  ###'
  ###'
  
  df_gender <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, GENDER) %>%
    summarise(N_GRADS = sum(GRADS, na.rm = TRUE), 
              N_UC_GRADS = sum(UC_GRADS, na.rm = TRUE)) %>%
    mutate(PCT_UC_GRADS = 100*(N_UC_GRADS/N_GRADS))
  
  
  
  ###'######################################################################
  ###'
  ###' Dataframe #3. Aggregated by Ethnicity group
  ###'
  ###'
  
  df_race <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, ETHNIC) %>%
    summarise(N_GRADS = sum(GRADS, na.rm = TRUE), 
              N_UC_GRADS = sum(UC_GRADS, na.rm = TRUE)) %>%
    mutate(PCT_UC_GRADS = 100*(N_UC_GRADS/N_GRADS))
  
  
  
  ###'######################################################################
  ###'
  ###' Join the generated three dataframes
  ###'
  ###'
  
  temp_list <- list()
  
  ### Assign/Rename Category variables
  names(df_sch)
  temp_list[[1]] <- df_sch <- df_sch %>%
    mutate(Category = "school aggregate") %>%
    select(CDS_vars, Category, everything())
  
  names(df_gender)
  temp_list[[2]] <- df_gender <- df_gender %>%
    rename(Category = GENDER)
  
  names(df_race)
  temp_list[[3]] <- df_race <- df_race %>%
    rename(Category = ETHNIC)
  
  
  ### Bind rows!
  df_bind <- bind_rows(temp_list)
  classmode(df_bind, everything())
  
  
  ### Arrange rows and add AcademicYear
  df_bind <- df_bind %>%
    arrange(CountyCode, DistrictCode, SchoolCode) %>%
    mutate(AcademicYear = year_acad) %>%
    select(CountyCode, DistrictCode, SchoolCode, AcademicYear, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Embed into the list
  ###'
  ###'
  
  meta_list[[i]] <- df_bind
  
  
  
  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("Year ", year_num, " completed", "\n"))
  
}



###'######################################################################
###'
###' Append to longitudinal dataset
###'
###'

### Bind rows!
df_bind <- bind_rows(meta_list)
classmode(df_bind, everything())


### Define a factor for Category
tabdf(df_bind, Category)
Category_levels <- c("school aggregate", "male", "female", 
                     "White", "Hispanic or Latino", "African American", "Asian",
                     "Filipino", "American Indian or Alaska Native", "Pacific Islander", 
                     "Two or More Races", "Not reported", "Multiple or No Response")

df_bind <- df_bind %>%
  mutate(Category = factor(Category, levels = Category_levels))


### Arrange rows and columns
df_bind <- df_bind %>%
  select(CountyCode, DistrictCode, SchoolCode, Category, AcademicYear, 
         everything()) %>% 
  arrange(CountyCode, DistrictCode, SchoolCode, Category, AcademicYear)


### Save the resulting dataframe
setwd(data_dir)
dualsave(df_bind, paste0("graduates_managed_PCT_UC_GRADS", "_1992_2016" ))

