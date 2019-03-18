
###'######################################################################
###'
###' Generate Variables
###' 
###' School-level Student Compositions 
###' 
###' (8) Dropout 
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character/07_Dropout")


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

setwd(data_dir)

years <- c(sprintf("%02d", seq(91, 99)), sprintf("%02d", seq(0, 16)))

meta_list <- list()


for (i in seq_along(years)){
  
  
  ###'######################################################################
  ###'
  ###' Load dataset
  ###'
  ###'
  
  year_num <- years[i]
  
  load(file = paste0("dropouts", year_num, "_cleaned", ".rda"))
  
  
  
  ###'######################################################################
  ###'
  ###' Delete unnecessary variables
  ###'
  ###'

  classmode(df, everything())
  
  df <- df %>% select(-CDS_CODE, -YEAR)
  
  
  
  ###'######################################################################
  ###'
  ###' Variable order
  ###'
  ###'
  
  ### Define common grade labels
  grade_labels <- c(7:12, "US", "TOT")
  
  
  ### Variable/factor order
  CDS_vars <- c("CountyCode", "DistrictCode", "SchoolCode")
  E_vars <- paste0("E", grade_labels)
  D_vars <- paste0("D", grade_labels)
  vars <- c(E_vars, D_vars)
  
  
  
  ###'######################################################################
  ###'
  ###' Dataframe #1. School-level dropout rates by Grade
  ###' 
  ###' - Aggregate to School-Level and Calculate Dropout rates by Grade
  ###' - Collapse Ethnicity groups and Gender group
  ###'
  ###'
  
  ###' Aggregate to school-level
  ###' Collapse Ethinicity and Gender groups
  df_sch <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode) %>%
    summarise_at(.vars = vars, .funs = function(x) sum(x, na.rm = TRUE))
  
  
  ### Generate long data for enrollment
  df_sch_E_long <- df_sch %>%
    select(CDS_vars, E_vars) %>%
    gather(key = Grade, value = E, E_vars) %>% 
    arrange(CountyCode, DistrictCode, SchoolCode, Grade) 
  
  
  df_temp_E <- df_sch_E_long %>%
    filter(Grade != "TOT") %>%
    group_by(CountyCode, DistrictCode, SchoolCode) %>%
    summarise(Grade = "ETotal", 
              E = sum(E, na.rm = TRUE))
  
  
  names(df_sch_E_long); names(df_temp_E)
  df_sch_E_long <- bind_rows(df_sch_E_long, df_temp_E) %>%
    mutate(Grade = factor(Grade, 
                          levels = c(E_vars, "ETotal"), 
                          labels = c(grade_labels, "Total"))) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, Grade)
  
  classmode(df_sch_E_long, everything())
  
  
  ### Generate long data for dropout
  df_sch_D_long <- df_sch %>%
    select(CDS_vars, D_vars) %>%
    gather(key = Grade, value = D, D_vars) %>% 
    arrange(CountyCode, DistrictCode, SchoolCode, Grade) 
  
  
  df_temp_D <- df_sch_D_long %>%
    filter(Grade != "TOT") %>%
    group_by(CountyCode, DistrictCode, SchoolCode) %>%
    summarise(Grade = "DTotal", 
              D = sum(D, na.rm = TRUE))
  
  
  names(df_sch_D_long); names(df_temp_D)
  df_sch_D_long <- bind_rows(df_sch_D_long, df_temp_D) %>%
    mutate(Grade = factor(Grade, 
                          levels = c(D_vars, "DTotal"), 
                          labels = c(grade_labels, "Total"))) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, Grade)
  
  classmode(df_sch_D_long, everything())
  
  
  ### Merge two long datasets
  df_sch_long <- df_sch_E_long %>%
    left_join(df_sch_D_long, by = c(CDS_vars, "Grade"))
  
  
  ### Calculate dropout rates
  df_sch_long <- df_sch_long %>%
    mutate(PCT_Dropout = 100*(D/E))
  
  
  ### Convert to wide format
  var_levels <- c(paste0("E_", grade_labels), "E_Total",  
                  paste0("D_", grade_labels), "D_Total", 
                  paste0("PCT_Dropout_", grade_labels), "PCT_Dropout_Total")
  
  df_sch_wide <- df_sch_long %>%
    gather(key = quantity, value = value, E, D, PCT_Dropout) %>%
    unite(col = key, quantity, Grade, sep = "_") %>% 
    mutate(key = factor(key, levels = var_levels)) %>%
    spread(key = key, value = value, fill = NA)
  
  classmode(df_sch_wide, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Dataframe #2. School-level dropout rates by Grade and Gender
  ###' 
  ###' - Aggregate to School-Level and Calculate Dropout rates by Grade and Gender
  ###' - Collapse Ethnicity groups
  ###'
  ###'
  
  ###' Aggregate to school-level
  df_gender <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, GENDER) %>%
    summarise_at(.vars = vars, .funs = function(x) sum(x, na.rm = TRUE))
  
  
  ### Generate long data for enrollment
  df_gender_E_long <- df_gender %>%
    select(CDS_vars, GENDER, E_vars) %>%
    gather(key = Grade, value = E, E_vars) %>% 
    arrange(CountyCode, DistrictCode, SchoolCode, Grade) 
  
  
  df_temp_E <- df_gender_E_long %>%
    filter(Grade != "ETOT") %>%
    group_by(CountyCode, DistrictCode, SchoolCode, GENDER) %>%
    summarise(Grade = "ETotal", 
              E = sum(E, na.rm = TRUE))
  
  
  names(df_gender_E_long); names(df_temp_E)
  df_gender_E_long <- bind_rows(df_gender_E_long, df_temp_E) %>%
    mutate(Grade = factor(Grade, 
                          levels = c(E_vars, "ETotal"), 
                          labels = c(grade_labels, "Total"))) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, GENDER, Grade)
  
  classmode(df_gender_E_long, everything())
  
  
  ### Generate long data for dropout
  df_gender_D_long <- df_gender %>%
    select(CDS_vars, GENDER, D_vars) %>%
    gather(key = Grade, value = D, D_vars) %>% 
    arrange(CountyCode, DistrictCode, SchoolCode, Grade) 
  
  
  df_temp_D <- df_gender_D_long %>%
    filter(Grade != "DTOT") %>%
    group_by(CountyCode, DistrictCode, SchoolCode, GENDER) %>%
    summarise(Grade = "DTotal", 
              D = sum(D, na.rm = TRUE))
  
  
  names(df_gender_D_long); names(df_temp_D)
  df_gender_D_long <- bind_rows(df_gender_D_long, df_temp_D) %>%
    mutate(Grade = factor(Grade, 
                          levels = c(D_vars, "DTotal"), 
                          labels = c(grade_labels, "Total"))) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, Grade)
  
  classmode(df_gender_D_long, everything())
  
  
  ### Merge two long datasets
  df_gender_long <- df_gender_E_long %>%
    left_join(df_gender_D_long, by = c(CDS_vars, "GENDER", "Grade"))
  
  
  ### Calculate dropout rates
  df_gender_long <- df_gender_long %>%
    mutate(PCT_Dropout = 100*(D/E))
  
  
  ### Convert to wide format
  var_levels <- c(paste0("E_", grade_labels), "E_Total",  
                  paste0("D_", grade_labels), "D_Total", 
                  paste0("PCT_Dropout_", grade_labels), "PCT_Dropout_Total")
  
  df_gender_wide <- df_gender_long %>%
    gather(key = quantity, value = value, E, D, PCT_Dropout) %>%
    unite(col = key, quantity, Grade, sep = "_") %>% 
    mutate(key = factor(key, levels = var_levels)) %>%
    spread(key = key, value = value, fill = NA)
  
  classmode(df_gender_wide, everything())
  

  
  ###'######################################################################
  ###'
  ###' Dataframe #3. School-level dropout rates by Grade and Ethnicity
  ###' 
  ###' - Aggregate to School-Level and Calculate Dropout rates by Grade and Ethnicity
  ###' - Collapse Gender groups
  ###'
  ###'
  
  ###' Aggregate to school-level
  df_race <- df %>%
    group_by(CountyCode, DistrictCode, SchoolCode, ETHNIC) %>%
    summarise_at(.vars = vars, .funs = function(x) sum(x, na.rm = TRUE))
  
  
  ### Generate long data for enrollment
  df_race_E_long <- df_race %>%
    select(CDS_vars, ETHNIC, E_vars) %>%
    gather(key = Grade, value = E, E_vars) %>% 
    arrange(CountyCode, DistrictCode, SchoolCode, Grade) 
  
  
  df_temp_E <- df_race_E_long %>%
    filter(Grade != "ETOT") %>%
    group_by(CountyCode, DistrictCode, SchoolCode, ETHNIC) %>%
    summarise(Grade = "ETotal", 
              E = sum(E, na.rm = TRUE))
  
  
  names(df_race_E_long); names(df_temp_E)
  df_race_E_long <- bind_rows(df_race_E_long, df_temp_E) %>%
    mutate(Grade = factor(Grade, 
                          levels = c(E_vars, "ETotal"), 
                          labels = c(grade_labels, "Total"))) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, ETHNIC, Grade)
  
  classmode(df_race_E_long, everything())
  
  
  ### Generate long data for dropout
  df_race_D_long <- df_race %>%
    select(CDS_vars, ETHNIC, D_vars) %>%
    gather(key = Grade, value = D, D_vars) %>% 
    arrange(CountyCode, DistrictCode, SchoolCode, Grade) 
  
  
  df_temp_D <- df_race_D_long %>%
    filter(Grade != "DTOT") %>%
    group_by(CountyCode, DistrictCode, SchoolCode, ETHNIC) %>%
    summarise(Grade = "DTotal", 
              D = sum(D, na.rm = TRUE))
  
  
  names(df_race_D_long); names(df_temp_D)
  df_race_D_long <- bind_rows(df_race_D_long, df_temp_D) %>%
    mutate(Grade = factor(Grade, 
                          levels = c(D_vars, "DTotal"), 
                          labels = c(grade_labels, "Total"))) %>%
    arrange(CountyCode, DistrictCode, SchoolCode, Grade)
  
  classmode(df_race_D_long, everything())
  
  
  ### Merge two long datasets
  df_race_long <- df_race_E_long %>%
    left_join(df_race_D_long, by = c(CDS_vars, "ETHNIC", "Grade"))
  
  
  ### Calculate dropout rates
  df_race_long <- df_race_long %>%
    mutate(PCT_Dropout = 100*(D/E))
  
  
  ### Convert to wide format
  var_levels <- c(paste0("E_", grade_labels), "E_Total",  
                  paste0("D_", grade_labels), "D_Total", 
                  paste0("PCT_Dropout_", grade_labels), "PCT_Dropout_Total")
  
  df_race_wide <- df_race_long %>%
    gather(key = quantity, value = value, E, D, PCT_Dropout) %>%
    unite(col = key, quantity, Grade, sep = "_") %>% 
    mutate(key = factor(key, levels = var_levels)) %>%
    spread(key = key, value = value, fill = NA)
  
  classmode(df_race_wide, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Join the generated three dataframes
  ###'
  ###'

  temp_list <- list()
  
  ### Assign/Rename Category variables
  names(df_sch_wide)
  temp_list[[1]] <- df_sch_wide <- df_sch_wide %>%
    mutate(Category = "school aggregate") %>%
    select(CDS_vars, Category, everything())
  
  names(df_gender_wide)
  temp_list[[2]] <- df_gender_wide <- df_gender_wide %>%
    rename(Category = GENDER)
  
  names(df_race_wide)
  temp_list[[3]] <- df_race_wide <- df_race_wide %>%
    rename(Category = ETHNIC)
  
  
  ### Bind rows!
  df_bind <- bind_rows(temp_list)
  classmode(df_bind, everything())
  
  
  ### Arrange rows and add AcademicYear
  df_bind <- df_bind %>%
    arrange(CountyCode, DistrictCode, SchoolCode) %>%
    mutate(AcademicYear = if_else(as.numeric(year_num) > 90, 
                                  1900 + as.numeric(year_num), 
                                  2000 + as.numeric(year_num))) %>%
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
dualsave(df_bind, paste0("dropouts_managed_wide", "_1991_2016" ))
