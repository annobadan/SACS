
###'######################################################################
###'
###' Task    : Relating Kindergarten Program Type with School Poverty variables
###'           Generate snippet
###'           
###' Category: Data Management (for Visualization) 
###' 
###' Data    : CBEDS Data about Schools & Districts
###'           2015-16, 2016-17, 2017-18
###' 
###' Date    : 2019-02-10
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
###' Load pre-cleaned dataset
###'
###'

setwd(data_dir)

load("CBEDS_School_Data_01_Kindergarten_Program_Type_3-year-long_merged.rda")

df <- df_to_save; rm(df_to_save)



###'######################################################################
###'
###' Convert the kindergarten types into factor variables 
###'
###'

### Check variable classes and modes 
classmode(df, everything())
tabdf(df, Kindergarten)
tabdf(df, Transitional_K)


### Set factor labels
type_levels <- c("Full-day", 
                 "Full-and-Part-day", 
                 "Part-day", 
                 "None")

type_labels <- c("Full day", 
                 "Full and Part day", 
                 "Part day", 
                 "None")


### Convert into factors
df <- df %>%
  mutate(Kindergarten = factor(Kindergarten, 
                               levels = type_levels, 
                               labels = type_labels), 
         Transitional_K = factor(Transitional_K, 
                               levels = type_levels, 
                               labels = type_labels))



###'######################################################################
###'
###' Add one more factor variable:
###' 
###' Merging "Part-and-full day" into "Full-day" 
###' 
###' 
###'
###'

### Check variable classes and modes 
classmode(df, everything())
tabdf(df, Kindergarten)
tabdf(df, Transitional_K)


### Mutate new variable (1) Kindergarten
df <- df %>%
  mutate(Kindergarten3 = case_when(
    
    Kindergarten %in% c("Full day", "Full and Part day") ~ "Full day", 
    Kindergarten %in% c("Part day") ~ "Part day", 
    Kindergarten %in% c("None") ~ "None", 
    TRUE ~ as.character(Kindergarten)
    
  )
)

df %>%
  select(Kindergarten, Kindergarten3)


### Mutate new variable (2) Transitional Kindergarten
df <- df %>%
  mutate(Transitional_K3 = case_when(
    
    Transitional_K %in% c("Full day", "Full and Part day") ~ "Full day", 
    Transitional_K %in% c("Part day") ~ "Part day", 
    Transitional_K %in% c("None") ~ "None", 
    TRUE ~ as.character(Transitional_K)
    
  )
)

df %>%
  select(Transitional_K, Transitional_K3)


### Convert to factors

type <- c("Full day", "Part day", "None")

df <- df %>%
  mutate(Kindergarten3 = factor(Kindergarten3, 
                                levels = type, labels = type), 
         Transitional_K3 = factor(Transitional_K3, 
                                  levels = type, labels = type))

classmode(df, Kindergarten3, Transitional_K3)
tabdf(df, Kindergarten3)
tabdf(df, Transitional_K3)



###'######################################################################
###'
###' Filter only Elementary Schools
###'
###' - It seems that we can exclude all school types other than Elementary
###'
###'

tbl_temp <- df %>%
  group_by(Traditional_SOC, Charter, Kindergarten3) %>%
  summarise(N = n())


tbl_temp_year <- df %>%
  group_by(Traditional_SOC, Charter, Kindergarten3, AcademicYear) %>%
  summarise(N = n())


df_elem <- df %>%
  filter(Traditional_SOC == "Elementary School")



###'######################################################################
###'
###' Generate a filter
###'
###' (1) for selecting only Elementary Schools Available across three years: 
###' 
###' 2015-16, 2016-17, 2017-18
###' 
###'
###'

### Generate summary statistics counting observed number of years in each school
df_temp <- df_elem %>%
  group_by(CountyCode, DistrictCode, SchoolCode) %>%
  summarise(N_years = n_distinct(AcademicYear))

tabdf(df_temp, N_years)


### Generate filter in df_elem dataset
df_elem <- df_elem %>%
  group_by(CountyCode, DistrictCode, SchoolCode) %>%
  mutate(N_years = n_distinct(AcademicYear))



###'######################################################################
###'
###' Generate a filter
###'
###' (2) for selecting only Elementary Schools Offering Kindergarten: 
###' 
###' 2015-16, 2016-17, 2017-18
###' 
###'
###'

### Generate summary statistics 
tabdf(df_elem, GSoffered)
tabdf(df_elem, GSserved)


### Generate filter in df_elem dataset
df_elem <- df_elem %>%
  mutate(Kinder_Offered = case_when(
    grepl("K", GSoffered) | grepl("P", GSoffered) ~ "Offered", 
    is.na(GSoffered) ~ NA_character_, 
    TRUE ~ "Not Offered"
  ))

tabdf(df_elem, GSoffered)
tabdf(df_elem, Kinder_Offered)


df_elem <- df_elem %>%
  mutate(Kinder_Served = case_when(
    grepl("K", GSserved) | grepl("P", GSserved) ~ "Served", 
    is.na(GSserved) ~ NA_character_, 
    TRUE ~ "Not Served"
  ))

tabdf(df_elem, GSserved)
tabdf(df_elem, Kinder_Served)



###'######################################################################
###'
###' Generate a quartile indicator base only on elementary schools
###' 
###' 

df_FRPM <- df_elem %>%
  group_by(CountyCode, DistrictCode, SchoolCode) %>%
  summarise(MN_PCT_FRPM = mean(PCT_FRPM_CALPADS_Undup, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(quartile = factor(ntile(MN_PCT_FRPM, 4), 
                           levels = seq(1, 4, 1), 
                           labels = paste0("Quartile", seq(1, 4, 1))))

tabdf(df_FRPM, quartile)


### Check cutpoints
df_FRPM_cutpoints <- df_FRPM %>%
  group_by(quartile) %>%
  summarise(N = n_distinct(SchoolCode), 
            min = min(MN_PCT_FRPM, na.rm = TRUE), 
            mean = mean(MN_PCT_FRPM, na.rm = TRUE), 
            max = max(MN_PCT_FRPM, na.rm = TRUE)) %>%
  drop_na()

setwd(data_dir)
write.csv(df_FRPM_cutpoints, "Elementary schools 3-year mean FRPM cutpoints.csv")



### Merge with df_elem
df_elem <- df_elem %>%
  left_join(df_FRPM, by = c("CountyCode", 
                            "DistrictCode", 
                            "SchoolCode"))



###'######################################################################
###'
###' Save the resulting datafiles
###' 
###' 

dualsave(df_elem, "CBEDS_School_Data_01_Kindergarten_Program_Type_3-year-long_managed")


