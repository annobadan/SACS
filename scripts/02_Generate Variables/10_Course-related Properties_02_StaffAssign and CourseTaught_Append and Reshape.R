
###'######################################################################
###'
###' Generate Variables
###' 
###' Course-related Properties 2003-2017
###' 
###' (1) Append => Generate long dataset
###'    - df_Class Periods and Sizes_2003-2011.rda
###'    - df_Class Periods and Sizes_2012-2017.rda
###'    
###' (2) Spread => Generate wide dataset
###'   
###' 
###' 20181230 JoonHo Lee
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


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import generated datasets
###'
###'

setwd(data_dir)

### Data 2003-2011
load(file = "df_Class Periods and Sizes_2003-2011.rda")
df_0311 <- df_to_save; rm(df_to_save)


### Data 2012-2017
load(file = "df_Class Periods and Sizes_2012-2017.rda")
df_1217 <- df_to_save; rm(df_to_save)



###'######################################################################
###'
###' Bind rows: 2003-2011 & 2012-2017
###'
###'

### Check the list of variables
names(df_0311)
names(df_1217)

all.equal(names(df_0311), names(df_1217))


### Bind rows and arrange
df_bind <- bind_rows(df_0311, df_1217) %>%
  arrange(CountyCode, DistrictCode, SchoolCode, Subject_Category, 
          AcademicYear)


### Save the long data
dualsave(df_bind, "df_Class Periods and Sizes_2003-2017_long")



###'######################################################################
###'
###' Convert to wide format
###'
###'

### Check distribution of Subject Category
temp <- tabdf(df_bind, Subject_Category)
classmode(df_bind, everything())


### Recode Subject_Category into short characters
long_names <- temp[,1][1:length(temp[,1])-1]
short_names <- c("Admin", "Art", "CTE", "Computer", "ELA", "FoLan", 
                 "Social", "Math", "Other", "PE", "Science", "SelfCon", 
                 "SpeDesg", "SPED", "Total")

lookup <- setNames(short_names, long_names)  # lookup table

df_bind$Subject_Category <- unname(lookup[df_bind$Subject_Category])  # recode

# df_bind$Subject_Category[is.na(df_bind$Subject_Category)] <- "NA"

tabdf(df_bind, Subject_Category)


### Spread
varnames <- names(df_bind)[-(1:5)]

subj <- rep(short_names, length(varnames))

varnames_vec <- rep(varnames, each = length(short_names))

level_vec_extended <- paste(varnames_vec, subj, sep = "_")


df_wide <- df_bind %>%
  gather(key = key, value = value, N_Subject:MN_size_NonAP) %>%
  unite(variable, key, Subject_Category, sep = "_") %>%  
  spread(key = variable, value = value, fill = NA)

df_wide <- df_wide %>%
  dplyr::select(CountyCode:AcademicYear, level_vec_extended, everything()) %>%
  arrange(CountyCode, DistrictCode, SchoolCode, AcademicYear)


### Save the wide dataset
dualsave(df_wide, "df_Class Periods and Sizes_2003-2017_wide")


