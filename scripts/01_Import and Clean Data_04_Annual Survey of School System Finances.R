
###'######################################################################
###'
###' Import and Clean Annual Survey of School System Finances
###' 
###' See this webpage for the reference:
###' https://www.census.gov/programs-surveys/school-finances.html
###' 
###' 201819 JoonHo Lee
###' 
###' 

###'######################################################################
###'
###' Basic settings
###'
###'

### Remove previous workspace
rm(list=ls())


### Set working directory 
work_dir <- c("~/SACS")
setwd(work_dir)


### Set data containing working directory
data_dir <- c("D:/Data/LCFF/Financial/Annual Survey of School System Finances")


### Call libraries
library(tidyverse)
library(readxl)
library(foreign)
library(haven)



###'######################################################################
###'
###' Import and Clean Excel data
###' 
###' (1) Individual Unit Tables 2000 - 2016
###' 
###' Variable formats are consistent from 2003
###' Append dataframe from 2003 to 2016
###'
###'

### Set data containing working directory
setwd(paste0(data_dir, "/Individual Unit Tables"))


### Import all files and save as list

years <- sprintf("%02d",seq(0, 16))

filenames <- paste0("elsec", years, "t")

indv_units_list <- list() 

for (i in seq_along(filenames)){
  
  df <- read.dta(paste0(filenames[i], ".dta"))
  
  indv_units_list[[i]] <- df
  
  names(indv_units_list)[i] <- 2000 + as.numeric(years[i])
  
}


### Save datalist as RData
setwd(work_dir)
save(indv_units_list, file = "processed_data/Individual_Unit_Tables_list.Rdata")


### Append dataframe from 2003 to 2016

years03 <- years[-(1:3)]

indv_units_df <- data.frame()


for (i in seq_along(years03)){
  
  idx <- as.character(2000 + as.numeric(years03[i]))
  
  temp_df <- indv_units_list[[idx]]             # extract dataframe from the list
  
  temp_df$Fiscalyear <- as.numeric(idx)         # add year indicator
  
  indv_units_df <- bind_rows(indv_units_df, temp_df)  # append to the df_collection
  
}


### Save the dataframe as .rda format
indv_units_df <- indv_units_df %>%
  select(Fiscalyear, everything())

save(indv_units_df, file = "processed_data/Individual_Unit_Tables_df.rda")



###'######################################################################
###'
###' Import and Clean Excel data
###' 
###' (2) All Data Items
###'
###'

### Set data containing working directory
setwd(paste0(data_dir, "/All Data Items"))


### Import all files and save as list

years <- sprintf("%02d",seq(0, 16))

filenames <- paste0("elsec", years)

all_data_items_list <- list() 

for (i in seq_along(filenames)){
  
  df <- read.dta(paste0(filenames[i], ".dta"))
  
  all_data_items_list[[i]] <- df
  
  names(all_data_items_list)[i] <- 2000 + as.numeric(years[i])
  
}


### Save datalist as RData
setwd(work_dir)
save(all_data_items_list, file = "processed_data/All_Data_Items_list.Rdata")



###'######################################################################
###'
###' Extract basic information from All Data Items  
###' and merge it with Individual Unit Tables 
###'
###' merge STATE & SCHLEV based on NCESID
###' 
###'

### Extract STATE & SCHLEV

years03 <- years[-(1:3)]

add_info_df <- data.frame()

for (i in seq_along(years03)){
  
  idx <- as.character(2000 + as.numeric(years03[i]))
  
  temp_df <- all_data_items_list[[idx]] %>%
    mutate(Fiscalyear = 2000 + as.numeric(YRDATA)) %>%
    select(Fiscalyear, NCESID, STATE, SCHLEV)
  
  add_info_df <- bind_rows(add_info_df, temp_df)
  
}


### Merge it with Individual Unit Tables
indv_units_df <- left_join(indv_units_df, add_info_df, 
                           by = c("Fiscalyear", "NCESID"))

indv_units_df <- indv_units_df %>%
  select(Fiscalyear, STATE, IDCENSUS, NCESID, SCHLEV, everything())

save(indv_units_df, file = "processed_data/Individual_Unit_Tables_df.rda")



###'######################################################################
###'
###' Mutate two factors:
###' 
###' (1) State_Name, State Abbreviation
###' (2) SChool level
###'
###'

### Add State Name and Abbreviation
State_Code <- read_excel("processed_data/State_Code.xlsx")

indv_units_df$STATE <- as.numeric(indv_units_df$STATE)

indv_units_df <- indv_units_df %>% 
  left_join(State_Code, by = c("STATE")) %>%
  select(Fiscalyear, STATE, STATE_NAME, STATE_ABB, everything())


### Add School level factors

School_Level <- data.frame(
  c("Elementary School System Only", 
    "Secondary School System Only", 
    "Elementary-Secondary School System", 
    "Vocational or Special Education School System", 
    "Nonoperating School System", 
    "Educational Service Agency"), 
  c(1, 2, 3, 5, 6, 7)
)

names(School_Level) <- c("School_Level", "SCHLEV")

indv_units_df$SCHLEV <- as.numeric(indv_units_df$SCHLEV)

indv_units_df <- indv_units_df %>%
  left_join(School_Level, by = c("SCHLEV")) %>%
  select(Fiscalyear:NCESID, SCHLEV, School_Level, everything())
  

### Save the resulting data frame
save(indv_units_df, file = "processed_data/Individual_Unit_Tables_df.rda")




