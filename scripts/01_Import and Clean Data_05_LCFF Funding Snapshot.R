
###'######################################################################
###'
###' Import and Clean Data
###' 
###' 05_LCFF Funding Snapshot Data
###' 
###' Source: https://ias.cde.ca.gov/lcffsnapshot/lcff.aspx
###' 
###' FY1314
###' FY1415
###' FY1516
###' FY1617
###' FY1718
###' 
###' 20180901 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/LCFF_Funding_Snapshot")


### Call libraries
library(tidyverse)
library(readxl)
library(Hmisc)
library(ldat)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import raw excel files #1
###' 
###' File formats are consistent across:
###' 
###' FY1314, FY1415, FY1516 
###'
###'

### Set data working directory
setwd(data_dir)


### Import variable names
varnames <- read.csv(file = "funding_snapshot_variables.csv", 
                     as.is = TRUE)


### For loop over FY1314, 1415, 1516

FY_vec <- c(2013, 2014, 2015)

df_long_131415 <- data.frame()


for (i in seq_along(FY_vec)) {

  ### Import raw excel file
  filename <- paste0("lcffsnapshot", FY_vec[i] - 2000, "an", ".xls")
  
  df <- read_excel(filename, 
                   sheet = 3, 
                   col_names = TRUE, 
                   na = "N/A", 
                   skip = 9)
  
  
  ### Attach variable names
  names(df)[1:12] <- c("Ccode", "Dcode", "Scode", 
                       "LEA", "Charter_Num", "County", 
                       "ADA_K_3", "ADA_4_6", "ADA_7_8", "ADA_9_12", "ADA_Total", 
                       "UPP")
  
  
  ### Remove wrongly imported rows
  df <- df %>%
    filter(!is.na(Dcode) & !is.na(Scode) & !is.na(LEA))
  
  
  ###' An issue of "Remaining Need" variable:
  ###' What is the difference between "0" remaining and "At target"?
  ###' Generate a dummy variable indicating "At Target"
  ###' and Recode Remaining Need as NA
  
  df$At_Target <- ifelse(df$AC == "At Target", 1, 0)
  df$AC[df$AC == "At Target"] <- 0
  
  
  ### Reshape to long data & merge variable names
  df_long <- df %>% 
    select(Ccode, Dcode, Scode, County, LEA, Charter_Num, everything()) %>%
    gather(key, value, M:AH, na.rm = FALSE) %>%
    left_join(varnames[,-4], by = c("key")) %>%
    select(Ccode:Charter_Num, starts_with("ADA"), UPP, 
           category, variable, value, At_Target) %>%
    mutate_at(.vars = vars(Ccode, Dcode, Scode, value), 
              .funs = funs(as.numeric)) %>%
    mutate(LEA_type = ifelse(Scode == 0, "School District", "Charter School"), 
           Fiscalyear = FY_vec[i]) %>%
    select(Fiscalyear, Ccode:Charter_Num, LEA_type, At_Target, everything())
  
  
  ### Append to the previous dataframe
  df_long_131415 <- bind_rows(df_long_131415, df_long)
}

### Save as processed data
setwd(work_dir)
write.csv(df_long_131415, file = "processed_data/funding_snapshot_long_131415.csv")



###'######################################################################
###'
###' Import raw excel files #1
###' 
###' File formats are consistent across:
###' 
###' FY1617, FY1718
###' 
###' FY1718 is not finalized version (p2, not an). Need to be updated.
###'
###'

### Set data working directory
setwd(data_dir)


### Import variable names
varnames <- read.csv(file = "funding_snapshot_variables.csv", 
                     as.is = TRUE)


### For loop over FY1314, 1415, 1516

FY_vec <- c(2016, 2017)

df_long_1617 <- data.frame()


for (i in seq_along(FY_vec)) {
  
  ### Import raw excel file
  type <- ifelse(FY_vec[i] == 2016, "an", "p2")
  filename <- paste0("lcffsnapshot", FY_vec[i] - 2000, type, ".xlsx")
  
  df <- read_excel(filename, 
                   sheet = 2, 
                   col_names = FALSE, 
                   na = "N/A", 
                   skip = 7)
  
  
  ### Attach variable names
  names(df)[1:12] <- c("Ccode", "Dcode", "Scode", 
                       "LEA", "Charter_Num", "County", 
                       "ADA_K_3", "ADA_4_6", "ADA_7_8", "ADA_9_12", "ADA_Total", 
                       "UPP")
  
  
  ### Remove wrongly imported rows
  df <- df %>%
    filter(!is.na(Dcode) & !is.na(Scode) & !is.na(LEA))
  
  
  ###' An issue of "Remaining Need" variable:
  ###' What is the difference between "0" remaining and "At target"?
  ###' Generate a dummy variable indicating "At Target"
  ###' and Recode Remaining Need as NA
  
  df$At_Target <- ifelse(df$X__29 == "At Target", 1, 0)
  df$X__29[df$X__29 == "At Target"] <- 0
  
  
  ### Reshape to long data & merge variable names
  df_long <- df %>% 
    select(Ccode, Dcode, Scode, County, LEA, Charter_Num, everything()) %>%
    gather(key2, value, X__13:X__34, na.rm = FALSE) %>%
    left_join(varnames[-3], by = c("key2")) %>%
    select(Ccode:Charter_Num, starts_with("ADA"), UPP, 
           category, variable, value, At_Target) %>%
    mutate_at(.vars = vars(Ccode, Dcode, Scode, value), 
              .funs = funs(as.numeric)) %>%
    mutate(LEA_type = ifelse(Scode == 0, "School District", "Charter School"), 
           Fiscalyear = FY_vec[i]) %>%
    select(Fiscalyear, Ccode:Charter_Num, LEA_type, At_Target, everything())
  
  
  ### Append to the previous dataframe
  df_long_1617 <- bind_rows(df_long_1617, df_long)
}

### Save as processed data
setwd(work_dir)
write.csv(df_long_1617, file = "processed_data/funding_snapshot_long_1617.csv")



###'######################################################################
###'
###' Append the two long datasets
###' 
###'

funding_snapshot_13to17 <- bind_rows(df_long_131415, 
                                     df_long_1617)



###'######################################################################
###'
###' Calculate the per-pupil values
###'
###'

funding_snapshot_13to17 <- funding_snapshot_13to17 %>%
  mutate(value_PP = value/ADA_Total)



###'######################################################################
###'
###' Convert to the real 2016 dollars
###'
###'

funding_snapshot_13to17 <- CPI_converter(funding_snapshot_13to17, 
                                         year_to = 2016, 
                                         year_from = Fiscalyear, 
                                         variable_to_convert = value_PP)



###'######################################################################
###'
###' Save as .rda file
###'
###'

setwd(work_dir)
save(funding_snapshot_13to17, file = "processed_data/funding_snapshot_13to17.rda")

