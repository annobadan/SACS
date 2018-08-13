
###'######################################################################
###'
###' Generate Variables
###' 
###' AVERAGE DAILY ATTENDANCE
###' 
###' Average Daily Attendence (differ by years) 
###' FY0304 - FY0708: RegularADA, SpecialEdADA, ROCPADA, AdultEdADA
###' FY0809 - FY1213: RegularADA, SpecialEdADA
###' FY1314 - FY1617: K12ADA
###' => Need to generate K12ADA for FY0304 - FY1213
###' 
###' Add K12ADA from Current Expense of Education Data 
###' => This has a priority
###' 
###' 
###' 20180719 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Financial/Annual Financial Data")


### Call libraries
library(tidyverse)
library(readxl)
library(foreign)
library(haven)



###'######################################################################
###'
###' District-level ADA over years
###' (1) Based on the Current Expense of Education data
###' 
###  

df_cur_exp <- read.csv(file = "data/current_expense_of_education_allyears.csv")

df_ADA_cur_exp <- df_cur_exp %>%
  select(FiscalYear, Ccode, Dcode, contains("ADA")) %>%
  rename(Fiscalyear = FiscalYear)



###'######################################################################
###'
###' District-level ADA over years
###' (2) Based on the SACS UserGL data
###' 
###' FY0304 - FY0708: RegularADA, SpecialEdADA, ROCPADA, AdultEdADA
###' FY0809 - FY1213: RegularADA, SpecialEdADA
###' FY1314 - FY1617: K12ADA
###' => Need to generate K12ADA for FY0304 - FY1213     
###'
###'

years <- paste0(sprintf("%02d",seq(3, 16)), sprintf("%02d",seq(4, 17)))

df_ADA_SACS <- data.frame()

for (i in seq_along(years)){  # Loop over 14 years
  
  ### Import original dataset
  
  year_chr <- years[i]
  
  setwd(paste0(data_dir, "/sacs", year_chr))
  
  load(file = "UserGL_merged.rda")
  
  df <- UserGL_merged; rm(UserGL_merged) 
  
  ### Summarize the calculated total expenditure
  if("K12ADA" %in% names(df)){
    df_ADA <- df %>%
      group_by(Ccode, Dcode) %>%
      summarise_at(vars(matches("ADA")), first)
  } else {
    df_ADA <- df %>%
      group_by(Ccode, Dcode) %>%
      summarise_at(vars(matches("ADA")), first) %>%
      ungroup() %>%
      #' Assume that K12ADA = RegularADA + SpecialEdADA
      #' Excluding Regional Occupation Center/Program (ROCPADA) and 
      #' Adult Education (AdultEdADA)
      #' ROCPADA and AdultEdADA are available only for FY0304-0708
      mutate(K12ADA = rowSums(.[, c("RegularADA", "SpecialEdADA")]))
  }
  
  ### Add a year indicator
  df_ADA$Fiscalyear <- 2000 + as.numeric(substr(year_chr, 1, 2))

  ### Collect df_ADA over years
  df_ADA_SACS <- bind_rows(df_ADA_SACS, df_ADA)
  
  ### Print iterations
  cat("Fiscal Year =", year_chr, "\n", sep = " ")
}



###'######################################################################
###'
###' Merge the two dataframes:
###' 
###' (1) df_ADA based on the Current Expense of Education
###' (2) df_ADA based on the SACS UserGL data
###'
###'

df_ADA_allyears <- left_join(df_ADA_SACS, df_ADA_cur_exp, 
                             by = c("Fiscalyear", "Ccode", "Dcode"))

df_ADA_allyears <- df_ADA_allyears %>%
  select(Fiscalyear, everything())



###'######################################################################
###'
###' Save the resulting object as .csv and .rda format
###'
###'

setwd(work_dir)
folder <- "data"
filename <- "df_ADA_allyears"

save(df_ADA_allyears, file = paste0(folder, "/", filename, ".rda"))
write.csv(df_ADA_allyears, file = paste0(folder, "/", filename, ".csv"))

