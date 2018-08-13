
###'######################################################################
###'
###' Import and Clean Current Expense of Education Data
###' 
###' See this webpage for the reference:
###' https://www.cde.ca.gov/ds/fd/ec/currentexpense.asp
###' 
###' 20180723 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Financial/Current Expense of Education")


### Call libraries
library(tidyverse)
library(readxl)
library(foreign)
library(haven)



###'######################################################################
###'
###' Import and Clean Excel data
###'

### Prepare loop over years
current_expense_allyears <- data.frame()
years <- paste0(sprintf("%02d",seq(3, 16)), sprintf("%02d",seq(4, 17)))

for (i in seq_along(years)){
  
  ### Import raw excel files
  year <- years[i]
  
  setwd(data_dir)
  
  filename <- paste0("currentexpense", year, ".xls")
  
  filepath <- file.path(paste0(data_dir, "/", filename))
  
  temp_read <- read_excel(filepath)
  
  
  ### Clean the imported data
  
  skiprows <- min(which(temp_read[[1]] == "01"))
  
  df <- temp_read[-(1:skiprows), ]
  
  names(df) <- c("Ccode", "Dcode", "Dname", "TotalExp_C", "K12ADA_C", "TotalExp_K12_C", "Dtype")
  
  to_numeric <- c("Ccode", "Dcode", "TotalExp_C", "K12ADA_C", "TotalExp_K12_C")
  
  df[, to_numeric] <- sapply(df[, to_numeric], as.numeric)
  
  
  ### Append to data frame
  df <- df %>% 
    mutate(FiscalYear = 2000 + as.numeric(substr(year, 1, 2))) %>%
    select(FiscalYear, Ccode, Dcode, Dname, Dtype, everything())
  
  current_expense_allyears <- rbind.data.frame(current_expense_allyears, df)
}



###'######################################################################
###'
###' Generate CPI adjusted expenditures
###'
###'

### Call functions
setwd(work_dir)
source("code/functions/CPI_converter.R")


### Generated CPI adjusted variables
df <- current_expense_allyears
df <- CPI_converter(df, 2016, FiscalYear, TotalExp_C)
df <- CPI_converter(df, 2016, FiscalYear, TotalExp_K12_C)



###'######################################################################
###'
###' Save the resulting data frame
###'
###'

### as .csv table
setwd(work_dir)
write.csv(df, file = "data/current_expense_of_education_allyears.csv")

### as .rda file
setwd(data_dir)
save(df, file = "current_expense_of_education_allyears.rda")

