
###'######################################################################
###'
###' Convert String Code to Numeric
###' 
###'
###' 
###' 20180721 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Financial/Annual Financial Data")


### Call libraries
library(readxl)
library(foreign)
library(haven)
library(dplyr)
library(ggplot2)


###'######################################################################
###'
###' Identify rows including characters in each code variable
###' 
###' Fund, Resource, Goal, Function, Object 
###'  
###'

### Prepare loop
years <- paste0(sprintf("%02d",seq(3, 16)), sprintf("%02d",seq(4, 17)))
str_codes_allyears <- data.frame()


for (i in seq_along(years)){
  
  ### Import original datasets
  
  year_chr <- years[i]
  
  setwd(paste0(data_dir, "/sacs", year_chr))
  
  load(file = "UserGL_merged.rda")
  
  df <- UserGL_merged; rm(UserGL_merged) 
  
  
  ### Identify the codes including characters
  
  codes_nonnum <- function(x) {
    nonnum_rows <- is.na(suppressWarnings(as.numeric(as.character(x))))
    unique(x[nonnum_rows & !is.na(x)])
  }
  
  col_idx <- which(names(df) %in% c("Fund", "Resource", "Goal", "Function", "Object"))
  
  str_codes <- sapply(df[col_idx], codes_nonnum)
  
  str_codes_df <- data.frame(year = year_chr, 
                             variable = rep(names(str_codes), lapply(str_codes, length)), 
                             value = unlist(str_codes))
  
  row.names(str_codes_df) <- c()
  
  str_codes_allyears <- rbind.data.frame(str_codes_allyears, str_codes_df)
  
  ### End of for loop
}

### Save the resulting table
setwd(work_dir)
write.csv(str_codes_allyears, file = "table/string_codes_allyears.csv")



###'######################################################################
###'
###' Recode the string codes to numeric
###' 
###' (1) Goal
###' 
###' ADLT = 10001
###' CHLD = 10002
###' CAFE = 10003
###' 
###' (2) Object
###' 
###' 979Z = 10001
###' PCRA = 10002
###' 
###' Note: Although the amount for the object code "PCRA" is shown 
###' in the LE-B worksheet, it is not included in the TOTAL COSTS 
###' because there are no comparable budget data. 
###' It is shown as a non-add item so the amount will 
###' be available for next year's reports.
###'
###'

years <- paste0(sprintf("%02d",seq(3, 16)), sprintf("%02d",seq(4, 17)))

for (i in seq_along(years)){
  
  ### Import original datasets
  
  year_chr <- years[i]
  
  setwd(paste0(data_dir, "/sacs", year_chr))
  
  load(file = "UserGL_merged.rda")
  
  df <- UserGL_merged; rm(UserGL_merged) 
  
  
  ### Recode "Goal" and "Object" variables
  
  df$Goal[df$Goal == "ADLT"] <- "10001"
  df$Goal[df$Goal == "CHLD"] <- "10002"
  df$Goal[df$Goal == "CAFE"] <- "10003"
  df$Object[df$Object == "979Z"] <- "10001"
  df$Object[df$Object == "PCRA"] <- "10002"
  
  
  ### Lable PCRA
  df$Object_Desc[df$Object == "10002"] <- "Program Cost Report Allocations"
  
  
  ### Convert string codes into numeric
  df[col_idx] <- sapply(df[col_idx], as.numeric)
  
  
  ### Save the converted data
  UserGL_merged <- df; rm(df) 
  save(UserGL_merged, file = "UserGL_merged.rda")
}

