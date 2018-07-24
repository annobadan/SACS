
###'######################################################################
###'
###' Generate K12 ADA variable
###' 
###' FY0304 - FY0708: RegularADA, SpecialEdADA, ROCPADA, AdultEdADA
###' FY0809 - FY1213: RegularADA, SpecialEdADA
###' FY1314 - FY1617: K12ADA
###' 
###' => Need to generate K12ADA for FY0304 - FY1213
###' 
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
data_dir <- c("D:/Data/LCFF/Financial/Annual Financial Data")

### Call libraries
library(readxl)
library(foreign)
library(haven)
library(dplyr)
library(ggplot2)
library(stringr)



### Prepare loop
years <- paste0(sprintf("%02d",seq(3, 16)), sprintf("%02d",seq(4, 17)))


### Import original dataset

year_chr <- years[i]

setwd(paste0(data_dir, "/sacs", year_chr))

load(file = "UserGL_merged.rda")

df <- UserGL_merged; rm(UserGL_merged) 



### Average Daily Attendence (differ by years)

if("K12ADA" %in% names(df)){
  df_ADA <- df %>%
    group_by(Ccode, Dcode) %>%
    summarise_at(vars(matches("ADA")), first) 
} else {
  df_ADA <- df %>%
    group_by(Ccode, Dcode) %>%
    summarise_at(vars(matches("ADA")), first) %>%
    ungroup() %>%
    # Assume that K12ADA = RegularADA + SpecialEdADA
    # Excluding Regional Occupation Center/Program (ROCPADA) and Adult Education (AdultEdADA)
    # ROCPADA and AdultEdADA are available only for FY0304-0708
    mutate(K12ADA = rowSums(df_ADA[, c("RegularADA", "SpecialEdADA")]))
}



















