
###'######################################################################
###'
###' Transfrom into real 2016 dollars
###' 
###' Using the consumer price index
###' 
###' 20180715 JoonHo Lee
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
setwd("~/SACS")


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
###' Import CPI dataset
###' 
###'

### Set working directory
folder <- paste0(data_dir, "/CPI")
setwd(folder)


### Read CSV file
CPI <- read.csv(file = "CPI_1913-2017.csv", header = TRUE)


###' Calculate the conversion factors based on 2016 dollars
###' ex) $1.00 (2006) : $x (2016) = 201.6 : 240.0
###'     $x (2016) = 240.0 / 201.6   
CPI <- CPI %>% 
  mutate(cvt_factor = 240.0/CPI)




###'######################################################################
###'
###' CPI conversion over years
###'
###'

years <- c("0304", "0405", "0506", "0607", "0708", "0809", "0910", 
           "1011", "1112", "1213", "1314", "1415", "1516", "1617")


for (i in seq_along(years)){
  
  
  ### Assign fiscal year: letter and numeric
  year_chr <- years[i]
  year_num <- 2000 + as.numeric(substr(year_chr, 1, 2))


  ### Extract CPI conversion factor
  idx <- which(CPI$Year == year_num)
  cvt_factor <- CPI$cvt_factor[idx]
  
  
  ### Set data containing working directory
  folder <- paste0(data_dir, "/sacs", year_chr)
  setwd(folder)
  
  
  ### Load Cleaned UserGL and UserGL_Totals
  load(file = "UserGL_merged.rda")
  load(file = "UserGL_Totals_merged.rda")
  
  
  ### Multiply CPI conversion factor to the "values"
  UserGL_merged <- UserGL_merged %>%
    mutate(Value2016 = round(Value*cvt_factor, 2)) %>%
    select(-Value, -Value2016, Value, Value2016)
  
  UserGL_Totals_merged <- UserGL_Totals_merged %>%
    mutate(Value2016 = round(Value*cvt_factor, 2)) %>%
    select(-Value, -Value2016, Value, Value2016)
  

  ### Save data with Value2016 
  save(UserGL_merged, file = "UserGL_merged.rda")
  write_dta(UserGL_merged, "UserGL_merged.dta")
  write.csv(UserGL_merged, "UserGL_merged.csv")
  
  save(UserGL_Totals_merged, file = "UserGL_Totals_merged.rda")
  write_dta(UserGL_Totals_merged, "UserGL_Totals_merged.dta")
  write.csv(UserGL_Totals_merged, "UserGL_Totals_merged.csv")

  ### End of for loop
}



