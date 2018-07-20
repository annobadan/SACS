
###'######################################################################
###'
###' Generate District-level variables
###' 
###' 
###' 20180717 JoonHo Lee
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
work_dir <- c("~/LCFF/20180702_Simulated_Instrumental_Variable")
setwd(work_dir)


### Set data containing working directory
data_dir <- c("D:/Data/LCFF/Financial/Annual Financial Data")


### Call libraries
library(readxl)
library(foreign)
library(haven)
library(dplyr)
library(ggplot2)





### Import original dataset

years <- c("0304", "0405", "0506", "0607", "0708", "0809", "0910", 
           "1011", "1112", "1213", "1314", "1415", "1516", "1617")

year_chr <- years[i]

setwd(paste0(data_dir, "/sacs", year_chr))

load(file = "UserGL_merged.rda")




### 

























