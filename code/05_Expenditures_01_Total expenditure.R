
###'######################################################################
###'
###' (1) Defining Total Expenditures
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
###' Definition 1: All funds
###' 
###' Defines total expenditures as all SACS outgo (objects 1000-7999)
###' 
###' except:
###' 
###' - Tuition (objects 7100-7199)
###' - Transfers to Other Districts (objects 7211, 7221, 7281)
###' - Transfers to Charter in Lieu of Property Taxes (object 7280)
###' - Inter-fund Transfers (object7600-7629)
###' - Transfers to County Offices of Education (objects 7212, 7222, 7282)
###' 
###' These categories are primarily transfers that will be accounted for
###' elsewhere and thus should be taken out to avoid double-counting.
###'
###'


### Prepare loop
years <- paste0(sprintf("%02d",seq(3, 16)), sprintf("%02d",seq(4, 17)))


### Import original dataset

year_chr <- years[i]

setwd(paste0(data_dir, "/sacs", year_chr))

load(file = "UserGL_merged.rda")

df <- UserGL_merged 





















