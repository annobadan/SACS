
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
###'






















