
###'######################################################################
###'
###' Split UserGL_merged files by district
###' 
###' To check out raw data points more easily
###' 
###' 
###' 20180716 JoonHo Lee
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




###'######################################################################
###'
###' Import UserGL files each year and save CVS files by district
###'
###'


years <- paste0(sprintf("%02d",seq(3, 16)), sprintf("%02d",seq(4, 17)))


for (i in seq_along(years)){

  ### Assign fiscal year
  year_chr <- years[i]
  
  ### Load UserGL file 
  folder <- paste0(data_dir, "/sacs", year_chr)
  setwd(folder)
  load(file = "UserGL_merged.rda")
  
  ### Split by district name
  splited_df <- split(UserGL_merged, UserGL_merged$Dname)
  
  ### Save each list component as csv file
  subfolder <- paste0(folder, "/splited_UserGL_merged")
  dir.create(subfolder)
  
  for (j in seq_along(splited_df)){
    
    # Extract the district name
    district_name <- unique(splited_df[[j]]$Dname)
    
    # Substitue "/" within distric name 
    district_name <- gsub("\\W", "_", district_name)
    
    # Save as CSV file
    write.csv(splited_df[[j]], file = paste0(subfolder, "/", district_name, ".csv"))
    
  }
}

