
###'######################################################################
###'
###' Import and Clean State California's Overall and Local Expenditures data
###' 
###' See this webpage for the reference:
###' https://www.census.gov/programs-surveys/school-finances.html
###' 
###' 201822 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Financial/State California's Overall and Local Expenditures")


### Call libraries
library(tidyverse)
library(readxl)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)




###'######################################################################
###'
###' Import and Clean Excel data
###' 
###' Package tabulizer doesn't work dueto rJava issues
###' 
###' - First, clean file name manually => Done
###' - Second, prepare excel datasets to import manually => Done
###' 
###'

### Import the prepared Excel file
May_revision_budget <- read_excel("processed_data/Collection_May Revision Budget.xlsx", 
                                  na = "NA")


### Reshape to long data
df <- May_revision_budget %>% 
  gather(Fiscalyear, value, FY1617:FY0001) 


### Convert Fiscalyear characters to numeric
df$Fiscalyear <- 2000 + as.numeric(substr(df$Fiscalyear, start = 3, stop = 4))


### Sort based on Fiscalyear
df <- df %>% 
  arrange(Fiscalyear)


### Convert to 2016 dollars using CPI converter
df <- CPI_converter(df, 
                    year_to = 2016, 
                    year_from = Fiscalyear, 
                    variable_to_convert = value)



###'######################################################################
###'
###' Save as .csv file
###'
###'

write.csv(df, "processed_data/May_Revision_Budget_2000_to_2016.csv")

