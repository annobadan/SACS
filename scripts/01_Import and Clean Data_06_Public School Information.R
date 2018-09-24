
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - Public Schools Information
###' 
###' These data files are updated daily.
###'  
###' 
###' 01. Public Schools: pubschls
###' 
###' 
###' 20180924 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/School_Listing/Public_Schools_and_Districts")


### Call libraries
library(tidyverse)
library(readxl)
library(Hmisc)
library(ldat)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import Public Schools data file
###' 
###' Original file format: .txt
###'
###'

### Set the data containing folder as a working directory
setwd(data_dir)


### Import tab delimited text file
pubschls <- read.delim("pubschls_20180923.txt", header = TRUE, 
                       colClasses = "character")


### Check the classes of imported variables
classmode(pubschls, everything())


### Assign simple working name for dataset
df <- pubschls 
names(df)



###'######################################################################
###'
###' Select only necessary variables
###' 
###'

names(df)

df <- df %>% 
  select(CDSCode, County, District, School,
         DOCType, SOCType, 
         EdOpsName, 
         EILName, 
         GSoffered, GSserved, 
         Charter, CharterNum, FundingType, 
         Magnet, Virtual, 
         StatusType, OpenDate, ClosedDate, 
         NCESDist, NCESSchool, 
         Latitude, Longitude)



###'######################################################################
###'
###' Convert empty strings to NA
###'
###'

### Check the classes of variables
classmode(df, everything())


### Transform all columns
df <- df %>% mutate_all(funs(empty_as_na)) 



###'######################################################################
###'
###' CDS code: 
###' break into County Code, District Code, School Code
###'
###'

### Check number of characters
classmode(df, CDSCode)
table(nchar(df$CDSCode))


### Substring CountyCode
df$CountyCode <- substr(df$CDSCode, start = 1, stop = 2)
df$DistrictCode <- substr(df$CDSCode, start = 3, stop = 7)
df$SchoolCode <- substr(df$CDSCode, start = 8, stop = 14)

tabdf(df, CountyCode)
tabdf(df, DistrictCode)
tabdf(df, SchoolCode)


### Convert character to numeric
classmode(df, CountyCode, DistrictCode, SchoolCode)
df <- df %>%
  mutate_at(.vars = c("CountyCode", "DistrictCode", "SchoolCode"), 
            .funs = as.numeric)

classmode(df, CountyCode, DistrictCode, SchoolCode)
tabdf(df, CountyCode)


### Rearrange row and colmn orders
df <- df %>%
  arrange(CountyCode, DistrictCode, SchoolCode) %>%
  rename(CountyName = County, DistrictName = District, SchoolName = School) %>%
  select(CDSCode, CountyCode, DistrictCode, SchoolCode,  
         CountyName, DistrictName, SchoolName, 
         everything())



###'######################################################################
###' 
###' School name 
###' 

### Recode empty string to NA
nrow(df)
nrow(df %>% filter(SchoolName == ""))
df$SchoolName[df$SchoolName == ""] <- NA



###'######################################################################
###' 
###' DOC: convert to factor
###' 
###' The District Ownership Code (DOC) is the numeric code 
###' used to identify the category of the Administrative Authority.
###' 
###' 

### Rename DOCtype to DOC
df <- df %>% rename(DOC = DOCType)
tabdf(df, DOC)


### Convert to factor
df$DOC <- factor(df$DOC)
tabdf(df, DOC)



###'######################################################################
###' 
###' SOC: Convert to factor
###' 
###' 	The School Ownership Code is a numeric code 
###' 	used to identify the type of school.
###' 
###' 

### Rename SOCtype to SOC
df <- df %>% rename(SOC = SOCType)
tabdf(df, SOC)


### Convert to factor
df$SOC <- factor(df$SOC)
classmode(df, SOC)
tabdf(df, SOC)



###'######################################################################
###'
###' EdOps
###'
###' The Education Option Code is a short text description of 
###' the type of education offered.
###' 

### Rename variable 
df <- df %>% rename(EdOps = EdOpsName)
tabdf(df, EdOps)


### Convert to factor
df$EdOps <- factor(df$EdOps)
tabdf(df, EdOps)



###'######################################################################
###'
###' EIL
###' 
###' The Educational Instruction Level Code is a short text description of 
###' the institution's type relative to the grade range served.
###'
###'

### Rename variable
df <- df %>% rename(EIL = EILName)
tabdf(df, EIL)


### Convert to factor
df$EIL <- factor(df$EIL)
tabdf(df, EIL)



###'######################################################################
###'
###' GSoffered, GSserved
###'
###'

### Check distribution
tabdf(df, GSoffered)
tabdf(df, GSserved)


###'######################################################################
###'
###' Charter variables
###'
###'

### Check distribution
tabdf(df, Charter)
tabdf(df, CharterNum)
tabdf(df, FundingType)


### Generate charter dummy
tabdf(df, Charter)
df$Charter <- if_else(df$Charter == "Y", 1, 0)


### Convert CharterNum to numeric
classmode(df, CharterNum)
df$CharterNum <- as.numeric(df$CharterNum)


## Convert Fundingtype to factor
classmode(df, FundingType)
df$FundingType <- factor(df$FundingType)
tabdf(df, FundingType)



###'######################################################################
###'
###' Magnet and Virtual
###'
###'

### Check distribution
tabdf(df, Magnet)
tabdf(df, Virtual)


### Generate Magnet dummy variable
df$Magnet <- if_else(df$Magnet == "Y", 1, 0)


### Virtual: convert to factor 
df$Virtual <- factor(df$Virtual, 
                     levels = c("F", "V", "C", "N"), 
                     labels = c("Exclusively Virtual", 
                                "Primarily Virtual", 
                                "Primarily Classroom", 
                                "Not Virtual"))

tabdf(df, Virtual)



###'######################################################################
###'
###' Status Type
###' 
###'

### Check distribution
tabdf(df, StatusType)


### Convert to factor
df$StatusType <- factor(df$StatusType)



###'######################################################################
###'
###' Open/Closed dates
###'

# # df$OpenDate <- as.Date(df$OpenDate, format = "%m/%d/%y")
# 
# df$OpenDate <- as.Date(df$OpenDate)
# 
# 
# # df$ClosedDate <- as.Date(df$ClosedDate, format = "%m/%d/%y")
# 
# df$ClosedDate <- as.Date(df$ClosedDate)


classmode(df, OpenDate)

classmode(df, ClosedDate)



###'######################################################################
###'
###' Latitude/Longitude
###' 

df$Latitude <- as.numeric(df$Latitude)
df$Longitude <- as.numeric(df$Longitude)



###'######################################################################
###'
###' NCESDist/NCESSchool
###'
###'

df$NCESDist <- as.numeric(df$NCESDist)
df$NCESSchool <- as.numeric(df$NCESSchool)

classmode(df, NCESDist)
classmode(df, NCESSchool)



###'######################################################################
###' 
###' Save data objects
###' 
###' 

### Assign object name
pubschls <- df


### Set working directory for storing data
setwd(data_dir)


### Save the resulting data
save(pubschls, file = "pubschls_cleaned_20180923.rda")
write.dta(pubschls, file = "pubschls_cleaned_20180923.dta")





