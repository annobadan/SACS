
###'######################################################################
###'
###' (1) Defining Total Expenditures
###' 
###' => This forms the base for other calculations
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

df <- UserGL_merged; rm(UserGL_merged) 


### Filter all SACS outgo except a few categories  

df_outgo_def1 <- df %>%
  filter(Object >= 1000 & Object <= 7999) %>%     # filter only expenditures
  filter(!(Object >= 7100 & Object <= 7199)) %>%  # filter out tuition
  filter(!(Object >= 7600 & Object <= 7629)) %>%  # filter out interfund transfers
  filter(!Object %in% c(7211, 7221, 7281, 7280, 
                        7212, 7222, 7282))        # filter out other transfers


### Calculate total expenditure by Definition 1 

df_outgo_def1_dist <- df_outgo_def1 %>%
  group_by(Ccode, Dcode) %>%
  summarise(
    # Fiscal year
    Fiscalyear = first(Fiscalyear), 
    
    # District information
    Dname = first(Dname),
    Dtype = first(Dtype), 
    
    # Total Expenditure (Definition 1)
    TotalExp = round(sum(Value, na.rm = TRUE), 0), 
    TotalExp16 = round(sum(Value2016, na.rm = TRUE), 0)
  ) %>%
  select(Fiscalyear, Ccode, Dcode, everything())


###' Average Daily Attendence (differ by years)

if("K12ADA" %in% names(df_outgo_def1)){
  df_ADA <- df %>%
    group_by(Ccode, Dcode) %>%
    summarise_at(vars(matches("ADA")), first) %>% 
    
} else {
  df_ADA <- df %>%
    group_by(Ccode, Dcode) %>%
    summarise_at(vars(matches("ADA")), first) %>%
    left_join(df_outgo_def1_dist, by = c("Ccode", "Dcode")) %>%
    ungroup() %>%
    mutate(
      K12ADA = rowSums(df_ADA[, c("RegularADA", "SpecialEdADA")]), 
      TotalExp_perK12 = TotalExp/K12ADA, 
      TotalExp16_perK12 = TotalExp16/K12ADA, 
      TotalExp_perReg = TotalExp/RegularADA) 
      TotalExp16_perK12 = TotalExp16/RegularADA
      )
  
  
}
mutate(total = rowSums(votes[, candidates]))






###'######################################################################
###'
###' Definition 2: Only the General Fund
###' 
###' Takes total expenditures under Definition 1 and
###' and make additional exclusion of all funds 
###' except the General Fund (funds 1, 3, and 6 in SACS)
###' 
###' => Most California Department of Education (CDE) finance calculations, 
###'    including official cost of education figures, include only spending 
###'    from the general fund
###'    
###' 















