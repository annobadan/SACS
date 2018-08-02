
###'######################################################################
###'
###' Generate Variables: Expenditures
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
library(tidyverse)
library(readxl)
library(foreign)
library(haven)


### Call functions
source(file = "code/functions/generate_filters.R")
source(file = "code/functions/CPI_converter.R")
source(file = "code/functions/plot_trend.R")



###'######################################################################
###'
###' Generate district-level ADA over years
###' based on the Current Expense of Education data
###' 
###  

df_cur_exp <- read.csv(file = "data/current_expense_of_education_allyears.csv")

df_cur_exp_ADA <- df_cur_exp %>%
  select(FiscalYear, Ccode, Dcode, contains("ADA")) %>%
  rename(Fiscalyear = FiscalYear)



###'######################################################################
###'
###' Generate district-level variables: Loop over years
###'    
###'          

### Prepare loop
years <- paste0(sprintf("%02d",seq(3, 16)), sprintf("%02d",seq(4, 17)))

### Empty dataframes to collect the results from loops
which_df <- c("total_def1", "total_def2", "std_vs_nonstd", 
              "nonstd_sub", "std_sub", "salaries", "benefits")

for (i in seq_along(which_df)){
  assign(paste0("collect_", which_df[i]), data.frame())
}


### Implement loop
for (i in seq_along(years)){  # Loop over years
  
  ### Import original dataset
  
  year_chr <- years[i]
  
  setwd(paste0(data_dir, "/sacs", year_chr))
  
  load(file = "UserGL_merged.rda")
  
  df <- UserGL_merged; rm(UserGL_merged) 
  
  
  ### Generate filter variables
  
  df <- generate_filters(df)
  
  
  ###'######################################################################
  ###' 
  ###' Total expenditures: Definition 1
  ###' 
  ###' 
  
  df_temp <- df %>%
    filter(exp_vs_rev == "expenditure") %>%   # Only expenditures 
    filter(definition1 == 1) %>%              # Definition 1 (All funds)
    group_by(Ccode, Dcode) %>%
    summarise(
      # Fiscal year
      Fiscalyear = first(Fiscalyear), 
      
      # District information
      Dname = first(Dname),
      Dtype = first(Dtype), 
      
      # Total Expenditure
      TotalExp = sum(Value, na.rm = TRUE)
    ) %>%
    select(Fiscalyear, Ccode, Dcode, everything())
  
  ### Merge with ADA data
  df_temp <- left_join(df_temp, df_cur_exp_ADA, 
                             by = c("Fiscalyear", "Ccode", "Dcode"))
  
  ### Calculate values per ADA
  df_temp %>%
    mutate(TotalExp_PP = TotalExp/K12ADA_C) -> df_temp
  
  ### Transform into real 2016 dolloars using the CPI converter
  df_total_def1 <- CPI_converter(df_temp, 2016, Fiscalyear, TotalExp_PP)
  
  
  
  ###'######################################################################
  ###' 
  ###' Total expenditures: Definition 2
  ###' 
  ###' 
  
  df_temp <- df %>%
    filter(exp_vs_rev == "expenditure") %>%   # Only expenditures 
    filter(definition2 == 1) %>%              # Definition 2 (General funds)
    group_by(Ccode, Dcode) %>%
    summarise(
      # Fiscal year
      Fiscalyear = first(Fiscalyear), 
      
      # District information
      Dname = first(Dname),
      Dtype = first(Dtype), 
      
      # Total Expenditure
      TotalExp = sum(Value, na.rm = TRUE)
    ) %>%
    select(Fiscalyear, Ccode, Dcode, everything())
  
  ### Merge with ADA data
  df_temp <- left_join(df_temp, df_cur_exp_ADA, 
                       by = c("Fiscalyear", "Ccode", "Dcode"))
  
  ### Calculate values per ADA
  df_temp %>%
    mutate(TotalExp_PP = TotalExp/K12ADA_C) -> df_temp
  
  ### Transform into real 2016 dolloars using the CPI converter
  df_total_def2 <- CPI_converter(df_temp, 2016, Fiscalyear, TotalExp_PP)
  
  
  
  ###'######################################################################
  ###' 
  ###' Student vs. Non-Student Spending
  ###' 
  ###' 
  
  df_temp <- df %>%
    filter(exp_vs_rev == "expenditure") %>%   # Only expenditures 
    filter(definition1 == 1) %>%              # Definition 1 (All funds)
    group_by(Ccode, Dcode, std_vs_nonstd) %>%
    summarise(
      # Fiscal year
      Fiscalyear = first(Fiscalyear), 
      
      # District information
      Dname = first(Dname),
      Dtype = first(Dtype), 
      
      # Total Expenditure
      TotalExp = sum(Value, na.rm = TRUE)
    ) %>%
    select(Fiscalyear, Ccode, Dcode, everything())
  
  ### Merge with ADA data
  df_temp <- left_join(df_temp, df_cur_exp_ADA, 
                       by = c("Fiscalyear", "Ccode", "Dcode"))
  
  ### Calculate values per ADA
  df_temp %>%
    mutate(TotalExp_PP = TotalExp/K12ADA_C) -> df_temp
  
  ### Transform into real 2016 dolloars using the CPI converter
  df_std_vs_nonstd <- CPI_converter(df_temp, 2016, Fiscalyear, TotalExp_PP)
    
  
  
  ###'######################################################################
  ###' 
  ###' Non-Student Spending Subcategories
  ###' 
  ###' 
  
  df_temp <- df %>%
    filter(exp_vs_rev == "expenditure") %>%   # Only expenditures 
    filter(definition1 == 1) %>%              # Definition 1 (All funds)
    group_by(Ccode, Dcode, nonstd_sub) %>%
    summarise(
      # Fiscal year
      Fiscalyear = first(Fiscalyear), 
      
      # District information
      Dname = first(Dname),
      Dtype = first(Dtype), 
      
      # Total Expenditure
      TotalExp = sum(Value, na.rm = TRUE)
    ) %>%
    select(Fiscalyear, Ccode, Dcode, everything()) %>%
    filter(!is.na(nonstd_sub))
  
  ### Merge with ADA data
  df_temp <- left_join(df_temp, df_cur_exp_ADA, 
                       by = c("Fiscalyear", "Ccode", "Dcode"))
  
  ### Calculate values per ADA
  df_temp %>%
    mutate(TotalExp_PP = TotalExp/K12ADA_C) -> df_temp
  
  ### Transform into real 2016 dolloars using the CPI converter
  df_nonstd_sub <- CPI_converter(df_temp, 2016, Fiscalyear, TotalExp_PP)
  
  
  
  ###'######################################################################
  ###' 
  ###' Student Spending Subcategories
  ###' 
  ###' 
  
  df_temp <- df %>%
    filter(exp_vs_rev == "expenditure") %>%   # Only expenditures 
    filter(definition1 == 1) %>%              # Definition 1 (All funds)
    group_by(Ccode, Dcode, std_sub) %>%
    summarise(
      # Fiscal year
      Fiscalyear = first(Fiscalyear), 
      
      # District information
      Dname = first(Dname),
      Dtype = first(Dtype), 
      
      # Total Expenditure
      TotalExp = sum(Value, na.rm = TRUE)
    ) %>%
    select(Fiscalyear, Ccode, Dcode, everything()) %>%
    filter(!is.na(std_sub))
  
  ### Merge with ADA data
  df_temp <- left_join(df_temp, df_cur_exp_ADA, 
                       by = c("Fiscalyear", "Ccode", "Dcode"))
  
  ### Calculate values per ADA
  df_temp %>%
    mutate(TotalExp_PP = TotalExp/K12ADA_C) -> df_temp
  
  ### Transform into real 2016 dolloars using the CPI converter
  df_std_sub <- CPI_converter(df_temp, 2016, Fiscalyear, TotalExp_PP)
  
  
  
  ###'######################################################################
  ###' 
  ###' Salaries Subcategories
  ###' 
  ###' 
  
  df_temp <- df %>%
    filter(exp_vs_rev == "expenditure") %>%   # Only expenditures 
    filter(definition1 == 1) %>%              # Definition 1 (All funds)
    group_by(Ccode, Dcode, salaries) %>%
    summarise(
      # Fiscal year
      Fiscalyear = first(Fiscalyear), 
      
      # District information
      Dname = first(Dname),
      Dtype = first(Dtype), 
      
      # Total Expenditure
      TotalExp = sum(Value, na.rm = TRUE)
    ) %>%
    select(Fiscalyear, Ccode, Dcode, everything()) %>%
    filter(!is.na(salaries))
  
  ### Merge with ADA data
  df_temp <- left_join(df_temp, df_cur_exp_ADA, 
                       by = c("Fiscalyear", "Ccode", "Dcode"))
  
  ### Calculate values per ADA
  df_temp %>%
    mutate(TotalExp_PP = TotalExp/K12ADA_C) -> df_temp
  
  ### Transform into real 2016 dolloars using the CPI converter
  df_salaries <- CPI_converter(df_temp, 2016, Fiscalyear, TotalExp_PP)
  
  
  
  ###'######################################################################
  ###' 
  ###' Employ Benefits Subcategories
  ###' 
  ###' 
  
  df_temp <- df %>%
    filter(exp_vs_rev == "expenditure") %>%   # Only expenditures 
    filter(definition1 == 1) %>%              # Definition 1 (All funds)
    group_by(Ccode, Dcode, benefits) %>%
    summarise(
      # Fiscal year
      Fiscalyear = first(Fiscalyear), 
      
      # District information
      Dname = first(Dname),
      Dtype = first(Dtype), 
      
      # Total Expenditure
      TotalExp = sum(Value, na.rm = TRUE)
    ) %>%
    select(Fiscalyear, Ccode, Dcode, everything()) %>%
    filter(!is.na(benefits))
  
  ### Merge with ADA data
  df_temp <- left_join(df_temp, df_cur_exp_ADA, 
                       by = c("Fiscalyear", "Ccode", "Dcode"))
  
  ### Calculate values per ADA
  df_temp %>%
    mutate(TotalExp_PP = TotalExp/K12ADA_C) -> df_temp
  
  ### Transform into real 2016 dolloars using the CPI converter
  df_benefits <- CPI_converter(df_temp, 2016, Fiscalyear, TotalExp_PP)
  
  
  
  ###'######################################################################
  ###' 
  ###' Append to previous data frames
  ###' 
  ###' 
  
  for (i in seq_along(which_df)){
    assign(paste0("collect_", which_df[i]), 
           bind_rows(get(paste0("collect_", which_df[i])), 
                     get(paste0("df_", which_df[i])))) 
  }
  
  ### Print iterations
  cat("Fiscal Year =", year_chr, "\n", sep = " ")
  
} # End of loop over years (i)



###'######################################################################
###'
###' Save the resulting dataset
###'
###'

setwd(file.path(paste0(work_dir, "/data")))

rm(list = setdiff(ls(), paste0("collect_", which_df)))

save.image(file = "collect_dist_measures.RData")



