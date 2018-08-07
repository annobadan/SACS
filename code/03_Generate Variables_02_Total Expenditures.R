
###'######################################################################
###'
###' Generate Variables
###' 
###' Longitudinal Trend of District-level ADA Weighted Averages
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

### Start with a clean slate
gc()            # force R to release memory it is no longer using
rm(list=ls())   # delete all the objects in the workspace


### Set working directory 
work_dir <- c("~/SACS")
setwd(work_dir)


### Set a directory containing large data files
data_dir <- c("D:/Data/LCFF/Financial/Annual Financial Data")


### Call libraries
library(tidyverse)
library(readxl)
library(foreign)
library(haven)


### Call functions
list.files("code/functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Generate district-level variables: Loop over years
###'    
###'          

### Prepare a for loop
years <- paste0(sprintf("%02d",seq(3, 16)), sprintf("%02d",seq(4, 17)))
definitions <- c("definition1", "definition2")


### Empty dataframes to collect the results
df_names <- c("total_def1", "total_def2", "std_vs_nonstd", 
              "nonstd_sub", "std_sub", "salaries", "benefits")

for (i in seq_along(df_names)){
  assign(paste0("collect_", df_names[i]), data.frame())
}


### Implement a for loop

for (i in seq_along(years)){  # Loop over years
  
  ### Import original dataset
  
  year_chr <- years[i]
  
  setwd(paste0(data_dir, "/sacs", year_chr))
  
  load(file = "UserGL_merged.rda")
  
  df <- UserGL_merged; rm(UserGL_merged) 
  
  
  ### Generate categories 
  
  df <- generate_categories(df)
  
  
  for (j in seq_along(definitions)){   ### Loop over definitions
    
    ###' Filter by precondtion and definition 
    ###' (1) Sort out only expenditures
    ###' (2) Sort cases based on definition of Total expenditures
    ###'     - Definition 1: All funds
    ###'     - Definition 2: General fund only 
    precondition_TRUE <- df$exp_vs_rev == "expenditure"
    definition_TRUE <- df[, names(df) == definitions[j]] == 1
    df_temp <- df[precondition_TRUE & definition_TRUE, ]
    
    
    ###'######################################################################
    ###'
    ###' Define a helper function
    ###' Collect pre-defined functions
    ###'
    ###'
    
    aggregate_with_3functions <- function(df, third_factor = NULL){
      
      third_factor <- enquo(third_factor)   # Enquote expressions
      df %>%
        aggregate_SACS_entries(Ccode, Dcode, !!third_factor) %>%
        perpupil_dollars_calculator(sum_value) %>%
        CPI_converter(2016, Fiscalyear, sum_value_PP) %>%
        filter(!is.na(!!third_factor))
    }
    
    
    ###'######################################################################
    ###'
    ###' Total Expenditure:  two factors (Ccode, Dcode) 
    ###'
    
    df_total_exp <- df_temp %>% 
      aggregate_SACS_entries(Ccode, Dcode) %>%
      perpupil_dollars_calculator(sum_value) %>%
      CPI_converter(2016, Fiscalyear, sum_value_PP)
    
    
    ###'######################################################################
    ###'
    ###' A for loop for data with three factors
    ###'
    ###'
    
    start <- which(names(df) == "std_vs_nonstd")
    stop <- which(names(df) == "pupil_services")
    name_vec <- names(df)[start:stop]
    
    for (k in seq_along(name_vec)){
      var <- (name_vec[k])
      temp <- aggregate_with_3functions(df_temp, var)  
      assign(paste0("df_", name_vec[k]), temp)
      
    }
    
    
    ###'######################################################################
    ###'
    ###' Student vs. Non-Student Spending
    ###'
    
    df_std_vs_nonstd <- df_temp %>% 
      aggregate_SACS_entries(Ccode, Dcode, std_vs_nonstd) %>%
      perpupil_dollars_calculator(sum_value) %>%
      CPI_converter(2016, Fiscalyear, sum_value_PP)
    
    df_std_vs_nonstd2 <- aggregate_with_3functions(df_temp, std_vs_nonstd)
    
    df_trial <- left_join(df_std_vs_nonstd, df_std_vs_nonstd2, 
                          by = c("Fiscalyear", "Ccode", "Dcode", "std_vs_nonstd"))
    
    which(is.na(df_trial$sum_value.x == df_trial$sum_value.y))
    
    df_trial[2156:2157, ]
    df_std_vs_nonstd[2156:2157, ]
    df_std_vs_nonstd2[2156:2157,]
    
    ###'######################################################################
    ###'
    ###' Non-Student Spending: Subcategories 
    ###'
    
    df_nonstd_sub <- df_temp %>% 
      aggregate_SACS_entries(Ccode, Dcode, nonstd_sub) %>%
      perpupil_dollars_calculator(sum_value) %>%
      CPI_converter(2016, Fiscalyear, sum_value_PP) %>%
      filter(!is.na(nonstd_sub))
    
    df_nonstd_sub2 <- aggregate_with_3functions(df_temp, 
                                                nonstd_sub)
    
    
    ###'######################################################################
    ###'
    ###' Student Spending: Subcategories 
    ###'
    
    df_std_sub <- df_temp %>% 
      aggregate_SACS_entries(Ccode, Dcode, std_sub) %>%
      perpupil_dollars_calculator(sum_value) %>%
      CPI_converter(2016, Fiscalyear, sum_value_PP) %>%
      filter(!is.na(std_sub))
    
    df_std_sub2 <- aggregate_with_3functions(df_temp, 
                                                std_sub)
    
    
    
  
  
  

  
  
  
  ###'######################################################################
  ###' 
  ###' 
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
  
  } # End of loop over definitions (j)
}   # End of loop over years (i)


###'######################################################################
###'
###' Save the resulting dataset
###'
###'

setwd(file.path(paste0(work_dir, "/data")))

rm(list = setdiff(ls(), paste0("collect_", which_df)))

save.image(file = "collect_dist_measures.RData")



