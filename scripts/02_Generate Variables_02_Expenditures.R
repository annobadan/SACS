
###'######################################################################
###'
###' Generate Variables
###' 
###' Longitudinal Trend of District-level Per-Pupil Dollars
###' 
###' (1) Expenditures
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


### Names for the dataframes to collect
df_names <- c("total_exp", 
              "std_vs_nonstd", "nonstd_sub", "std_sub", "salaries", "benefits", 
              "goals", "general_Ed", "supp_Ed", "SPED", "functions", "SPED_inst", 
              "pupil_services")


### Implement a for loop
for (i in seq_along(definitions)){   ### Loop over definitions
  
  ### Assign/Reassign empty dataframes to collect the results
  for (m in seq_along(df_names)){
    assign(paste0("collect_", df_names[m]), data.frame())
  }
  
  for (j in seq_along(years)){  # Loop over years
  
  ### Import original dataset
  
  year_chr <- years[j]
  
  setwd(paste0(data_dir, "/sacs", year_chr))
  
  load(file = "UserGL_merged.rda")
  
  df <- UserGL_merged; rm(UserGL_merged) 
  
  
  ### Generate categories 
  
  df <- generate_categories(df)
  
  
  ###' Filter by precondtion and definition 
  ###' (1) Sort out only expenditures
  ###' (2) Sort cases based on definition of Total expenditures
  ###'     - Definition 1: All funds
  ###'     - Definition 2: General fund only 
  precondition_TRUE <- df$exp_vs_rev == "expenditure"
  definition_TRUE <- df[, names(df) == definitions[i]] == 1
  df_temp <- df[precondition_TRUE & definition_TRUE, ]
  
  
  ###'######################################################################
  ###'
  ###' Define a helper function
  ###' Collect the pre-defined functions
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
  ###' A for loop for dataframes with three factors
  ###'
  ###'
  
  start <- which(names(df) == "std_vs_nonstd")
  stop <- which(names(df) == "pupil_services")
  name_vec <- names(df)[start:stop]
  
  for (k in seq_along(name_vec)){
    
    # Rename the third factor
    names(df_temp)[names(df_temp) == name_vec[k]] <- "third_factor"
    
    # Apply the predefined functions
    temp <- aggregate_with_3functions(df_temp, third_factor)
    names(temp)[names(temp) == "third_factor"] <- name_vec[k]
    
    # Assign object name
    assign(paste0("df_", name_vec[k]), temp)
    
    # Rename back the original variable name
    names(df_temp)[names(df_temp) == "third_factor"] <- name_vec[k]
  }
  
  
  ###'######################################################################
  ###' 
  ###' Append to the data frames for collection
  ###' 
  ###' 
  
  for (l in seq_along(df_names)){
    
    # Append to the previous dataframe
    collect_df <- get(paste0("collect_", df_names[l]))
    current_df <- get(paste0("df_", df_names[l]))
    appended_df <- bind_rows(collect_df, current_df)
    
    # Assign names with df_name
    assign(paste0("collect_", df_names[l]), appended_df)
  }
  
  rm(collect_df)  # remove the temporary dataframe
  
  
  ###'######################################################################
  ###' 
  ###' End the loop over years and save the resulting dataframes as list
  ###' 
  ###' 
  
  ### Print iterations
  cat("Definition =", i, "/", "Fiscal Year =", year_chr, "\n", sep = " ")
  
  }   # End of loop over years (j)
  
  
  ### Save the resulting dataframes as list
  temp_list <- list()
  
  for (p in seq_along(df_names)){
    
    collect_df <- get(paste0("collect_", df_names[p]))
    
    temp_list[[p]] <- collect_df
  }
  
  names(temp_list) <- df_names  # Naming list elements
  
  assign(paste0("list_expenditures_def_", i), temp_list)
  
} # End of loop over definitions (i)



###'######################################################################
###'
###' Save the resulting lists
###'
###'

setwd(work_dir)

rm(list = setdiff(ls(), paste0("list_expenditures_def_", c(1, 2))))

save.image(file = "data/list_expenditures_def_1_2.RData")



