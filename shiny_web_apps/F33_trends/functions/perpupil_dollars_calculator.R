
###'######################################################################
###'
###' Helper Functions to Calculate the District-level Per-Pupil Dollars
###' 
###' (1) aggregate_SACS_entries(): 
###'     Aggregate all SACS entries based on the group definition  
###' 
###' (2) perpupil_dollars_calculator():
###'     Merge with the ADA data and calculate per-pupil dollars 
###' 
###'  
###' 20180801 JoonHo Lee
###' 
###' 

### Package dependency
library(tidyverse)

### Function dependency: generate_categories
work_dir <- c("~/SACS")
setwd(work_dir)
source("functions/generate_categories.R")


### Data dependency: df_ADA_allyears
setwd(work_dir)
load(file = "processed_data/df_ADA_allyears.rda")



# ###'######################################################################
# ###'
# ###' Import data for testing codes
# ###'
# ###'
# 
# data_dir <- c("D:/Data/LCFF/Financial/Annual Financial Data")
# years <- paste0(sprintf("%02d",seq(3, 16)), sprintf("%02d",seq(4, 17)))
# i = 1
# year_chr <- years[i]
# setwd(paste0(data_dir, "/sacs", year_chr))
# load(file = "UserGL_merged.rda")
# df <- UserGL_merged
# df <- generate_categories(df)



###'######################################################################
###'
###' Define a simple helper function 
###' (1) Aggregate all SACS entries based on the group definition
###' 
###'

aggregate_SACS_entries <- function(df, ...){
  
  group_var <- quos(...)   # Enquote expressions
  
  df %>%
    group_by(!!!group_var) %>%
    summarise(
      # Fiscal year
      Fiscalyear = first(Fiscalyear), 
      
      # District information
      Dname = first(Dname),
      Dtype = first(Dtype), 
      
      # Aggregate at district-level
      sum_value = sum(Value, na.rm = TRUE)
    ) %>%
    select(Fiscalyear, !!!group_var, everything()) 
}

# ### Test the code
# df_temp <- aggregate_SACS_entries(df, Ccode, Dcode)



###'######################################################################
###'
###' Define a simple helper function
###' (2) Merge with the ADA data and calculate per-pupil dollars       
###'    
###'        

perpupil_dollars_calculator <- function(df, sum_value = sum_value){
  
  sum_value <- quo(sum_value)  # Enquote variable
  
  df %>%
    left_join(df_ADA_allyears, by = c("Fiscalyear", "Ccode", "Dcode")) %>%
    mutate(sum_value_PP = !!sum_value/K12ADA_C)
}

# ### Test the code
# df_temp <- perpupil_dollars_calculator(df_temp, sum_value)

