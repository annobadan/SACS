
###'######################################################################
###'
###' Data Visualization
###' 
###' (1) Change of Expenditures per ADA
###' 
###' 
###' 20180806 JoonHo Lee
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
###' Import the prepared dataset
###'
###'

### Expenditures per ADA
load(file = "data/list_expenditures_def_1_2.RData")



###'######################################################################
###'
###' Define helper functions
###'
###'

###' (1) operation14(): Remove districts with insufficient years of data
###' 
###' => Analyze only traditional schools in elementary, high, and unified 
###'    school districts that have been in continuous operation (14 years) 
###'    in California from 2003 through 2017

setwd(work_dir)
load(file = "data/years_of_operation.rda")   # Data dependency: years_of_operation.csv

operation14 <- function(df){
  df %>%
    left_join(select(years_of_operation, -Dname, -Dtype), by = c("Ccode", "Dcode")) %>%
    filter(opr_years == 14)
}


###' (2) get_weighted_mean(): Get weighted district averages

get_weighted_mean <- function(df, 
                              x = Fiscalyear, 
                              y = sum_value_PP_16, 
                              weight = K12ADA_C, 
                              ...){
  
  ### Enquote variables
  x <- enquo(x)
  y <- enquo(y)
  weight <- enquo(weight)
  group_var <- quos(...)
  
  df %>% 
    group_by(!!x, !!!group_var) %>%
    summarise(mean_value = round(weighted.mean(!!y, !!weight, na.rm = TRUE), 0))
}



###'######################################################################
###'
###' Prepare a for loop
###'
###'

### Names for the dataframes to collect
df_names <- c("total_exp", 
              "std_vs_nonstd", "nonstd_sub", "std_sub", "salaries", "benefits", 
              "goals", "general_Ed", "supp_Ed", "SPED", "functions", "SPED_inst", 
              "pupil_services")


### Elements for ggplot
title_vec <- c("Total Expenditure per ADA", 
               "Student vs. Non-Student Spending", 
               "Non-Student Spending", 
               "Student Spending (by Object codes)", 
               "Salaries", 
               "Employee Benefits", 
               "Student Spending (by Goal codes)", 
               "General Education, K-12", 
               "Supplemental Education, K-12", 
               "Special Education", 
               "Student Spending (by Function codes)", 
               "Special Education Instruction", 
               "Pupil Services")

Dtype_vec <- c("Elementary", "High", "Unified")

sub_definition_vec <- c("Definition 1 (All funds)", 
                        "Definition 2 (General fund only)")

sub_weighted <- c("District Averages Weighted by ADA")

caption <- "Source: Annual Financial Data, California Department of Education"

ylab <- "Average Expenditures Per Student (in Real 2016 Dollars)"

xlab <- "Fiscal Year"



###'######################################################################
###'
###' Implement the for loop
###'
###'

for (i in seq_along(df_names)){
  
  ### Extract the dataframe from the list
  idx_list <- which(names(list_expenditures_def_1) == df_names[i])
  df <- list_expenditures_def_1[[idx_list]]
  
  
  ### Sort out districts with continuous operation
  df <- operation14(df)
  
  
  ### Get weighted averages
  if (i == 1){
    df_plot <- get_weighted_mean(df, Fiscalyear, sum_value_PP_16, K12ADA_C)
    
    ### Calculate the optimal y-limits
    num_vec <- df_plot$mean_value
    bottom <- min(num_vec) - (min(num_vec) - 0)/5
    ceiling <- max(num_vec) + (min(num_vec) - 0)/5
    
    ### Plot 
    p <- plot_trend_xy(df_plot, Fiscalyear, mean_value, yline = 2013, 
                       ylim = c(bottom, ceiling))
  } else {
    names(df)[names(df) == df_names[i]] <- "factor"
    df_plot <- get_weighted_mean(df, Fiscalyear, sum_value_PP_16, K12ADA_C, factor)
    
    ### Calculate the optimal y-limits
    num_vec <- df_plot$mean_value
    bottom <- min(num_vec) - (min(num_vec) - 0)/5
    ceiling <- max(num_vec) + (min(num_vec) - 0)/5

    ### Plot 
    p <- plot_trend_grp(df_plot, Fiscalyear, mean_value, factor, yline = 2013, 
                        ylim = c(bottom, ceiling))
    
    ### Calculate the optimal height for the pdf file
    num_factor <- length(levels(df_plot$factor))
    height <- ifelse(num_factor <= 4, 6, num_factor + 3)
  }
  
  ### Labels
  p <- p + 
    labs(title = title_vec[i],
         subtitle = paste0(sub_definition_vec[1], ", ", sub_weighted), 
         caption = caption, 
         y = ylab,  
         x = xlab)
  
  ### Save as pdf file
  filename <- paste0("Expenditures_", sprintf("%02d", i), "_", title_vec[i])
  
  ggsave(paste0("figure/", filename, ".pdf"), p, 
         width = 9, height = ifelse(i == 1, 6, height))
}  # End of loop over df_names


