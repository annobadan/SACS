
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


###' (3) Calculate the y-limits and height for the PDF file

auto_ylim <- function(value_vec = NULL){
  
  ### The optimal y-limits
  bottom <- min(value_vec) - (min(value_vec) - 0)/5
  ceiling <- max(value_vec) + (min(value_vec) - 0)/5
  
  ### Return objects
  auto_ylim <- c(bottom, ceiling)
  return(auto_ylim)
}

auto_height <- function(factor_vec = NULL){

  ### The height for the PDF file
  num_factor <- length(levels(factor_vec))
  height <- ifelse(num_factor <= 4, 6, num_factor + 3)
  
  ### Return objects
  return(height)
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

for (i in seq_along(df_names)){              # (1) Loop over variables
  
  for (j in seq_along(sub_definition_vec)){  # (2) Loop over definitions
    
    ### Extract the dataframe from the list
    datalist <- get(paste0("list_expenditures_def_", j))
    idx <- which(names(datalist) == df_names[i])
    df <- datalist[[idx]]

        
    ### Sort out districts with continuous operation
    df <- operation14(df)
    
    
    ### Plot
    
    if (i == 1){  # (1) Total Expenditure: "zero" factor
      
      ### Plot 1: with "0" factor, no facet
      df_plot <- get_weighted_mean(df, Fiscalyear, sum_value_PP_16, K12ADA_C)
      
      p1 <- plot_trend_xy(df_plot, Fiscalyear, mean_value, yline = 2013, 
                          ylim = auto_ylim(df_plot$mean_value))
      
      
      ### Plot 2: factoring with District type
      df_plot <- get_weighted_mean(df, Fiscalyear, sum_value_PP_16, K12ADA_C, Dtype)
      df_plot %>% filter(Dtype != "Comm Admin Dist") -> df_plot
      
      p2 <- plot_trend_grp(df_plot, Fiscalyear, mean_value, Dtype, yline = 2013, 
                           ylim = auto_ylim(df_plot$mean_value))
      
      
      ### Labels
      labels <- labs(title = title_vec[i],
                     subtitle = paste0(sub_definition_vec[j], ", ", sub_weighted), 
                     caption = caption, 
                     y = ylab,  
                     x = xlab) 
      
      p1 <- p1 + labels
      p2 <- p2 + labels + labs(title = paste0(title_vec[i], " by District Type"))
      
      
      ### Save as pdf file
      filename_p1 <- paste0("Expenditures_", "Definition", j, "_", 
                            sprintf("%02d", i), "_", title_vec[i])
      filename_p2 <- paste0(filename_p1, "_by District Type")
      
      ggsave(paste0("figure/", filename_p1, ".pdf"), p1, width = 9, height = 6)
      ggsave(paste0("figure/", filename_p2, ".pdf"), p2, width = 9, height = 6)      
      
      
    } else {   # (2) Other plots with one factor
      
      ### Plot 1: with one factor, zero facet
      names(df)[names(df) == df_names[i]] <- "factor"
      df_plot <- get_weighted_mean(df, Fiscalyear, sum_value_PP_16, K12ADA_C, factor)
      
      p1 <- plot_trend_grp(df_plot, Fiscalyear, mean_value, factor, yline = 2013, 
                           ylim = auto_ylim(df_plot$mean_value))
      
      
      ### Plot 2: with one factor, facet with District Type
      df_plot <- get_weighted_mean(df, Fiscalyear, sum_value_PP_16, K12ADA_C, factor, Dtype)
      df_plot %>% filter(Dtype != "Comm Admin Dist") -> df_plot
      
      p2 <- plot_trend_grp_facet(df_plot, Fiscalyear, mean_value, factor, .~ Dtype,  
                                 yline = 2013, ylim = auto_ylim(df_plot$mean_value))
      
      ### Labels
      labels <- labs(title = title_vec[i],
                     subtitle = paste0(sub_definition_vec[j], ", ", sub_weighted), 
                     caption = caption, 
                     y = ylab,  
                     x = xlab) 
      
      p1 <- p1 + labels
      p2 <- p2 + labels + labs(title = paste0(title_vec[i], " by District Type"))
      
      
      ### Save as pdf file
      filename_p1 <- paste0("Expenditures_", "Definition", j, "_", 
                            sprintf("%02d", i), "_", title_vec[i])
      filename_p2 <- paste0(filename_p1, "_by District Type")
      
      ggsave(paste0("figure/", filename_p1, ".pdf"), p1, 
             width = 9, height = auto_height(df_plot$factor))
      ggsave(paste0("figure/", filename_p2, ".pdf"), p2, 
             width = 18, height = auto_height(df_plot$factor))  
    }
  }  # End of loop over definitions
}    # End of loop over variables


