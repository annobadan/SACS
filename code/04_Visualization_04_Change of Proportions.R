
###'######################################################################
###'
###' Data Visualization
###' 
###' (4) Change of Proportions
###' 
###' 
###' 20180808 JoonHo Lee
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


### Revenues per ADA
load(file = "data/list_revenues.rda")



###'######################################################################
###'
###' Prepare a for loop
###'
###'

### Names for the dataframes to collect
df_names <- c("std_vs_nonstd", "nonstd_sub", "std_sub", "salaries", "benefits", 
              "goals", "general_Ed", "supp_Ed", "SPED", "functions", "SPED_inst", 
              "pupil_services")

### Elements for ggplot
title_vec <- c("Student vs. Non-Student Spending", 
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

sub_weighted <- c("Proportions Based on District Averages Weighted by ADA")

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
    
    
    ###'######################################################################
    ###'
    ###' Plot 1: with one factor, zero facet
    ###' 
    ###' 
    
    ### Generate data for plotting
    names(df)[names(df) == df_names[i]] <- "factor"
    df_plot <- get_weighted_mean(df, Fiscalyear, sum_value_PP_16, K12ADA_C, factor)
    df_plot <- group_percent(df_plot, mean_value, Fiscalyear)
    group_total <- df_plot %>% 
      group_by(Fiscalyear) %>% 
      summarise(group_sum = first(group_sum))
    
    
    ### Plot
    p1 <- ggplot(df_plot, aes(x = Fiscalyear, y = mean_value, fill = factor)) +
      geom_bar(position = position_stack(), stat = "identity", width = 0.7) +
      geom_text(aes(label = label_text), position = position_stack(vjust = 0.5), size = 3) + 
      geom_text(data = group_total,  
                aes(x = Fiscalyear, y = group_sum + mean(df_plot$group_sum)/30, 
                    label = comma(group_sum), fill = NULL), 
                size = 3) + 
      theme_trend + 
      scale_x_continuous(breaks = seq(min(df_plot$Fiscalyear), 
                                      max(df_plot$Fiscalyear), 
                                      by = 1))
    
    
    ###'######################################################################
    ###'
    ###' Plot 2: with one factor, facet with District Type
    ###' 
    ###' 
    
    ### Generate data for plotting
    df_plot <- get_weighted_mean(df, Fiscalyear, sum_value_PP_16, K12ADA_C, factor, Dtype)
    df_plot %>% filter(Dtype != "Comm Admin Dist") -> df_plot
    
    df_plot <- group_percent(df_plot, mean_value, Fiscalyear, Dtype)
    group_total <- df_plot %>% group_by(Fiscalyear, Dtype) %>% summarise(group_sum = first(group_sum))
    
    
    ### Plot
    p2 <- ggplot(df_plot, aes(x = Fiscalyear, y = mean_value, fill = factor)) +
      geom_bar(position = position_stack(), stat = "identity", width = 0.7) +
      geom_text(aes(label = label_text), position = position_stack(vjust = 0.5), size = 3) + 
      geom_text(data = group_total,  
                aes(x = Fiscalyear, y = group_sum + mean(df_plot$group_sum)/30, 
                    label = comma(group_sum), fill = NULL), 
                size = 3) + 
      facet_grid(.~Dtype) + 
      theme_trend + 
      scale_x_continuous(breaks = seq(min(df_plot$Fiscalyear), 
                                      max(df_plot$Fiscalyear), 
                                      by = 1))
    
    
    ###'######################################################################
    ###'
    ###' Label and Save the resulting plots
    ###'
    ###'
    
    ### Labels
    labels <- labs(title = title_vec[i],
                   subtitle = paste0(sub_definition_vec[j], ", ", sub_weighted), 
                   caption = caption, 
                   y = ylab,  
                   x = xlab) 
    
    p1 <- p1 + labels
    p2 <- p2 + labels + labs(title = paste0(title_vec[i], " by District Type"))
    
    
    ### Save as pdf file
    filename_p1 <- paste0("Proportions_Expenditures_", "Definition", j, "_", 
                          sprintf("%02d", i), "_", title_vec[i])
    filename_p2 <- paste0(filename_p1, "_by District Type")
    
    ggsave(paste0("figure/", filename_p1, ".pdf"), p1, 
           width = 9, height = auto_height(df_plot$factor))
    ggsave(paste0("figure/", filename_p2, ".pdf"), p2, 
           width = 20, height = auto_height(df_plot$factor)) 
      
 
  }  # End of loop over definitions
}    # End of loop over variables


