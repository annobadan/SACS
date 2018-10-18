
###'######################################################################
###'
###' Data Visualization
###' 
###' Change of School-level Student Compositions
###' 
###' 
###' 20181009 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character/Enrollment")


### Call libraries
library(tidyverse)
library(foreign)
library(rlang)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import the prepared dataset
###'
###'

load(file = "processed_data/df_student_composition_0317_Traditional.rda")



###'######################################################################
###'
###' Prepare a for loop
###'
###'

### Variable names to investigate
var_names <- c("Total_Enroll", 
               "PCT_male", "PCT_female", 
               "PCT_White", "PCT_Latino", "PCT_Black", "PCT_Asian", 
               "PCT_Filipino", "PCT_Pacific", "PCT_Native", 
               "PCT_Free", "PCT_FRPM")


### Elements for ggplot
title_vec <- c("Total School Enrollment", 
               "% Male Students", "% Female Students", 
               "% White Students", "% Hispanic/Latino Students", 
               "% African American Students", 
               "% Asian Students", 
               "% Filipino Students", 
               "% Pacific Islander Students", 
               "% American Indian or Alaska Native Students", 
               "% Students Eligible for Free Meals", 
               "% Student Eligible for Free or Reduced Price Meals (FRPM)")

subtitle <- "By School Type and Poverty Level (% FRPM)"

xlab <- "Academic Year"

ylab <- "Average Weighted by Total School Enrollment"



###'######################################################################
###'
###' Implement the for loop
###'
###'

for (i in seq_along(var_names)){
  
  ### Assign variable name
  df_temp <- df %>%
    mutate(AcademicYear = AcademicYear + 2000)
  
  idx <- which(names(df_temp) == var_names[i])
  names(df_temp)[idx] <- "variable"
  
  
  ###' Generate data to plot
  ###' SOC by Poverty Level
    
  df_plot <- df_temp %>%
    group_by(AcademicYear, SOC, PCT_FRPM_15bin) %>%
    summarise(mean_value = round(mean(variable, na.rm = TRUE), 2)) 
    

  df_plot <- df_plot %>%
    filter(SOC != "K-12 Schools")
  
  
  ### Plot!
  p1 <- plot_trend_grp_facet(df_plot, 
                             x = AcademicYear, 
                             y = mean_value, 
                             group = PCT_FRPM_15bin, 
                             facet_formula = . ~ SOC, 
                             yline = 2013, 
                             ylim = auto_ylim(df_plot$mean_value)) + 
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  
  
  labels <- labs(title = paste0(title_vec[i], " 2003-2017"), 
                 subtitle = subtitle, 
                 caption = NULL, 
                 x = xlab, y = ylab)
  
  p2 <- p1 + labels 
  
  
  ### Save as pdf file
  ggsave(paste0("figures/", "Student_Composition_",
                sprintf("%02d", i), "_", var_names[i],".pdf"), p2, 
         width = 17, height = 9)
  
}

