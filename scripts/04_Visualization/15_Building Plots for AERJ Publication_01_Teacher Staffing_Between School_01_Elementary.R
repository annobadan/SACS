
###'######################################################################
###'
###' Building Plots for AERJ Publication
###' 
###' (1) Teacher Staffing - Between-school teacher sorting
###' 
###' 
###' < Figure 1-1 >: Districts attracted new teachers with new money
###' - % New in District: Attract teachers 
###' - Mean Years in district: Mirror (A)
###' - % Novice teachers: Many of them are novice
###' 
###' 
###' < Figure 1-2 >: Newly hired teachers are mostly non-tenured employment  
###' - % Tenured
###' - % Long-term substitutes
###' - % Probationary
###' - % Other non-tenured employment status  
###' 
###' < Figure 2-1 >: Poor schools hired more white teachers
###' - % White Q20, Q50, Q80
###'  
###' < Figure 2-2 >: Poor schools hired more teachers holding a master's degree or above
###' - % MasterAbove Q20, Q50, Q80
###'  
###'  
###' 20190104 JoonHo Lee
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


### Set a directory containing result data files
data_dir <- file.path(work_dir, "figures", "20_AERJ Publication", 
                      "Figure_01_Teacher Staffing_01_Between School")
  

### Call libraries
library(tidyverse)
library(cowplot)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Define functions for plotting
###'
###'

setwd(work_dir)

source(file = "scripts/04_Visualization/14_Define helper functions for building plots for AERJ publication.R")



###'######################################################################
###'
###' Elementary
###' 
###' "Districts and schools hired rising counts of new teachers"
###' 
###' - % New in District: Attract teachers 
###' - Mean Years in district: Mirror (A)
###' - % Novice teachers: Many of them are novice
###' 
###'

setwd(data_dir)

### (A) % New in District
df <- tau_recoded_df(read.csv(file = "Row_01_PCT_New_in_dist.csv"))

p1 <- event_study_xy(df, 
                     year, estimate, conf.low, conf.high, 
                     title = as.character(unique(df$y_label))) + facet_wrap(~ quantile) 


### (B) Mean Years in district
df <- tau_recoded_df(read.csv(file = "Row_01_mean_yrs_in_dist.csv"))

p2 <- event_study_xy(df, 
                     year, estimate, conf.low, conf.high, 
                     title = as.character(unique(df$y_label))) + facet_wrap(~ quantile) 


### (C) % Novice teachers
df <- tau_recoded_df(read.csv(file = "Row_01_PCT_New_teach.csv"))

p3 <- event_study_xy(df, 
                     year, estimate, conf.low, conf.high, 
                     title = as.character(unique(df$y_label))) + facet_wrap(~ quantile)


### Combine into a row
p_row1 <- plot_grid(p1, p2, p3, nrow = 1, labels = c("A", "B", "C"), align = 'h')

title_row1 <- ggdraw() + 
  draw_label("Districts and schools hired rising counts of new teachers", fontface = 'bold')

p_row1_title <- plot_grid(title_row1, p_row1, 
                          ncol = 1, 
                          rel_heights = c(0.1, 1))  # rel_heights values control title margins


### Save as a pdf file
ggsave("Figure_01_Teacher Staffing_Between_01_Hired new teachers_Elementary.pdf", 
       p_row1_title, width = 13, height = 4.5)



###'######################################################################
###'
###' Elementary
###' 
###' "Districts first relied on long-term substitutes, then on non-tenured teachers"
###' 
###' - % Long-term substitutes 
###' - % Probationary
###' - % Tenured
###' 
###'

###' % Long-term substitutes
df <- tau_recoded_df(read.csv(file = "Row_04_PCT_Longterm_Sub.csv"))

p1 <- event_study_xy(df, 
                     year, estimate, conf.low, conf.high, 
                     title = as.character(unique(df$y_label))) + facet_wrap(~ quantile)


###' % Probationary
df <- tau_recoded_df(read.csv(file = "Row_04_PCT_Probationary.csv"))

p2 <- event_study_xy(df, 
                     year, estimate, conf.low, conf.high, 
                     title = as.character(unique(df$y_label))) + facet_wrap(~ quantile)



###' % Tenured
df <- tau_recoded_df(read.csv(file = "Row_04_PCT_Tenured.csv"))

p3 <- event_study_xy(df, 
                     year, estimate, conf.low, conf.high, 
                     title = as.character(unique(df$y_label))) + facet_wrap(~ quantile) 


###' % Other non-tenured employment status  
df <- tau_recoded_df(read.csv(file = "Row_04_PCT_Other.csv"))

p4 <- event_study_xy(df, 
                     year, estimate, conf.low, conf.high, 
                     title = as.character(unique(df$y_label))) + facet_wrap(~ quantile)


### Combine into a row
p_row2 <- plot_grid(p1, p2, p3, nrow = 1, labels = c("D", "E", "F"), align = 'h')

title_row <- ggdraw() + 
  draw_label("Districts first relied on long-term substitutes, then on non-tenured teachers", 
             fontface = 'bold')

p_row2_title <- plot_grid(title_row, p_row2,  
                          ncol = 1, 
                          rel_heights = c(0.1, 1))  # rel_heights values control title margins


### Save as a pdf file
ggsave("Figure_01_Teacher Staffing_Between_02_Hired non-tenured teachers_Elementary.pdf", 
       p_row2_title, width = 13, height = 4.5)



###'######################################################################
###'
###' Elementary
###' 
###' - Combine 
###'   (1) p_row1_title: New teachers
###'   (2) p_row2_title: Non-tenured teachers
###'
###'

final_plot_grid <- plot_grid(p_row1_title,
                             p_row2_title, 
                             ncol = 1, 
                             align = 'v', axis = 'l', 
                             rel_heights = c(1, 1))

ggsave("Figure_01_Teacher Staffing_Between_03_Hired new and non-tenured teachers_Elementary.pdf", 
       final_plot_grid, width = 13, height = 9)



###'######################################################################
###'
###' Elementary
###' 
###' "High-poverty schools hired more white teachers and 
###'  teachers holding a master's degree or above"
###' 
###' - % White Q20, (Q50), Q80
###' - % Master Above Q20, (Q50), Q80
###'
###'

### % White (WITH Q50)
df <- tau_recoded_df(read.csv(file = "Row_02_PCT_White_Tch.csv"))

p <- event_study_xy(df, 
                    year, estimate, conf.low, conf.high, 
                    title = as.character(unique(df$y_label))) + facet_wrap(~ quantile) 

p_row3 <- plot_grid(p, nrow = 1, labels = c("A"), align = 'h')  # combine into a row


### % White (WITHOUT Q50)
df <- tau_recoded_df(read.csv(file = "Row_02_PCT_White_Tch.csv")) %>%
  filter(element != "tau50")

p <- event_study_xy(df, 
                    year, estimate, conf.low, conf.high, 
                    title = as.character(unique(df$y_label))) + facet_wrap(~ quantile) 

p_row3_woQ50 <- plot_grid(p, nrow = 1, labels = c("A"), align = 'h')  # combine into a row


### % Master Above (WITH Q50)
df <- tau_recoded_df(read.csv(file = "Row_03_PCT_Master_Plus.csv"))

p <- event_study_xy(df, 
                    year, estimate, conf.low, conf.high, 
                    title = as.character(unique(df$y_label))) + facet_wrap(~ quantile) 

p_row4 <- plot_grid(p, nrow = 1, labels = c("B"), align = 'h')  # combine into a row


### % Master Above (WITHOUT Q50)
df <- tau_recoded_df(read.csv(file = "Row_03_PCT_Master_Plus.csv")) %>%
  filter(element != "tau50")

p <- event_study_xy(df, 
                    year, estimate, conf.low, conf.high, 
                    title = as.character(unique(df$y_label))) + facet_wrap(~ quantile) 

p_row4_woQ50 <- plot_grid(p, nrow = 1, labels = c("B"), align = 'h')  # combine into a row



###'######################################################################
###'
###' Elementary
###' 
###' - Combine 
###'   (1) p_row3: White teachers
###'   (2) p_row4: Teachers holding a master's degree or above
###'
###'

### Title row
title_row <- ggdraw() + 
  draw_label("High-poverty schools hired more white teachers and teachers holding a master's degree or above", 
             fontface = 'bold')

title_row_woQ50 <- ggdraw() + 
  draw_label("High-poverty schools hired more white teachers and \n teachers holding a master's degree or above", 
             fontface = 'bold')


### WITH Q50
final_plot_grid <- plot_grid(title_row, 
                             p_row3,
                             p_row4, 
                             ncol = 1, 
                             align = 'v', axis = 'l', 
                             rel_heights = c(0.1, 1, 1))

ggsave("Figure_01_Teacher Staffing_Between_04_Hired White Master Above teachers_Elementary.pdf", 
       final_plot_grid, width = 13, height = 9)


### WITHOUT Q50
final_plot_grid <- plot_grid(title_row_woQ50, 
                             p_row3_woQ50,
                             p_row4_woQ50, 
                             ncol = 1, 
                             align = 'v', axis = 'l', 
                             rel_heights = c(0.12, 1, 1))

ggsave("Figure_01_Teacher Staffing_Between_04_Hired White Master Above teachers_Elementary_WithoutQ50.pdf", 
       final_plot_grid, width = 8.6, height = 8)
