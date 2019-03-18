
###'######################################################################
###'
###' Building Plots for AERJ Publication
###' 
###' (3) Curricula Structure
###'   
###'   
###' < Figure 1 > %Always AG
###' < Figure 2 > %Not AG
###' < Figure 3 > %AP
###'  
###' 20190107 JoonHo Lee
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
                      "Figure_03_Curricula Structure")


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
###' High school ELA/Math A-G and AP classes
###'
###'

setwd(data_dir)

### (A) % Always AG - ELA
df <- tau_recoded_df(read.csv(file = "Row_01_PCT_Always_AG_ELA.csv"))

p1 <- event_study_xy(df, 
                     year, estimate, conf.low, conf.high, 
                     title = as.character(unique(df$y_label))) + facet_wrap(~ quantile) 


### (B) % Not AG - Math
df <- tau_recoded_df(read.csv(file = "Row_01_PCT_Not_AG_Math.csv"))

p2 <- event_study_xy(df, 
                     year, estimate, conf.low, conf.high, 
                     title = as.character(unique(df$y_label))) + facet_wrap(~ quantile) 


### (C) % AP - ELA
df <- tau_recoded_df(read.csv(file = "Row_01_PCT_AP_ELA.csv"))

p3 <- event_study_xy(df, 
                     year, estimate, conf.low, conf.high, 
                     title = as.character(unique(df$y_label))) + facet_wrap(~ quantile)


### Combine into a row
p_row2 <- plot_grid(p1, p2, p3, nrow = 1, labels = c("D", "E", "F"), align = 'h')

title_row2 <- ggdraw() + 
  draw_label("Schools received larger funding increases shrink the count of college-prep courses \n in proportion to growing number of elective courses", 
             fontface = 'bold')

p_row2_title <- plot_grid(title_row2, p_row2, 
                          ncol = 1, 
                          rel_heights = c(0.14, 1))  # rel_heights values control title margins


### Save as a pdf file
ggsave("Figure_02_Curricula Structure_01_AP and A-G.pdf", 
       p_row2_title, width = 13, height = 4.5)



###'######################################################################
###'
###' Combine with School average class size
###'
###'

setwd(file.path(work_dir, "figures", "20_AERJ Publication", 
                "Figure_02_Organizational Features"))

### (A) Elementary school: Homeroom class
df <- tau_recoded_df(read.csv(file = "Row_01_MN_size_SelfCon.csv"))

p1 <- event_study_xy(df, 
                     year, estimate, conf.low, conf.high, 
                     title = as.character(unique(df$y_label))) + facet_wrap(~ quantile) 


### (B) High school: ELA class
df <- tau_recoded_df(read.csv(file = "Row_01_MN_size_ELA.csv"))

p2 <- event_study_xy(df, 
                     year, estimate, conf.low, conf.high, 
                     title = as.character(unique(df$y_label))) + facet_wrap(~ quantile) 


### (C) High school: Math class
df <- tau_recoded_df(read.csv(file = "Row_01_MN_size_Math.csv"))

p3 <- event_study_xy(df, 
                     year, estimate, conf.low, conf.high, 
                     title = as.character(unique(df$y_label))) + facet_wrap(~ quantile)


### Combine into a row
p_row1 <- plot_grid(p1, p2, p3, nrow = 1, labels = c("A", "B", "C"), align = 'h')

title_row1 <- ggdraw() + 
  draw_label("The infusion of new LCFF dollars explains declines in school average class size", 
             fontface = 'bold')

p_row1_title <- plot_grid(title_row1, p_row1, 
                          ncol = 1, 
                          rel_heights = c(0.1, 1))  # rel_heights values control title margins


### Save as a pdf file
ggsave("Figure_02_Organizational Features_01_Class size reduction.pdf", 
       p_row1_title, width = 13, height = 4.5)



###'######################################################################
###'
###' High school
###' 
###' - Combine 
###'   (1) p_row1_title: Class size
###'   (2) p_row2_title: A-G and AP
###'
###'

setwd(data_dir)

final_plot_grid <- plot_grid(p_row1_title,
                             p_row2_title, 
                             ncol = 1, 
                             align = 'v', axis = 'l', 
                             rel_heights = c(1, 1))

ggsave("Figure_02_Organizational Features_and_Curricula Structure.pdf", 
       final_plot_grid, width = 13, height = 9)

