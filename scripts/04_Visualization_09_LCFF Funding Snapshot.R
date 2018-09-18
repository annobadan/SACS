
###'######################################################################
###'
###' Data Visualization
###' 
###' (9) LCFF Funding Snapshot Data
###' 
###' FY1314, 1415, 1516, 1617, 1718
###' 
###' 
###' 20180902 JoonHo Lee
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
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import precleaned dataset
###'
###'

load("processed_data/funding_snapshot_13to17.rda")

df <- funding_snapshot_13to17



###'######################################################################
###'
###' Filter only school districts
###'
###'

df <- df %>%
  filter(LEA_type == "School District")



###'######################################################################
###'
###' Labels for plotting
###'
###'

labels <- labs(title = NULL,
               caption = "Source: LCFF Funding Snapshot", 
               y = "Per-pupil dollar amounts in real 2016 dollars",  
               x = "Fiscal Year") 



###'######################################################################
###'
###' LCFF Target Entitlement
###'
###'

### Filter only LCFF Target Entitlement
df_target <- df %>%
  filter(category == "LCFF Target Entitlement")


### Get weighted mean by Fiscal year
df_target_plot <- get_weighted_mean(df_target, Fiscalyear, value_PP_16, 
                                    weight = ADA_Total, variable)


### Remove Total LCFF Target Entitlement
df_target_plot <- df_target_plot %>%
  filter(variable != "Total LCFF Target Entitlement")


### Order levels of variable factor
df_target_plot$variable <- factor(df_target_plot$variable, 
                                  levels = c("Base Grant Funding", 
                                             "Supplemental Grant Funding", 
                                             "Concentration Grant Funding", 
                                             "Necessary Small Schools Allowance", 
                                             "Add-On Funding"))


### Plot
p <- plot_proportions_grp(df_target_plot, Fiscalyear, mean_value, variable)

p <- p + labels +
  labs(title = "LCFF Target Entitlement", 
       subtitle = NULL)

ggsave("figures/LCFF Target Entitlement.pdf", p, width = 8, height = 8)



###'######################################################################
###'
###' LCFF Target vs. LCFF Floor
###'
###'

### Filter only LCFF versus vs. LCFF Floor
df_versus <- df %>%
  filter(category == "LCFF Target VS. LCFF Floor")


### Get weighted mean by Fiscal year
df_versus_plot <- get_weighted_mean(df_versus, Fiscalyear, value_PP_16, 
                                    weight = ADA_Total, variable)


### Remove LCFF Target Entitlement
df_versus_plot <- df_versus_plot %>%
  filter(variable != "LCFF Target Entitlement")


### Order levels of variable factor
df_versus_plot$variable <- factor(df_versus_plot$variable, 
                                  levels = c("LCFF Floor Entitlement (If not funded at the LCFF Target)", 
                                             " Current Year Gap Funding (If not funded at the LCFF Target)", 
                                             "Remaining Need"))


### Plot
p <- plot_proportions_grp(df_versus_plot, Fiscalyear, mean_value, variable)

p <- p + labels +
  labs(title = "LCFF Target vs. LCFF Floor", 
       subtitle = NULL)

ggsave("figures/LCFF Target vs LCFF Floor.pdf", p, width = 8, height = 8)



###'######################################################################
###'
###' LCFF Funding Sources (Actual Funding)
###'
###'

### Filter only LCFF Funding Sources (Actual Funding)
df_actual <- df %>%
  filter(category == "LCFF Funding Sources (Actual Funding)")


### Get weighted mean by Fiscal year
df_actual_plot <- get_weighted_mean(df_actual, Fiscalyear, value_PP_16, 
                                    weight = ADA_Total, variable)


### Remove Total Funding
df_actual_plot <- df_actual_plot %>%
  filter(variable != "Total Funding")


### Order levels of variable factor
df_actual_plot$variable <- factor(df_actual_plot$variable, 
                                  levels = c("Local Revenue", 
                                             "Education Protection Account State Aid", 
                                             "LCFF State Aid Before MSA", 
                                             "Additional SA for MSA"))


### Plot
p <- plot_proportions_grp(df_actual_plot, Fiscalyear, mean_value, variable)

p <- p + labels +
  labs(title = "LCFF Funding Sources (Actual Funding)", 
       subtitle = NULL)

ggsave("figures/LCFF Funding Sources (Actual Funding).pdf", p, width = 8, height = 8)


