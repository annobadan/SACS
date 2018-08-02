
###'######################################################################
###'
###' Data Visualization
###' 
###' Expenditure visualization
###' 
###' 
###' 20180724 JoonHo Lee
###' 
###' 

###'######################################################################
###'
###' Basic settings
###'
###'

### Remove previous workspace
rm(list=ls())


### Set working directory 
work_dir <- c("~/SACS")
setwd(work_dir)


### Set data containing working directory
data_dir <- c("D:/Data/LCFF/Financial/Annual Financial Data")


### Call libraries
library(readxl)
library(foreign)
library(tidyverse)
library(scales)


### Import the cleaned datasets
load(file = "data/collect_dist_measures.RData")


### Call plot_trend functions
source("code/functions/plot_trend.R")



###'######################################################################
###'
###' Remove districts with insufficient years of data
###' 
###' => Analyze only traditional schools in elementary, high, and unified 
###'    school districts that have been in continuous operation in California
###'    from 2003 through 2017
###'
###'

### Import years_of_operation.csv 
setwd(work_dir)
load(file = "data/years_of_operation.rda")


### Merge with the original data and filter only 14 years of operation => function
df_opr14 <- df %>%
  left_join(select(years_of_operation, -Dname, -Dtype), by = c("Ccode", "Dcode")) %>%
  filter(opr_years == 14)



###'######################################################################
###'
###' Plot 6. Student vs. Non-Student Spending  
###' 
###' - Change of Total Expenditures per ADA
###' - Definition 1: All funds
###' - Not inflation adjusted  VS. inflation adjusted 
###'   using the CPI-U deflator (in real 2016 dollars)  
###' 
###'

### Generate the data table for plotting
df_plot <- collect_std_vs_nonstd %>% 
  group_by(Fiscalyear, std_vs_nonstd) %>%
  summarise(mean_value = round(weighted.mean(TotalExp_PP_16, K12ADA_C, na.rm = TRUE), 0))


### Plot
p <- plot_trend_grp(df_plot, Fiscalyear, mean_value, std_vs_nonstd, 
                    yline = 2013, ylim = c(0, 15000))
p <- p + 
  labs(title = "Student vs. Non-Student Spending",
       subtitle = "Definition 1 (All funds), Weighted averages by ADA", 
       caption = "Source: Annual Financial Data, California Department of Education", 
       y = "Average total expenditures per student (in real 2016 dollars)",  
       x = "Fiscal Year")

filename <- c("Total Expenditure per ADA_06_Student vs Non-Student Spending")
ggsave(paste0("figure/", filename, ".pdf"), p, width = 9, height = 6)



###'######################################################################
###'
###' Plot 7. Student Spending Subcategories
###' 
###' - Change of Total Expenditures per ADA
###' - Definition 1: All funds
###' - Not inflation adjusted  VS. inflation adjusted 
###'   using the CPI-U deflator (in real 2016 dollars)  
###' 
###'

### Generate the data table for plotting
df_plot <- collect_std_sub %>% 
  group_by(Fiscalyear, std_sub) %>%
  summarise(mean_value = round(weighted.mean(TotalExp_PP_16, K12ADA_C, na.rm = TRUE), 0))


### Plot
p <- plot_trend_grp(df_plot, Fiscalyear, mean_value, std_sub, 
                    yline = 2013, ylim = c(0, 8000))
p <- p + 
  labs(title = "Student Spending Subcategories",
       subtitle = "Definition 1 (All funds), Weighted averages by ADA", 
       caption = "Source: Annual Financial Data, California Department of Education", 
       y = "Average total expenditures per student (in real 2016 dollars)",  
       x = "Fiscal Year")

filename <- c("Total Expenditure per ADA_07_Student Spending Subcategories")
ggsave(paste0("figure/", filename, ".pdf"), p, width = 9, height = 9)



###'######################################################################
###'
###' Plot 8. Non-Student Spending Subcategories
###' 
###' - Change of Total Expenditures per ADA
###' - Definition 1: All funds
###' - Not inflation adjusted  VS. inflation adjusted 
###'   using the CPI-U deflator (in real 2016 dollars)  
###' 
###'

### Generate the data table for plotting
df_plot <- collect_nonstd_sub %>% 
  group_by(Fiscalyear, nonstd_sub) %>%
  summarise(mean_value = round(weighted.mean(TotalExp_PP_16, K12ADA_C, na.rm = TRUE), 0))


### Plot
p <- plot_trend_grp(df_plot, Fiscalyear, mean_value, nonstd_sub, 
                    yline = 2013, ylim = c(0, 2200))
p <- p + 
  labs(title = "Non-Student Spending Subcategories",
       subtitle = "Definition 1 (All funds), Weighted averages by ADA", 
       caption = "Source: Annual Financial Data, California Department of Education", 
       y = "Average total expenditures per student (in real 2016 dollars)",  
       x = "Fiscal Year")

filename <- c("Total Expenditure per ADA_08_Non-Student Spending Subcategories")
ggsave(paste0("figure/", filename, ".pdf"), p, width = 9, height = 9)



###'######################################################################
###'
###' Plot 9. Salaries Subcategories
###' 
###' - Change of Total Expenditures per ADA
###' - Definition 1: All funds
###' - Not inflation adjusted  VS. inflation adjusted 
###'   using the CPI-U deflator (in real 2016 dollars)  
###' 
###'

### Generate the data table for plotting
df_plot <- collect_salaries %>% 
  group_by(Fiscalyear, salaries) %>%
  summarise(mean_value = round(weighted.mean(TotalExp_PP_16, K12ADA_C, na.rm = TRUE), 0))


### Plot
p <- plot_trend_grp(df_plot, Fiscalyear, mean_value, salaries, 
                    yline = 2013, ylim = c(0, 5000))
p <- p + 
  labs(title = "Salaries Subcategories",
       subtitle = "Definition 1 (All funds), Weighted averages by ADA", 
       caption = "Source: Annual Financial Data, California Department of Education", 
       y = "Average total expenditures per student (in real 2016 dollars)",  
       x = "Fiscal Year")

filename <- c("Total Expenditure per ADA_09_Salaries Subcategories")
ggsave(paste0("figure/", filename, ".pdf"), p, width = 9, height = 9)



###'######################################################################
###'
###' Plot 10. Benefits Subcategories
###' 
###' - Change of Total Expenditures per ADA
###' - Definition 1: All funds
###' - Not inflation adjusted  VS. inflation adjusted 
###'   using the CPI-U deflator (in real 2016 dollars)  
###' 
###'

### Generate the data table for plotting
df_plot <- collect_benefits %>% 
  group_by(Fiscalyear, benefits) %>%
  summarise(mean_value = round(weighted.mean(TotalExp_PP_16, K12ADA_C, na.rm = TRUE), 0))


### Plot
p <- plot_trend_grp(df_plot, Fiscalyear, mean_value, benefits, 
                    yline = 2013, ylim = c(0, 1000))
p <- p + 
  labs(title = "Employee Benefits Subcategories",
       subtitle = "Definition 1 (All funds), Weighted averages by ADA", 
       caption = "Source: Annual Financial Data, California Department of Education", 
       y = "Average total expenditures per student (in real 2016 dollars)",  
       x = "Fiscal Year")

filename <- c("Total Expenditure per ADA_10_benefits Subcategories")
ggsave(paste0("figure/", filename, ".pdf"), p, width = 10, height = 9)


