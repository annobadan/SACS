
###'######################################################################
###'
###' Data Visualization
###' 
###' (1) Change of Total Expenditure Per Average Daily Attendence
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



###'######################################################################
###'
###' Import the cleaned datasets
###' 
###'   

load("data/Total_Expenditures_PerADA_allyears.rda")

df <- df_exp_perADA_allyears  # assign brief name



###'######################################################################
###'
###' Call plot_trend functions
###' 
###'

setwd(work_dir)
source("code/functions_plot_trend.R")



###'######################################################################
###'
###' Remove districts with insufficient years of data
###' 
###' => Analyze only traditional schools in elementary, high, and unified 
###'    school districts that have been in continuous operation in California
###'    from 2003 through 2017
###'
###'

### Generate the table counting the years of operation
years_of_operation <- df %>% 
  group_by(Ccode, Dcode) %>%
  summarise(Dname = first(Dname), 
            Dtype = first(Dtype), 
            first = first(Fiscalyear), 
            last = last(Fiscalyear), 
            opr_years = n())

write.csv(years_of_operation, file = "table/years_of_operation.csv")


### Merge with the original data and filter only 14 years of operation
df_opr14 <- df %>%
  left_join(select(years_of_operation, -Dname, -Dtype), by = c("Ccode", "Dcode")) %>%
  filter(opr_years == 14)



###'######################################################################
###'
###' Plot #1-1.   
###' 
###' - Change of Total Expenditures per ADA
###' - Definition 1: All funds
###' - NOT inflation adjusted using the CPI-U deflator (in real 2016 dollars)  
###' 
###'

### Generate the data table for plotting
df_plot <- df_opr14 %>% 
  group_by(Fiscalyear) %>%
  summarise(mean_value = round(weighted.mean(D1_TotalExp_PP, K12ADA_C, na.rm = TRUE), 0))


### Plot 
p <- plot_trend_xy(df_plot, Fiscalyear, mean_value, yline = 2013, ylim = c(8000, 18000))
p <- p + 
  labs(title = "Total expenditures per student (Not inflation adjusted)",
       subtitle = "Definition 1 (All funds), Weighted averages by ADA", 
       caption = "Source: Annual Financial Data, California Department of Education", 
       y = "Average total expenditures per student (in dollars)",  
       x = "Fiscal Year")

filename <- c("Total Expenditure perADA_01_Definition1_Not inflation adjusted.pdf")
ggsave(paste0("figure/", filename, ".pdf"), p, width = 9, height = 5)



###'######################################################################
###'
###' Plot #1-2. Effect of the inflation adjustment  
###' 
###' - Change of Total Expenditures per ADA
###' - Definition 1: All funds
###' - Not inflation adjusted  VS. inflation adjusted 
###'   using the CPI-U deflator (in real 2016 dollars)  
###' 
###'

### Generate the data table for plotting
df_plot <- df_opr14 %>% 
  select(Fiscalyear, K12ADA_C, D1_TotalExp_PP, D1_TotalExp16_PP) %>%
  gather(key, value, -Fiscalyear, -K12ADA_C) %>%
  group_by(Fiscalyear, key) %>%
  summarise(mean_value = round(weighted.mean(value, K12ADA_C, na.rm = TRUE), 0))

df_plot$key <- factor(df_plot$key, levels = c("D1_TotalExp16_PP", "D1_TotalExp_PP"), 
                      labels = c("Inflation adjusted in real 2016 dollars", "Not adjusted"))


### Plot
p <- plot_trend_grp(df_plot, Fiscalyear, mean_value, key, 
                    yline = 2013, ylim = c(8000, 18000))
p <- p + 
  labs(title = "Effect of the inflation adjustment using CPI-U deflator on district per-pupil expenditures",
       subtitle = "Definition 1 (All funds), Weighted averages by ADA", 
       caption = "Source: Annual Financial Data, California Department of Education", 
       y = "Average total expenditures per student (in dollars)",  
       x = "Fiscal Year")

filename <- c("Total Expenditure perADA_02_Inflation adjusted vs not adjusted.pdf")
ggsave(paste0("figure/", filename, ".pdf"), p, width = 9, height = 6)



###'######################################################################
###'
###' Plot #1-3. Comparison based on definitions of total expenditures
###' 
###' - Change of Total Expenditures per ADA
###' 
###' - Compare the followings:
###'   (1) Definition 1 (All funds)
###'   (2) Definition 2 (General fund)
###'   (3) Current Expense of Education (General fund - more restricted?)
###'   
###' - inflation adjusted using the CPI-U deflator (in real 2016 dollars)  
###' 
###'

### Generate the data table for plotting
df_plot <- df_opr14 %>% 
  select(Fiscalyear, K12ADA_C, D1_TotalExp16_PP, D2_TotalExp16_PP, C_TotalExp16_PP) %>%
  gather(key, value, -Fiscalyear, -K12ADA_C) %>%
  group_by(Fiscalyear, key) %>%
  summarise(mean_value = round(weighted.mean(value, K12ADA_C, na.rm = TRUE), 0))

df_plot$key <- factor(df_plot$key, 
                      levels = c("D1_TotalExp16_PP", "D2_TotalExp16_PP", "C_TotalExp16_PP"), 
                      labels = c("Definition 1 (All Funds)", "Definition 2 (General Fund)", 
                                 "CDE definition (General Fund)"))


### Plot
p <- plot_trend_grp(df_plot, Fiscalyear, mean_value, key, 
                    yline = 2013, ylim = c(8000, 18000))
p <- p + 
  labs(title = "District per-pupil expenditures based on All funds vs. General funds",
       subtitle = "Inflation adjusted using the CPI-U deflator, Weighted averages by ADA",
       caption = "Source: Annual Financial Data, California Department of Education", 
       y = "Average total expenditures per student (in real 2016 dollars)",  
       x = "Fiscal Year")

filename <- c("Total Expenditure perADA_03_By definitions.pdf")
ggsave(paste0("figure/", filename, ".pdf"), p, width = 9, height = 6)



###'######################################################################
###'
###' Plot #1-4. Comparison based on (1) Definitions and (2) District Types
###' 
###' - Change of Total Expenditures per ADA
###' - Definition 1 (All funds) vs. Definition 2 (Only general funds)
###' - inflation adjusted using the CPI-U deflator (in real 2016 dollars) 
###' - Unified vs. Elementary vs. High school districts  
###' 
###'

### Generate the data table for plotting
df_plot <- df_opr14 %>% 
  select(Fiscalyear, K12ADA_C, Dtype, D1_TotalExp16_PP, D2_TotalExp16_PP) %>%
  filter(Dtype != "Comm Admin Dist") %>%
  gather(key, value, -Fiscalyear, -K12ADA_C, -Dtype) %>%
  group_by(Dtype, Fiscalyear, key) %>%
  summarise(mean_value = round(weighted.mean(value, K12ADA_C, na.rm = TRUE), 0))

df_plot$key <- factor(df_plot$key, levels = c("D1_TotalExp16_PP", "D2_TotalExp16_PP"), 
                      labels = c("Definition 1 (All Funds)", "Definition 2 (General Fund)"))

df_plot$Dtype <- factor(df_plot$Dtype, 
                        levels = c("UNIFIED", "ELEMENTARY", "HIGH"), 
                        labels = c("Unified", "Elementary", "High"))


### Plot
p <- plot_trend_grp_facet(df_plot, Fiscalyear, mean_value, 
                          key, .~Dtype, 
                          yline = 2013, ylim = c(8000, 18000))
p <- p +  
  labs(title = "District per-pupil expenditures by district type and definitions",
       subtitle = "Inflation adjusted using the CPI-U deflator, Weighted averages by ADA",
       caption = "Source: Annual Financial Data, California Department of Education", 
       y = "Average total expenditures per student (in real 2016 dollars)",  
       x = "Fiscal Year")

filename <- c("Total Expenditure perADA_04_By district types.pdf")
ggsave(paste0("figure/", filename, ".pdf"), p, width = 18, height = 6)



###'######################################################################
###'
###' Plot #1-5. Effect of the COE proration
###' 
###' - Change of Total Expenditures per ADA
###' - Definition 1 (All funds)
###' - inflation adjusted using the CPI-U deflator (in real 2016 dollars) 
###' - COE prorated vs. Not COE prorated 
###' 
###'

### Generate the data table for plotting
df_plot <- df_opr14 %>% 
  select(Fiscalyear, K12ADA_C, D1_TotalExp16_PP, D1_TotalExp16_PP_COE) %>%
  gather(key, value, -Fiscalyear, -K12ADA_C) %>%
  group_by(Fiscalyear, key) %>%
  summarise(mean_value = round(weighted.mean(value, K12ADA_C, na.rm = TRUE), 0))

df_plot$key <- factor(df_plot$key, levels = c("D1_TotalExp16_PP", "D1_TotalExp16_PP_COE"), 
                      labels = c("Not COE prorated", "COE prorated"))


### Plot
p <- plot_trend_grp(df_plot, Fiscalyear, mean_value, key, 
                    yline = 2013, ylim = c(8000, 18000))
p <- p + 
  labs(title = "Effect of the COE proration on district per-pupil expenditures - Definition 1 (All funds)",
       subtitle = "Inflation adjusted using the CPI-U deflator, Weighted averages by ADA", 
       caption = "Source: Annual Financial Data, California Department of Education", 
       y = "Average total expenditures per student (in real 2016 dollars)",  
       x = "Fiscal Year")

filename <- c("Total Expenditure perADA_05_Effect of the COE proration.pdf")
ggsave(paste0("figure/", filename, ".pdf"), p, width = 9, height = 6)

