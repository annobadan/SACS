
###'######################################################################
###'
###' Generate State-level predictors: Statewide Expenditures
###' 
###' - To use in the district per-pupil revenue prediction model
###' 
###' 
###' 20180903 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF")


### Call libraries
library(tidyverse)
library(lme4)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Load the precleaned datasets
###' 
###'

### Per-pupil expenditures & revenues
load(file = "processed_data/list_expenditures_def_1_2.RData")
load(file = "processed_data/list_revenues.rda")


### FY1314 Simuated IV
load(file = "processed_data/df_1314_SimIV.rda")


### The State of California's overall and local revenues/expenditures data
state_df <- read.csv(file = "processed_data/May_Revision_Budget_2000_to_2016.csv")



###'######################################################################
###'
###' Total state-level ADA across years 2003-2016
###'
###'

### Aggregate K12ADA_C to state level

names(list_revenues)

state_ADA_df <- list_revenues[["total_rev"]] %>%
  group_by(Fiscalyear) %>%
  summarise(state_ADA = sum(K12ADA_C, na.rm = TRUE)) 


###' Plot trend over years 2003-2016
###' Plot scales in thousand

state_ADA_df <- state_ADA_df %>%
  mutate(state_ADA_thousand = round(state_ADA/1000, 0))

p <- plot_trend_xy(state_ADA_df, x = Fiscalyear, y = state_ADA_thousand, 
                   yline = 2013, ylim = c(5000, 6000)) + 
  labs(title = "Total K-12 Average Daily Attendance in California, 2003-2016", 
       subtitle = NULL, 
       x = "Fiscal Year", 
       y = "Total K-12 ADA (in thousand)", 
       caption = "Source: California Department of Education")

ggsave("figures/California Total K-12 ADA.pdf", p, width = 8, height = 6)



###'######################################################################
###'
###' Generate the state-level predictor:
###' 
###' (1) CAExp: the total non-K-12 (or non-education) state expenditures 
###'            per pupil for birth cohort b
###'            
###' (2) CALocalAssist: the state local assistance provided (excluding education)
###'                    per pupil for birth cohort b
###'  
###' *** Why divide by State ADA?
###' 
###' => Change in state expenditure might reflect change in state population.
###'    We would like to rule out change in state expenditures due to change in population.
###'    State ADA is a good proxy of state population. 
###' 
###' => Patterns however are consistent with and without the division.
###' 
###' 

### Generate CAExp: the total non-K-12 (or non-education) state expenditures

state_operations_df <- state_df %>%
  filter(Category_1 == "State Operations") %>%
  filter(!(Category_2 %in% c("Education"))) %>%
  group_by(Fiscalyear) %>%
  summarise(CAExp = sum(value_16, na.rm = TRUE)) %>%
  left_join(state_ADA_df, by = c("Fiscalyear")) %>%
  mutate(CAExp_PP = round(CAExp/state_ADA_thousand, 0), 
         CAExp_thousand = round(CAExp/1000, 0)) %>%
  filter(!is.na(CAExp_PP))


### Generate CALocalAssist: the state local assistance provided (excluding education)

local_assistance_df <- state_df %>%
  filter(Category_1 == "Local Assistance") %>%
  filter(!(Category_2 %in% c("Public Schools − K−12", 
                             "Contributions to State Teachers' Retirement System", 
                             "Community Colleges", 
                             "School Facilities Aid", 
                             "Other Education"))) %>%
  group_by(Fiscalyear) %>%
  summarise(CALocalAssist = sum(value_16, na.rm = TRUE)) %>%
  left_join(state_ADA_df, by = c("Fiscalyear")) %>%
  mutate(CALocalAssist_PP = round(CALocalAssist/state_ADA_thousand, 0), 
         CALocalAssist_thousand = round(CALocalAssist/1000, 0)) %>%
  filter(!is.na(CALocalAssist_PP))



###'######################################################################
###'
###' Plot CAExp and CALocalAssist: 01. Total in Thousands
###'
###'

### Plot CAExp: Total in thousands
p <- plot_trend_xy(state_operations_df, x = Fiscalyear, y = CAExp_thousand, 
                   yline = 2013, ylim = c(10000, 30000)) + 
  labs(title = "Total Non-Education State Expenditures: Total in thousands", 
       subtitle = "Inflation adjusted using CPI-U deflator (in real 2016 dollars)",  
       x = "Fiscal Year", 
       y = "Dollars in thousands", 
       caption = "Source: The State Controller's Office")

ggsave("figures/State Operations_Non-Education Expenditures_01_Total in thousands.pdf", p, 
       width = 8, height = 6)


### Plot CALocalAssist: Total in thousands
p <- plot_trend_xy(local_assistance_df, x = Fiscalyear, y = CALocalAssist_thousand, 
                   yline = 2013, ylim = c(50000, 90000)) + 
  labs(title = "State Local Assistance Provided Excluding Education: Total in thousands", 
       subtitle = "Inflation adjusted using CPI-U deflator (in real 2016 dollars)",  
       x = "Fiscal Year", 
       y = "Dollars in thousands", 
       caption = "Source: The State Controller's Office")

ggsave("figures/Local_Assistance_Non-Education Expenditures_01_Total in thousands.pdf", p, 
       width = 8, height = 6)



###'######################################################################
###'
###' Plot CAExp and CALocalAssist: 02. Per-pupil
###'
###'

### Plot CAExp: Per-pupil
p <- plot_trend_xy(state_operations_df, x = Fiscalyear, y = CAExp_PP, 
                   yline = 2013, ylim = c(2000, 5000)) + 
  labs(title = "Total Non-Education State Expenditures Per-Pupil ", 
       subtitle = "Inflation adjusted using CPI-U deflator (in real 2016 dollars)",  
       x = "Fiscal Year", 
       y = "Amount in dollars", 
       caption = "Source: The State Controller's Office")

ggsave("figures/State Operations_Non-Education Expenditures_02_Per-pupil.pdf", p, 
       width = 8, height = 6)


### Plot CALocalAssist: Per-pupil
p <- plot_trend_xy(local_assistance_df, x = Fiscalyear, y = CALocalAssist_PP, 
                   yline = 2013, ylim = c(8000, 16000)) + 
  labs(title = "State Local Assistance Provided Excluding Education: Per-pupil", 
       subtitle = "Inflation adjusted using CPI-U deflator (in real 2016 dollars)",  
       x = "Fiscal Year", 
       y = "Amount in dollars", 
       caption = "Source: The State Controller's Office")

ggsave("figures/Local_Assistance_Non-Education Expenditures_02_Per-pupil.pdf", p, 
       width = 8, height = 6)



###'######################################################################
###'
###' Merge two dataframes: State operations & Local assistance
###'
###'

### Merge the two data frames
state_predictors <- state_operations_df %>%
  select(-state_ADA, -state_ADA_thousand) %>% 
  left_join(local_assistance_df, by = c("Fiscalyear")) %>%
  select(Fiscalyear, starts_with("state_ADA"), everything())


### Save as .rda file
save(state_predictors, file = "processed_data/state_predictors.rda")


