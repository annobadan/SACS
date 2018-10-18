
###'######################################################################
###'
###' Model Estimation. 
###' 
###' Preparation for IV Group Quantile Regression
###' 
###' The estimator is computationally simple to implement and 
###' consists of two steps: 
###' 
###' (i) perform quantile regression within each group 
###'     to estimate effects of micro-level covariates,  
###'     or, if no micro-level covariates are included, 
###'     calculate the desired quantile for the outcome within each group;
###'     
###' (ii) regress the estimated group-specific effects on group-level covariates 
###'      using either 2SLS, if the group-level covariates are endogenous, 
###'      or OLS, if the group-level covariates are exogenous, 
###'      either of which cases would render standard quantile regression inconsistent.  
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


### Call libraries
library(tidyverse)
library(foreign)
library(lme4)
library(quantreg)
library(AER)
library(ivpack)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import datasets
###'
###'

### Student compositions + Teacher compositions 2003-2017
setwd(work_dir)
load(file = "processed_data/df_Teacher_Student_compositions_0317.rda")
df <- df_Teacher_Student


### District information on actual/predicted per-pupil revenue from state
load(file = "processed_data/df_prepost_extend.rda")



###'######################################################################
###'
###' Number of schools within district
###'
###'

df_NSchools <- df %>% 
  group_by(CountyCode, DistrictCode, DistrictName) %>%
  summarise(NSchools = n_distinct(SchoolCode))

tabdf(df_NSchools, NSchools)
tabdf_plot(df_NSchools, NSchools, limits = c(0, 100))

save(df_NSchools, file = "processed_data/df_N_Schools_in_District.rda")



  










