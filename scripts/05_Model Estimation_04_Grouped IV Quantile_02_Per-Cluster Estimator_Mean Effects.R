
###'######################################################################
###'
###' Model Estimation
###'
###' First apply the Per-Cluster (PC) Estimator
###' 
###' Bates, M. D., Castellano, K. E., Rabe-Hesketh, S., & Skrondal, A. (2014). 
###' Handling correlations between covariates and random slopes in multilevel models. 
###' Journal of Educational and Behavioral Statistics, 39(6), 524-549.
###' 
###' - Before we go to the grouped IV quantile regression, 
###'   we first get the average effects using the PC estimator
###' 
###' 
###' 20181101 JoonHo Lee
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
library(arrayhelpers)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



