
###'######################################################################
###'
###' Model Estimation 
###' 
###' Try Per-Cluster (PC) Estimator by
###' 
###' Bates, M. D., Castellano, K. E., Rabe-Hesketh, S., & Skrondal, A. (2014). 
###' Handling correlations between covariates and random slopes in multilevel models. 
###' Journal of Educational and Behavioral Statistics, 39(6), 524-549.
###' 
###' 
###' 20181030 JoonHo Lee
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
library(arrayhelpers)
library(miceadds)
library(multiwayvcov)
library(lmtest)
library(sandwich)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import HS & B dataset
###'
###'

df <- read.csv(file = "processed_data/hsb.csv")



###'######################################################################
###'
###' Initial data setup
###'
###' The variables of interest are:
###' - sector (wj) = cluster-level indicator for whether school is Catholic
###' - ses (xij) = unit-level covariate that indexes students' socioeconomic status
###' - mathach (yij) = outcome, students' performance on a math test 
###' - schoolid = cluster (school) identifier */
###'
###'

classmode(df, everything())

df <- df %>% 
  select(schoolid, mathach, sector, ses)

### Generate cross-level interaction term between ses and sector (xij*wj) 
df <- df %>%
  mutate(sesXsector = ses*sector)



###'######################################################################
###'
###' Random Effects (REML)
###' 
###' Estimation is done in one step using covariates that are 
###' unit-level, cluster-level, and interactions between the two. 
###' 
###' Note: ses is the covariate with a random slope
###'
###'

fit_RE <- lmer(mathach ~ ses + sector + sesXsector + (ses|schoolid), df)

summary(fit_RE)



###'######################################################################
###'
###' Augmented Fixed Effects (FE+)
###'
###'

### Step 1. Fixed effect estimates

### Estimate coefficients of unit-level covariates using standard fixed effects
fit_FE <- lm(mathach ~ ses + sesXsector + factor(schoolid), data = df)
summary(fit_FE)


### Extract the coefficients
beta_ses <- as.vector(summary(fit_FE)$coefficients["ses",][1])
beta_sesXsector <- as.vector(summary(fit_FE)$coefficients["sesXsector",][1])


### Step 2. Regress quasi-residuals on cluster-level covariate

### Generate "ynew" as residuals from the first stage regression
df <- df %>%
  mutate(ynew = mathach - beta_ses*ses - beta_sesXsector*sesXsector)


### Regress the residuals on the cluster-level covariate, with cluster-robust SEs
fit_FEplus <- lm.cluster(ynew ~ sector, data = df, cluster = "schoolid")
summary(fit_FEplus)



###'######################################################################
###'
###' Per-Cluster Regression (PC)
###'
###'

###' Step 1
###' Not needed because there are no unit-level covariates 
###' that do not have random slopes (R3 = 0)


###' Step 2
###' For each cluster, regress outcome on unit-level covariate using OLS, 
###' saving estimates of the intercepts (a1) and coefficients (a2) 

schoolid_vec <- unique(df$schoolid)

collect_est <- data.frame()


for (i in seq_along(schoolid_vec)){
  
  ### Subset only one school
  df_sub <- df %>%
    filter(schoolid == schoolid_vec[i])
  
  ### Fit OLS regression for the one school data
  fit <- lm(mathach ~ ses, data = df_sub)
  
  
  ### Save the intercept and coefficient
  collect_est_row <- c(unique(df_sub$schoolid), 
                       summary(fit)$coefficients["(Intercept)", "Estimate"], 
                       summary(fit)$coefficients["ses", "Estimate"]) 
  
  names(collect_est_row) <- c("schoolid", "Intercept", "Slope_ses")
  collect_est <- bind_rows(collect_est, collect_est_row)
  
}

### Merge with the original dataset containing the "sector" variable
df_sector <- df %>%
  group_by(schoolid) %>%
  summarise(sector = first(sector)) %>%
  left_join(collect_est, by = c("schoolid"))


###' Step 3-A
###' Regress intercept estimates (a1) on cluster-level covariate, 
###' OLS with robust SEs
###' 
###' estimated intercept (_cons) = estimated intercept of model (gamma0)
###' estimated coefficient of sector = estimated coefficient of sector (gamma1)

fit_PC_Intercept <- lm(Intercept ~ sector, data = df_sector)
coeftest(fit_PC_Intercept, vcov = vcovHC(fit_PC_Intercept, type = "HC1"))


###' Step 3-B
###' Regress slope estimates (a2) on cluster-level covariate, 
###' OLS with robust SEs
###' 
###' estimated intercept (_cons) = estimated coefficient of ses (beta1)
###' estimated coefficient of sector = estimated interaction parameter (beta2)

fit_PC_Slope_ses <- lm(Slope_ses ~ sector, data = df_sector)
coeftest(fit_PC_Slope_ses, vcov = vcovHC(fit_PC_Slope_ses, type = "HC1"))

