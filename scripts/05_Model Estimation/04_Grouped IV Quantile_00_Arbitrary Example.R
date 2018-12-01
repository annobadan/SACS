
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
###' 20180918 JoonHo Lee
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
###' Import dataset 
###'
###'

df <- read.dta(file = "processed_data/nlsw88_IVQ_example.dta")

classmode(df, everything())



###'######################################################################
###'
###' Explore dataset
###'
###'

### Frequency tables: group-level (industry)
tabdf(df, industry)
tabdf(df, xvar)
tabdf(df, wvar)


### Frequency tables: Individual-level 
tabdf(df, wage)
tabdf(df, age)
tabdf(df, married)


### Distribution of individual-level variables within each group
ggplot(df, aes(y = wage, color = industry)) +
  geom_boxplot()+
  # geom_vline(data = mu, aes(xintercept = grp.mean, color = sex),
  #            linetype = "dashed")+
  labs(title = "Wage density curve", x = "log wage", y = "Density") + 
  theme_classic()



###'######################################################################
###'
###' STEP 1. Calculate conditional quantiles
###'
###' (i) perform quantile regression within each group 
###'     to estimate effects of micro-level covariates,  
###'     or, if no micro-level covariates are included, 
###'     calculate the desired quantile for the outcome within each group; 
###'
###'

### Two varying conditions
quantile_grid <- seq(from = 0.3, to = 0.7, by = 0.1)
industry_vec <- as.vector(unique(df$industry))


### Define empty dataframe to collect coefficients
tau_mat <- matrix(NA, 
                     nrow = length(industry_vec), 
                     ncol = length(quantile_grid))

tau_df <- data.frame(tau_mat)
names(tau_df) <- paste0("tau", quantile_grid*100)
rownames(tau_df) <- industry_vec


### Calculate conditional quantiles for each industry
for (i in seq_along(quantile_grid)){
  for (j in seq_along(industry_vec)){
    
    # Subset only applicable industry
    df_fit <- df %>%
      filter(industry == industry_vec[j])
    
    
    # Fit the standard quantile regression model
    qr_fit <- rq(wage ~ age + factor(married), 
                 data = df_fit, 
                 tau = quantile_grid[i])
    
    # Extract regression coefficient (intercept in this example)
    tau_df[j, i] <- as.vector(qr_fit[["coefficients"]][1])
  }
}

tau_df$industry <- rownames(tau_df)
rownames(tau_df) <- NULL



###'######################################################################
###'
###' STEP 2. Group-level 2SLS estimation
###'
###' (ii) regress the estimated group-specific effects on group-level covariates 
###'      using either 2SLS, if the group-level covariates are endogenous, 
###'      or OLS, if the group-level covariates are exogenous, 
###'      either of which cases would render standard quantile regression inconsistent. 
###'
###'

###' Make up some grouped variables
###' since there weren't any in sample dataset
###' (This part is unnecessary for real data)   
 
df_group_predictors <- df %>%
  group_by(industry) %>%
  mutate(xvar = runif(1, 0, 1),
         wvar = xvar*3 + runif(1, 0, 1)) %>%
  summarise(xvar = first(xvar), 
            wvar = first(wvar))

df_group <- tau_df %>%
  left_join(df_group_predictors, by = c("industry"))


### Fit group-level 2SLS regression over quantiles
df_est_collect <- data.frame()

for (i in seq_along(quantile_grid)){
  
  # Assign quantile, select variable
  df_est <- df_group
  idx <- names(df_est) == paste0("tau", quantile_grid[i]*100)
  names(df_est)[idx] <- "yvar"
  
  # Fit 2SLS IV regression
  ivfit <- ivreg(yvar ~ xvar | wvar, data = df_est)
  summary(ivfit)
  est <- robust.se(ivfit)
  est_mat <- matrix(est, ncol = 2, byrow = TRUE)
  est_mat <- est_mat[, 2]
  
  # Collect estimates
  df_est_collect <- rbind(df_est_collect, est_mat)
  
}

names(df_est_collect) <- c("coef", "std_err", "t_value", "p_value")

cbind.data.frame(quantile_grid, df_est_collect)


