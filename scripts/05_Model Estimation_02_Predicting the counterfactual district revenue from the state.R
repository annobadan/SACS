
###'######################################################################
###'
###' District per-pupil revenue prediction model
###' 
###' - Fit the regression model to obtain an estimate of 
###'   the COUNTERFACTUAL district revenue from the state
###'   if LCFF had not occurred.
###'   
###'   
###' 20180903 JoonHo Lee
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
library(foreign)


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


### State-level predictors
load(file = "processed_data/state_predictors.rda")



###'######################################################################
###'
###' Define Outcome variable: 
###' 
###' Per-pupil total revenue "FROM STATE" across years 2003-2016    
###'
###'

###' Load per-pupil revenue dataframe factored by SACS object code
###' To sort out only revenue FROM STATE
names(list_revenues)
df_rev <- list_revenues[["revenue_by_object"]]


###' Exclude the following revenue sources:
###' (1) Federal Revenue
###' (2) Other Local Revenue, not Contributions
###' (3) Other Local Revenue, Contributions
###' 
###' Then the resulting variable would includes:
###' (1) Revenue Limit (LCFF from 2013)
###' (2) Other State Revenue
###' (3) Other Financing Sources

table(df_rev$revenue_by_object)

df_rev_state <- df_rev %>%
  filter(!(revenue_by_object %in% c("Federal Revenue",
                                    "Other Local Revenue, not Contributions",
                                    "Other Local Revenue, Contributions",
                                    "Other Financing Sources")))

# df_rev_state <- df_rev %>%
#   filter(!(revenue_by_object %in% c("Federal Revenue", 
#                                     "Other Local Revenue, not Contributions", 
#                                     "Other Local Revenue, Contributions")))


### District-level per-pupil revenues FROM STATE
df_rev_state_sum <- df_rev_state %>% 
  group_by(Fiscalyear, Ccode, Dcode) %>%
  summarise(Dname = first(Dname), 
            Dtype = first(Dtype),
            K12ADA_C = first(K12ADA_C), 
            state_rev_PP_16 = sum(sum_value_PP_16, na.rm = TRUE)) %>%
  filter(Dtype %in% c("UNIFIED", 
                      "ELEMENTARY", 
                      "HIGH"))



###'######################################################################
###'
###' Merge predictors to the outcome dataframe
###' 
###'

df_rev_state_sum_pred <- df_rev_state_sum %>%
  left_join(state_predictors, by = c("Fiscalyear")) %>%   # fiscalyear predictors
  left_join(df_1314_SimIV, by = c("Ccode", "Dcode")) %>%  # district predictors 
  mutate(timetrend = Fiscalyear -2003)



###'######################################################################
###'
###' PRediction model
###' 
###' STEP #01. Run only for 2003-2012, just prior to LCFF, get coefficients
###'      
###'     

### Pre-LCFF (2003-2012) training data
df_preLCFF <- df_rev_state_sum_pred %>%
  filter(!(Fiscalyear %in% c(2013, 2014, 2015, 2016))) %>%
  select(Fiscalyear, Ccode, Dcode, state_rev_PP_16, CAExp_PP, CALocalAssist_PP, 
         timetrend, formula_weight)


###' Indicator for valid district (complete cases)
###' Important for prediction
valid_Dcode <- unique(df_preLCFF$Dcode[complete.cases(df_preLCFF)])
df_preLCFF <- df_preLCFF %>% filter(Dcode %in% valid_Dcode)


### Convert to factor: Dcode
df_preLCFF$Dcode <- factor(df_preLCFF$Dcode)


###' Model 01. Fixed effects regression without interaction terms
fit_preLCFF_mod1 <- lm(state_rev_PP_16 ~ CAExp_PP + CALocalAssist_PP + 
                         timetrend + formula_weight + timetrend:formula_weight + 
                         Dcode, 
                       data = df_preLCFF)

mod1_coef <- get_lm_est_df(fit_preLCFF_mod1)


###' Model 02. Fixed effects regression with full interaction terms
fit_preLCFF_mod2 <- lm(state_rev_PP_16 ~ Dcode + CAExp_PP:Dcode + CALocalAssist_PP:Dcode + 
                         timetrend + formula_weight + timetrend:formula_weight, 
                       data = df_preLCFF)

mod2_coef <- get_lm_est_df(fit_preLCFF_mod2)



###'######################################################################
###'
###' Prediction model
###' 
###' STEP #02. Predict the level of district per-pupil revenues from state sources
###'           for all years in the data, including the post-LCFF era (2013-2016)
###'     
###'     

### Pre-LCFF (2003-2012) + Post-LCFF (2013-2016): Actual data
df_prepost <- df_rev_state_sum_pred %>%
  select(Fiscalyear, Ccode, Dcode, state_rev_PP_16, CAExp_PP, CALocalAssist_PP, 
         timetrend, formula_weight)


###' Filter based on the indicator for valid district (complete cases)
df_prepost <- df_prepost %>% filter(Dcode %in% valid_Dcode)


### Convert to factor: Dcode
df_prepost$Dcode <- factor(df_prepost$Dcode)


### Get predicted values
df_prepost$pred_mod1 <- predict(fit_preLCFF_mod1, df_prepost)
df_prepost$pred_mod2 <- predict(fit_preLCFF_mod2, df_prepost)


### Fiscalyear
df_prepost$Fiscalyear <- df_prepost$timetrend + 2003



###'######################################################################
###'
###' Save the resulting dataset: df_prepost
###'
###'

### Save as .rda
save(df_prepost, file = "processed_data/Predicted_district_per-pupil_revenues.rda")

### Save as .csv
write.csv(df_prepost, file = "processed_data/Predicted_district_per-pupil_revenues.csv")



###'######################################################################
###'
###' Prediction model
###' 
###' Plot Actual vs. Predicted per-pupil revenues
###' 
###' 

### Generate actual vs predicted data to plot (along with Fiscalyear)
df_pred_vs_actual <- df_prepost %>%
  group_by(Fiscalyear) %>%
  summarise(actual_rev = mean(state_rev_PP_16, na.rm = TRUE), 
            predicted_rev_mod1 = mean(pred_mod1, na.rm = TRUE), 
            predicted_rev_mod2 = mean(pred_mod2, na.rm = TRUE)) %>%
  mutate_at(.vars = c("actual_rev", "predicted_rev_mod1", "predicted_rev_mod2"), 
            .funs = round) %>%
  gather(actual_vs_pred, mean_rev_PP_16, 2:4)


###' Predictions from mod2 is much closer to actual than those from mod1 
###' Have smaller residuals
df_pred_diff <- df_prepost %>%
  filter(Fiscalyear < 2013) %>%
  mutate(pred_diff_mod1 = state_rev_PP_16 - pred_mod1, 
         pred_diff_mod2 = state_rev_PP_16 - pred_mod2)

summary(df_pred_diff$pred_diff_mod1)
summary(df_pred_diff$pred_diff_mod2)


### Plot only predictions from model 2
df_plot <- df_pred_vs_actual %>% 
  filter(actual_vs_pred != "predicted_rev_mod1") 

df_plot$actual_vs_pred <- recode(df_plot$actual_vs_pred, 
                                 actual_rev = "Actual", 
                                 predicted_rev_mod1 = "Predicted")


p <- plot_trend_grp(df_plot, Fiscalyear, mean_rev_PP_16, actual_vs_pred, 
                    yline = 2013, ylim = c(7500, 18000)) + 
  labs(title = "Actual vs. Predicted District Per-pupil Revenues from the State", 
       subtitle = "Inflation adjusted using CPI-U deflator (in real 2016 dollars)", 
       x = "Fiscal Year", 
       y = "Per-pupil revenues from the state in real 2016 dollars", 
       caption = "Source: CDE's SACS Unaudited Data")

ggsave("figures/Actual_vs_Predicted_01_Per-pupil total revenue.pdf", p, 
       width = 9, height = 7)



