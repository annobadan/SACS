
###'######################################################################
###'
###' Effects of LCFF on Distric Per-Pupil Revenue from State  
###'
###' => Replicate Figure 10. 
###'    
###'    The evolution of district per-pupil revenue from the state
###'    before and after LCFF 
###'    according to the fourmula weight / simulated IV
###'
###' 20180915 JoonHo Lee
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


### ADA all years 2003-2016
load(file = "processed_data/df_ADA_allyears.rda")


### Predicted per-pupil revenue from the state
load(file = "processed_data/Predicted_district_per-pupil_revenues.rda")



###'######################################################################
###'
###' Merge district information to actual/predicted per-pupil revenue from state
###' 
###' To merge two additional information:
###' (1) K12ADA_C: to get weighted means of the differences
###' (2) SimIV and Sim_IV based grouping variables
###' 
###'

names(df_1314_SimIV)
names(df_ADA_allyears)

### K12ADA_C
df_K12ADA_C <- df_ADA_allyears %>%
  select(Fiscalyear, Ccode, Dcode, K12ADA_C) 


### Sim_IV + quartile based on formula weight
df_SimIV <- df_1314_SimIV %>%
  select(Ccode, Dcode, UPP, formula_weight, 
         SimIV, two_quantile, quartile, quintile, decile) %>%
  mutate(quartile_wt = ntile(formula_weight, 4)) %>%
  select(-formula_weight)


### Merge with df_prepost
df_prepost_extend <- df_prepost %>%
  mutate(Dcode = as.numeric(levels(Dcode))[Dcode]) %>%
  left_join(df_K12ADA_C, by = c("Fiscalyear", "Ccode", "Dcode")) %>%
  left_join(df_SimIV, by = c("Ccode", "Dcode")) %>%
  left_join(years_of_operation, by = c("Ccode", "Dcode")) 


### Generate diff score & centered pred_mod1, pred_mod2
df_prepost_extend <- df_prepost_extend %>% 
  mutate(diff_mod1 = state_rev_PP_16 - pred_mod1, 
         diff_mod2 = state_rev_PP_16 - pred_mod2, 
         pred_mod1_c = pred_mod1 - mean(pred_mod1), 
         pred_mod2_c = pred_mod2 - mean(pred_mod2))
  

### Save the resulting dataset
save(df_prepost_extend, file = "processed_data/df_prepost_extend.rda")
write.dta(df_prepost_extend, file = "processed_data/df_prepost_extend.dta")




###'######################################################################
###'
###' Define a function to extract and arrange estimates from lm()
###'
###'

arrange_LCFF_effects <- function(df_coef = df_coef, 
                                 year = "Fiscalyear", 
                                 category = "quartile") {
  
  ### Filter coef estimates only for year dummies
  df <- df_coef[grepl(year, df_coef$variable), ] 
  
  
  ### Generate two indicators: Year, SimIV categories
  Nchar <- nchar(df$variable)
  SimIV_loc <- unlist(gregexpr(category, df$variable))
  
  
  ### Generate year, SimIV indicators  
  df <- df %>%
    mutate(year_idx = as.numeric(substr(df$variable, Nchar - 3, Nchar)), 
           SimIV_idx = substr(variable, 
                              SimIV_loc + nchar(category) + 1, 
                              SimIV_loc + nchar(category) + 2),
           SimIV_idx = as.numeric(gsub("([0-9]+).*$", "\\1", SimIV_idx)), 
           SimIV_idx = if_else(is.na(SimIV_idx), 1, SimIV_idx)
    ) %>%
    arrange(SimIV_idx)
  
  
  ### Collect estimates by SimIV
  df_est <- df %>% 
    select(ends_with("idx"), estimate) %>%
    spread(key = SimIV_idx, value = estimate) %>%
    rbind(c(2013, rep(0, max(df$SimIV_idx)))) %>%
    arrange(year_idx)
  
  
  ### Calculate year differences for each SimIV category
  df_est[, -(1:2)] <- df_est[, -(1:2)] + df_est[, 2] 
  
  
  ### Convert to log data format to plot
  df_est_long <- df_est %>%
    gather(key = SimIV, value = estimate, -year_idx)
  
  return(df_est_long)
}


### Graph labels
labels <- labs(title = "Effects of LCFF funding formula on per-pupil revenue",
               subtitle = NULL, 
               caption = "Source: SACS Unaudited Data", 
               x = "Fiscal Year",  
               y = "Difference in district per-pupil revenues compared to 2013") 



###'######################################################################
###'
###' 1) Fit prediction model for district per-pupil revenue from state
###' 
###' 2) Plot the predicted per-pupil revenues by Simulated IV
###' 
###' => The effect of LCFF funding "offer" on the level of per-pupil revenue:
###'    Difference-in-difference estimates
###' 
###' 

###' Model 1.Counterfactual prediction model #2 (full interaction) + 
###'         district & year fixed effects

fit <- lm(state_rev_PP_16 ~ factor(quintile)*relevel(factor(Fiscalyear), "2013") + 
            pred_mod2 + relevel(factor(Dtype), "UNIFIED") +  factor(Dcode), 
          data = df_prepost_extend)

df_coef <- get_lm_est_df(fit)

df_plot <- arrange_LCFF_effects(df_coef, "Fiscalyear", "quintile")

df_plot <- df_plot %>% 
  filter(SimIV != 1) %>%
  mutate(estimate = round(estimate, 0))

p <- plot_trend_grp(df_plot, year_idx, estimate, SimIV, yline = 2013) + 
  geom_hline(yintercept = 0, col = "red", linetype = "dashed")

p <- p + labels + 
  labs(subtitle = "With district and year fixed effects, counterfactual prediction model #2")

ggsave("figures/LCFF funding formula effects_Model_01_pred_mod2_dist fixed effects.pdf", p, 
       width = 9, height = 7)

write.csv(df_coef, file = "tables/df_coef.csv")



###' Model 2.Counterfactual prediction model #2 (full interaction) + 
###'         district & year fixed effects

fit <- lm(state_rev_PP_16 ~ factor(quintile)*relevel(factor(Fiscalyear), "2013") + 
            pred_mod1 + relevel(factor(Dtype), "UNIFIED") +  factor(Dcode), 
          data = df_prepost_extend)

df_coef <- get_lm_est_df(fit)

df_plot <- arrange_LCFF_effects(df_coef, "Fiscalyear", "quintile")

df_plot <- df_plot %>% 
  filter(SimIV != 1) %>%
  mutate(estimate = round(estimate, 0))

p <- plot_trend_grp(df_plot, year_idx, estimate, SimIV, yline = 2013) + 
  geom_hline(yintercept = 0, col = "red", linetype = "dashed")

p <- p + labels + 
  labs(subtitle = "With district and year fixed effects, counterfactual prediction model #1")

ggsave("figures/LCFF funding formula effects_Model_02_pred_mod1_dist fixed effects.pdf", p, 
       width = 9, height = 7) 

write.csv(df_coef, file = "tables/df_coef.csv")



###' Model 3.Counterfactual prediction model #2 (full interaction) + change score +
###'         district & year fixed effects

fit <- lm(diff_mod2 ~ factor(quintile)*relevel(factor(Fiscalyear), "2013") + 
            relevel(factor(Dtype), "UNIFIED") +  factor(Dcode), 
          data = df_prepost_extend)

df_coef <- get_lm_est_df(fit)

df_plot <- arrange_LCFF_effects(df_coef, "Fiscalyear", "quintile")

df_plot <- df_plot %>% 
  filter(SimIV != 1) %>%
  mutate(estimate = round(estimate, 0))

p <- plot_trend_grp(df_plot, year_idx, estimate, SimIV, yline = 2013) + 
  geom_hline(yintercept = 0, col = "red", linetype = "dashed")

p <- p + labels + 
  labs(subtitle = "With district and year fixed effects, counterfactual prediction model #2, change scores")

ggsave("figures/LCFF funding formula effects_Model_03_pred_mod2 change scores_dist fixed effects.pdf", p, 
       width = 9, height = 7)

write.csv(df_coef, file = "tables/df_coef.csv")



###' Model 4.Counterfactual prediction model #2 (full interaction) + 
###'         with year and without fixed effects

fit <- lm(state_rev_PP_16 ~ factor(quintile)*relevel(factor(Fiscalyear), "2013") + 
            pred_mod2 + relevel(factor(Dtype), "UNIFIED"), 
          data = df_prepost_extend)

df_coef <- get_lm_est_df(fit)

df_plot <- arrange_LCFF_effects(df_coef, "Fiscalyear", "quintile")

df_plot <- df_plot %>% 
  filter(SimIV != 1) %>%
  mutate(estimate = round(estimate, 0))

p <- plot_trend_grp(df_plot, year_idx, estimate, SimIV, yline = 2013) + 
  geom_hline(yintercept = 0, col = "red", linetype = "dashed")

p <- p + labels + 
  labs(subtitle = "Without district fixed effects, counterfactual prediction model #2")

ggsave("figures/LCFF funding formula effects_Model_04_pred_mod2_without dist fixed effects.pdf", p, 
       width = 9, height = 7)

write.csv(df_coef, file = "tables/df_coef.csv")



###' Model 5.Counterfactual prediction model #2 (full interaction) + change score +
###'         without district fixed effects

fit <- lm(diff_mod2 ~ factor(quintile)*relevel(factor(Fiscalyear), "2013") + 
            relevel(factor(Dtype), "UNIFIED"), 
          data = df_prepost_extend)

df_coef <- get_lm_est_df(fit)

df_plot <- arrange_LCFF_effects(df_coef, "Fiscalyear", "quintile")

df_plot <- df_plot %>% 
  filter(SimIV != 1) %>%
  mutate(estimate = round(estimate, 0))

p <- plot_trend_grp(df_plot, year_idx, estimate, SimIV, yline = 2013) + 
  geom_hline(yintercept = 0, col = "red", linetype = "dashed")

p <- p + labels + 
  labs(subtitle = "Without district fixed effects, counterfactual prediction model #2, change scores")

ggsave("figures/LCFF funding formula effects_Model_05_pred_mod2 change scores_without dist fixed effects.pdf", p, 
       width = 9, height = 7)

write.csv(df_coef, file = "tables/df_coef.csv")



