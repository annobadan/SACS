
###'######################################################################
###'
###'  Fitting piecewise growth curve models:
###' 
###'  => Provide visual evidence that the funding formula weight  
###'     does not predict changes in funding levels in the previous years 
###'     leading up to the LCFF policy change in 2013
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
data_dir <- c("D:/Data/LCFF")


### Call libraries
library(tidyverse)
library(lme4)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Load the pre-cleaned datasets
###'
###'

### Per-pupil expenditures & revenues
load(file = "processed_data/list_expenditures_def_1_2.RData")
load(file = "processed_data/list_revenues.rda")


### FY1314 Simuated IV
load(file = "processed_data/df_1314_SimIV.rda")



###'######################################################################
###'
###' Generate datasets for model fitting
###' 
###' (1) Per-pupil total revenue (natural log)
###' (2) Per-pupil total expenditure (natural log)
###'
###'

### Dataset #1. Per-pupil total revenues
names(list_revenues)
df_rev <- list_revenues[["total_rev"]]

df_rev <- df_rev %>%
  left_join(df_1314_SimIV, by = c("Ccode", "Dcode")) %>%
  mutate(log_rev_PP = if_else(sum_value_PP_16 <= 0, 0, log(sum_value_PP_16))) %>%
  filter(Dtype %in% c("ELEMENTARY", "HIGH", "UNIFIED"))


### Check which observation generated the NaN when converting to log scale
idx <- which(is.nan(log(df_rev$sum_value_PP_16)))
df_rev$sum_value_PP_16[idx]


### Dataset #2. Per-pupil total expenditures
names(list_expenditures_def_1)
df_exp <- list_expenditures_def_1[["total_exp"]]

df_exp <- df_exp %>%
  left_join(df_1314_SimIV, by = c("Ccode", "Dcode")) %>%
  mutate(log_exp_PP = if_else(sum_value_PP_16 <= 0, 0, log(sum_value_PP_16))) %>%
  filter(Dtype %in% c("ELEMENTARY", "HIGH", "UNIFIED"))



###'######################################################################
###'
###' Initial exploratory plots
###'
###'

### Per-pupil total revenue
df_rev_plot <- df_rev %>%
  filter(log_rev_PP != 0)

p <- ggplot(data = df_rev_plot, aes(x = Fiscalyear, y = log_rev_PP)) +
  geom_line(aes(group = Dcode), color = "gray") + 
  geom_smooth(aes(group = 1), method = "loess", size = 2, color = "blue", se = TRUE) +
  geom_vline(xintercept = c(2007, 2012), col = "red", linetype = "dashed") + 
  facet_grid(.~ Dtype) + 
  theme_trend + 
  scale_x_continuous(breaks = seq(2003, 2016, by = 1)) + 
  labs(title = "Change of Per-pupil Total Revenues, 2003-2016",
       subtitle = "Inflation adjusted using CPI-U deflator (in real 2016 dollars)", 
       y = "Per-pupil total revenue (Natural Log)", 
       x = "Fiscal Year")

ggsave("figures/Growth_01_Per-pupil total revenue.pdf", p, width = 15, height = 9)


### Per-pupil total expenditure
df_exp_plot <- df_exp %>%
  filter(log_exp_PP != 0)

p <- ggplot(data = df_exp_plot, aes(x = Fiscalyear, y = log_exp_PP)) +
  geom_line(aes(group = Dcode), color = "gray") + 
  geom_smooth(aes(group = 1), method = "loess", size = 2, color = "blue", se = TRUE) +
  geom_vline(xintercept = c(2007, 2012), col = "red", linetype = "dashed") + 
  facet_grid(.~ Dtype) + 
  theme_trend + 
  scale_x_continuous(breaks = seq(2003, 2016, by = 1)) + 
  labs(title = "Change of Per-pupil Total Expenditures, 2003-2016",
       subtitle = "Inflation adjusted using CPI-U deflator (in real 2016 dollars)", 
       y = "Per-pupil total expenditure (Natural Log)", 
       x = "Fiscal Year")

ggsave("figures/Growth_02_Per-pupil total expenditure.pdf", p, width = 15, height = 9)



###'######################################################################
###'
###' Fitting piecewise growth curve models:
###' 
###' => Provide visual evidence that the funding formula weight  
###'    does not predict changes in funding levels in the previous years 
###'    leading up to the LCFF policy change in 2013
###'
###' Level 1: 
###' Y_{ij} = beta_{0j} + beta_{1j} time_1 + beta_{2j} time_2 + beta_{3j} time_3 + r_{ij}
###'
###' Level 2:
###' beta_{0j} = gamma_{00} + gamma_{01} formula_weight_{j} + U_{0j}
###' beta_{1j} = gamma_{10} + gamma_{11} formula_weight_{j} + U_{1j}
###' beta_{2j} = gamma_{20} + gamma_{21} formula_weight_{j} + U_{2j}
###' beta_{3j} = gamma_{30} + gamma_{31} formula_weight_{j} + U_{3j}
###'
###' Time coding scheme:
###' time_1: the pre-recession housing bubble (2003-2006) 
###' time_2: the housing crash and ensuing recession (2007-2009 + 2009-2012)
###' time_3: LCFF years (2013-2016)
###'
###' time_1: c(0, 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)
###' time_2: c(0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 6, 6, 6, 6)
###' time_3: c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4)
###'
###'

###' Generate time variables for piecewise regression
###' knot: 2006, 2012
Fiscalyear <- 2003:2016
time_1 <- c(0, 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)
time_2 <- c(0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 6, 6, 6, 6)
time_3 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4)
time_df <- tibble(Fiscalyear, time_1, time_2, time_3)


### Model 1. Per-pupil total revenue
df_rev <- left_join(df_rev, time_df, by = c("Fiscalyear"))

fit_rev <- lmer(log_rev_PP ~ (time_1 + time_2 + time_3) * formula_weight + 
                  (time_1 + time_2 + time_3 | Dcode), 
                data = df_rev, REML = FALSE)

summary(fit_rev)


### Model 2. Per-pupil total expenditure
df_exp <- left_join(df_exp, time_df, by = c("Fiscalyear"))

fit_exp <- lmer(log_exp_PP ~ (time_1 + time_2 + time_3) * formula_weight + 
                  (time_1 + time_2 + time_3 | Dcode), 
                data = df_exp, REML = FALSE)

summary(fit_exp)



###'######################################################################
###'
###' Graph fitted curves
###' 
###' 

### Define a function to get fitted values 
get_fitted_lmer_df <- function(lmer_fit){
  
  ### Calculate the fitted y
  fitted_values <- model.matrix(lmer_fit) %*% fixef(lmer_fit)
  
  ### Combine with the orginal dataset
  fitted_df <- data.frame(lmer_fit@frame, fitted = fitted_values)
  
  fitted_df
}


### Get quartile, quintile, and decile for formula_weight, not SimIV
formula_wt_quantiles <- df_1314_SimIV %>%
  mutate(quartile_wt = ntile(formula_weight, 4), 
         quintile_wt = ntile(formula_weight, 5), 
         decile_wt = ntile(formula_weight, 10)) %>%
  select(Ccode, Dcode, ends_with("_wt"))


### Plot #1. Per-pupil total revenue
fit_rev_df <- get_fitted_lmer_df(fit_rev) %>%
  left_join(time_df, by = c("time_1", "time_2", "time_3")) %>%  # merge Fiscalyear
  left_join(formula_wt_quantiles, by = c("Dcode")) %>%
  arrange(Dcode)


p <- ggplot(data = fit_rev_df, aes(x = Fiscalyear, y = log_rev_PP, 
                                   group = Dcode, color = quartile_wt)) + 
  geom_line(aes(y = fitted)) + 
  geom_vline(xintercept = c(2006, 2012), col = "red", linetype = "dashed") + 
  # geom_point(shape = 19, col = "grey") + 
  # stat_summary(fun.y = mean, geom = "point", col = "gray") + 
  theme_trend + 
  theme(legend.title = element_text(face = "plain")) + 
  scale_x_continuous(breaks = seq(2003, 2016, by = 1)) +
  scale_y_continuous(limits = c(9.25, 10)) + 
  labs(title = "Fitted Curves by the Magnitude of Formula Weights",
       subtitle = "Inflation adjusted using CPI-U deflator (in real 2016 dollars)", 
       y = "Per-pupil total revenue (Natural Log)", 
       x = "Fiscal Year", 
       color = "formula weight") 

ggsave("figures/Fitted_Curves_01_Per-pupil total revenue.pdf", p, width = 8, height = 6)


### Plot #2. Per-pupil total expenditure
fit_exp_df <- get_fitted_lmer_df(fit_exp) %>%
  left_join(time_df, by = c("time_1", "time_2", "time_3")) %>%  # merge Fiscalyear
  left_join(formula_wt_quantiles, by = c("Dcode")) %>%
  arrange(Dcode)


p <- ggplot(data = fit_exp_df, aes(x = Fiscalyear, y = log_exp_PP, 
                                   group = Dcode, color = quartile_wt)) + 
  geom_line(aes(y = fitted)) + 
  geom_vline(xintercept = c(2006, 2012), col = "red", linetype = "dashed") + 
  # geom_point(shape = 19, col = "grey") + 
  # stat_summary(fun.y = mean, geom = "point", col = "gray") + 
  theme_trend + 
  theme(legend.title = element_text(face = "plain")) + 
  scale_x_continuous(breaks = seq(2003, 2016, by = 1)) +
  scale_y_continuous(limits = c(9.25, 10)) + 
  labs(title = "Fitted Curves by the Magnitude of Formula Weights",
       subtitle = "Inflation adjusted using CPI-U deflator (in real 2016 dollars)", 
       y = "Per-pupil total expenditure (Natural Log)", 
       x = "Fiscal Year", 
       color = "formula weight") 

ggsave("figures/Fitted_Curves_02_Per-pupil total expenditure.pdf", p, width = 8, height = 6)

