
###'######################################################################
###'
###' Step 2. Estimating the LCFF-induced exogenous increases 
###'         in district per-pupil expenditure
###' 
###' - The first stage model of 2SLS-IV
###'   
###'   
###' 20181029 JoonHo Lee
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
###' Load the pre-cleaned datasets
###' 
###'

### Per-pupil expenditures & revenues
load(file = "processed_data/list_expenditures_def_1_2.RData")
load(file = "processed_data/list_revenues.rda")


### FY1314 Simuated IV
load(file = "processed_data/df_1314_SimIV.rda")


###' Predicted counterfactual trends of district per-pupil revenues
###' if LCFF had not occurred
load(file = "processed_data/df_prepost_extend.rda")



###'######################################################################
###'
###' Outcome variable 
###' - Begin with per-pupil total expenditure
###' - Construct the collection of outcome variables later
###'
###'

### Extract per-pupil expenditure: Total
df_y <- list_expenditures_def_1[["total_exp"]]


### Select only necessary variables
df_y_vars <- df_y %>%
  select(Fiscalyear, Ccode, Dcode, sum_value_PP_16) %>%
  rename(PPE_total = sum_value_PP_16)


### Delete missing values
df_y_vars <- df_y_vars[complete.cases(df_y_vars), ]
nrow(df_y_vars)



###'######################################################################
###'
###' Merge outcome variable with predictors
###'
###'

### Merge
df_xy <- df_prepost_extend %>%
  left_join(df_y_vars, by = c("Fiscalyear", "Ccode", "Dcode"))


### Arrange
df_xy <- df_xy %>%
  arrange(Ccode, Dcode, Fiscalyear) %>%
  select(Ccode, Dcode, Fiscalyear, PPE_total, everything())



###'######################################################################
###'
###' Fit prediction model
###' 
###' PPE_total: district per-pupil total expenditure
###' 
###' (1) year dummies: years since 2013 
###'     => replace with exposure? (0 ... 0, 1, 2, 3, 4)
###' (2) simulated IV decile dummies
###' (3) predicted counterfactual trend of district per-pupil revenues (pred_mod2)
###' (4) district fixed effects
###' (5) year fixed effects
###'   
###' 

###' Prepare variants of year dummies 
###' (1) years since event
###' (2) exposure
df_xy <- df_xy %>%
  mutate(year_since = Fiscalyear - 2013, 
         exposure = if_else(Fiscalyear > 2013, 
                            Fiscalyear - 2013, 0))

year_vars <- listvars(df_xy, Ccode, Dcode, 
                      Fiscalyear, year_since, exposure, nrow = 100)



#################################################
### Model 1. year dummies using "year_since"
#################################################

### Convert to factor
df_xy_mod1 <- df_xy
df_xy_mod1$year_since <- factor(df_xy_mod1$year_since)
df_xy_mod1$year_since <- relevel(df_xy_mod1$year_since, "0")
df_xy_mod1$decile <- factor(df_xy_mod1$decile)
df_xy_mod1$decile <- relevel(df_xy_mod1$decile, "1")

classmode(df_xy_mod1, everything())


### Select only necessary variables
df_xy_mod1 <- df_xy_mod1 %>%
  select(Ccode, Dcode, Fiscalyear, year_since,
         decile, pred_mod2, PPE_total)


### Indicator for valid district (complete cases) => Important for prediction
temp <- df_xy_mod1 %>%
  select(Ccode, Dcode, Fiscalyear, year_since,
         decile, pred_mod2, PPE_total) 

valid_Dcode <- unique(temp$Dcode[complete.cases(temp)])

df_xy_mod1 <- df_xy_mod1 %>% 
  filter(Dcode %in% valid_Dcode)


### Generate model matrix with a large number of dummies
mod_mat <- model.matrix(~ decile*year_since, df_xy_mod1)
write.csv(mod_mat, file = "tables/model_matrix.csv")


### Fit regression model
fit_mod1 <-lm(PPE_total ~ year_since*decile + pred_mod2 + 
                factor(Dcode) + factor(Fiscalyear), 
              data = df_xy_mod1)


### Get predicted values
df_xy_mod1$exo_mod1 <- predict(fit_mod1, df_xy_mod1)



#################################################
### Model 2. year dummies using "exposure"
#################################################

### Convert to factor
df_xy_mod2 <- df_xy
df_xy_mod2$exposure <- factor(df_xy_mod2$exposure)
df_xy_mod2$exposure <- relevel(df_xy_mod2$exposure, "0")
df_xy_mod2$decile <- factor(df_xy_mod2$decile)
df_xy_mod2$decile <- relevel(df_xy_mod2$decile, "1")

classmode(df_xy_mod2, everything())


### Select only necessary variables
df_xy_mod2 <- df_xy_mod2 %>%
  select(Ccode, Dcode, Fiscalyear, exposure,
         decile, pred_mod2, PPE_total)


### Indicator for valid district (complete cases) => Important for prediction
temp <- df_xy_mod2 %>%
  select(Ccode, Dcode, Fiscalyear, exposure,
         decile, pred_mod2, PPE_total) 

valid_Dcode <- unique(temp$Dcode[complete.cases(temp)])

df_xy_mod2 <- df_xy_mod2 %>% 
  filter(Dcode %in% valid_Dcode)


### Generate model matrix with a large number of dummies
mod_mat <- model.matrix(~ decile*exposure, df_xy_mod2)
write.csv(mod_mat, file = "tables/model_matrix.csv")


### Fit regression model
fit_mod2 <-lm(PPE_total ~ exposure*decile + pred_mod2 + 
                factor(Dcode) + factor(Fiscalyear), 
              data = df_xy_mod2)


### Get predicted values
df_xy_mod2$exo_mod2 <- predict(fit_mod2, df_xy_mod2)



###'######################################################################
###'
###' Merge two predicted values
###'
###'

df_xy_pred <- df_xy_mod1 %>%
  select(Ccode, Dcode, Fiscalyear, year_since, exo_mod1) %>%
  left_join(df_xy_mod2, by = c("Ccode", "Dcode", "Fiscalyear")) %>%
  select(Ccode, Dcode, decile, pred_mod2, 
         Fiscalyear, year_since, exposure, 
         PPE_total, exo_mod1, exo_mod2)



###'######################################################################
###'
###' Visualize the differences between:
###' 
###' (1) Counterfactual prediction 
###' (2) Actual
###' (3) Exogenous prediction_model 1
###' (4) Exogenous prediction_model 2
###'
###'

### Gather as long data format
df_plot <- df_xy_pred %>% 
  gather(key = variable, value = value, 
         PPE_total, exo_mod1, exo_mod2) 


### Generate factor
classmode(df_plot, everything())
df_plot <- df_plot %>%
  mutate(variable = factor(variable, 
                           levels = c("PPE_total", 
                                      "exo_mod1", "exo_mod2"), 
                           labels = c("Actual", 
                                      "LCFF-induced 1", "LCFF-induced 2")))


# ### Generate data frame for plotting
# df_plot_mean <- df_plot %>%
#   group_by(Fiscalyear, decile, variable) %>%
#   summarise(mean_value = mean(value, na.rm = TRUE))


### Randomly select 4 districts
Dcode_sample <- sample(unique(df_plot$Dcode), 4)
df_plot_samp <- df_plot %>%
  filter(Dcode %in% Dcode_sample)
  

### Plot!
p <- plot_trend_grp(df_plot_samp, Fiscalyear, value, variable, 
                    yline = 2013) + facet_wrap(~Dcode)

print(p)



###'######################################################################
###'
###' Save the resulting data
###'
###'

setwd(work_dir)
save(df_xy_pred, 
     file = "processed_data/LCFF_induced_exogenous_PPE_01_Total Expenditures.rda")
write.dta(df_xy_pred, 
          file = "processed_data/LCFF_induced_exogenous_PPE_01_Total Expenditures.dta")

