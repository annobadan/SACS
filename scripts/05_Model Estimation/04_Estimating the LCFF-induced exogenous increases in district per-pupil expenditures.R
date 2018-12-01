
###'######################################################################
###'
###' Step 2. Estimating the LCFF-induced exogenous increases 
###'         in district per-pupil expenditure
###' 
###' - The first stage model of 2SLS-IV
###'   
###'   
###' 20181029 JoonHo Lee
###' 20181112 JoonHo Lee
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
data_dir <- c("~/SACS/processed_data")


### Call libraries
library(tidyverse)
library(lme4)
library(foreign)
library(broom)


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
###' Prepare loop
###'
###'

### Extract the names of per-pupil expenditure dataframes
PPE_vec <- names(list_expenditures_def_1)


### Define an empty list to collect the resulting dataframes
list_PPE_exogenous <- list()


### Start a loop
for (i in seq_along(PPE_vec)){
  
  ### Reset loop
  setwd(work_dir)
  
  
  ###'######################################################################
  ###'
  ###' Extract dataframe from the list
  ###' 
  ###'
  
  df_name <- PPE_vec[i]
  
  df_y <- list_expenditures_def_1[[df_name]]
  
  
  ### Add a factor variable for "total_exp" 
  if (i == 1){
    
    df_y <- df_y %>%
      mutate(total_exp = "Total Expenditures") %>%
      mutate(total_exp = factor(total_exp))
    
  }
  
  
  
  ###'######################################################################
  ###'
  ###' Nest data & select only necessary variables
  ###'
  ###'
  
  ### Rename before nesting
  names(df_y)[names(df_y) %in% df_name] <- c("factor")
  names(df_y)[names(df_y) %in% c("sum_value_PP_16")] <- c("value")
  
  
  ### Nest data
  df_nested <- df_y %>%
    group_by(factor) %>%
    nest(Fiscalyear, Ccode, Dcode, value)

  head(df_nested)
  df_nested[["data"]]
  
  
  
  ###'######################################################################
  ###'
  ###' Delete missing values
  ###'
  ###'
  
  ### Define a temporary function
  listwise_deletion <- function(df){
    idx <- complete.cases(df)
    df <- df[idx,]
    return(df)
  }
  
  ### Apply to each nested list
  df_nested <- df_nested %>%
    mutate(data = map(.x = df_nested$data, .f = listwise_deletion))
  
  head(df_nested)
  
  
  
  ###'######################################################################
  ###'
  ###' Merge outcome variable with predictors
  ###'
  ###'
  
  ### Temporarily unnest for merging
  df_unnested <- df_nested %>% unnest()
  
  
  ### Merge and arrange 
  df_unnested_pred <-  df_unnested %>%
    right_join(df_prepost_extend, by = c("Fiscalyear", "Ccode", "Dcode")) %>%
    arrange(Ccode, Dcode, factor, Fiscalyear) 
  
  
  
  ###'######################################################################
  ###'
  ###' Prepare variables for regression models
  ###'
  ###'
  
  ###' Prepare variants of year dummies 
  ###' (1) years since event
  ###' (2) exposure
  df_unnested_pred <- df_unnested_pred %>%
    mutate(year_since = Fiscalyear - 2013, 
           exposure = if_else(Fiscalyear > 2012, 
                              Fiscalyear - 2012, 0))
  
  year_vars <- listvars(df_unnested_pred, Ccode, Dcode, 
                        Fiscalyear, year_since, exposure, nrow = 100)
  

  ### Convert predictors to factor format
  df_xy_mod2 <- df_unnested_pred
  df_xy_mod2$exposure <- factor(df_xy_mod2$exposure)
  df_xy_mod2$exposure <- relevel(df_xy_mod2$exposure, "0")
  df_xy_mod2$decile <- factor(df_xy_mod2$decile)
  df_xy_mod2$decile <- relevel(df_xy_mod2$decile, "1")
  
  classmode(df_xy_mod2, everything())
  
  
  ### Select only necessary variables
  df_xy_mod2 <- df_xy_mod2 %>%
    select(factor, Ccode, Dcode, Fiscalyear, exposure,
           decile, pred_mod2, value)
  
  
  
  ###'######################################################################
  ###'
  ###' Filter only complete cases
  ###'
  ###'
  
  ### Indicator for valid district (complete cases) => Important for prediction
  temp <- df_xy_mod2 %>%
    select(factor, Ccode, Dcode, Fiscalyear, exposure,
           decile, pred_mod2, value) 
  
  valid_Dcode <- unique(temp$Dcode[complete.cases(temp)])
  
  df_xy_mod2 <- df_xy_mod2 %>% 
    filter(Dcode %in% valid_Dcode)
  
  
  ### Generate model matrix with a large number of dummies
  mod_mat <- model.matrix(~ decile*exposure, df_xy_mod2)
  # write.csv(mod_mat, file = "tables/model_matrix.csv")
  
  
  ###' Remove factor that has only "0" values for exposure
  ###' which means no available observations from 2013
  temp <- df_xy_mod2 %>%
    group_by(factor) %>%
    summarise(N_exposure = length(unique(exposure)))
  
  factor_to_remove <- as.vector(temp$factor[temp$N_exposure == 1])
  
  df_xy_mod2 <- df_xy_mod2 %>%
    filter(!factor %in% factor_to_remove) %>%
    filter(!is.na(factor))

  
  
  ###'######################################################################
  ###'
  ###' Fit prediction model: 
  ###' 
  ###' Model 2. year dummies using "exposure"
  ###' 
  ###' (1) year dummies: exposure (0 ... 0, 1, 2, 3, 4)
  ###' (2) simulated IV decile dummies
  ###' (3) predicted counterfactual trend of district per-pupil revenues (pred_mod2)
  ###' (4) district fixed effects
  ###' (5) year fixed effects
  ###'   
  ###' 
  
  ### Nest the prepared dataset
  df_nested_pred <- df_xy_mod2 %>%
    group_by(factor) %>%
    nest()
  
  head(df_nested_pred)
  
  
  ### Fit regression model to each tibble
  fit_mod2 <- df_nested_pred %>%
    mutate(model = map(.x = df_nested_pred$data, 
                       ~lm(value ~ exposure*decile + pred_mod2 + 
                             factor(Dcode) + factor(Fiscalyear), 
                           data = .x)))
  
  
  ### Get predicted values by "augment"
  df_augmented <- fit_mod2 %>%
    mutate(exo_mod2 = map(model, ~augment(.x)))

  head(df_augmented)

  
  ### Unnest the predicted dataframe
  df_augmented_unnested <- df_augmented %>%
    unnest(exo_mod2)
  
  
  ### Rename variables
  names(df_augmented_unnested)
  df_augmented_unnested <- df_augmented_unnested %>%
    rename(Dcode = factor.Dcode., 
           Fiscalyear = factor.Fiscalyear.)
  
  
  
  ###'######################################################################
  ###'
  ###' Save the resulting dataframe
  ###'
  ###'
  
  setwd(data_dir)
  
  df <- df_augmented_unnested
  
  
  ### Save as an independent dataframe
  dualsave(df, paste0("LCFF_induced_exogenous_PPE_", 
                      sprintf("%02d", i), "_", 
                      df_name))
  
  
  ### Embed in the prepared list
  list_PPE_exogenous[[i]] <- df
  names(list_PPE_exogenous)[i] <- df_name
  
  
  
  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0(df_name, " ", sprintf("%02d", i), " completed", "\n"))
  
  
}  ### End of loop


###'######################################################################
###' 
###' Save the resulting list
###' 
###' 

setwd(data_dir)
save(list_PPE_exogenous, file = "list_PPE_exogenous.rda")



###'######################################################################
###' 
###' Generate dataframe containing actual and predicted per-pupil expenditures  
###' 
###' 

### Bind rows
df_bind <- bind_rows(list_PPE_exogenous)


### Define factor levels
levels <- lapply(list_PPE_exogenous, function(df) unique(df$factor))
factor_levels <- as.vector(unlist(levels))
df_bind <- df_bind %>% 
  mutate(factor = factor(factor, levels = factor_levels))
classmode(df_bind, everything())  


### Save as an independent dataframe
dualsave(df_bind, paste0("LCFF_induced_exogenous_PPE_2003_2016"))


