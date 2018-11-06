
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
library(arrayhelpers)


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


###' District information on actual/predicted per-pupil expenditures
###' LCFF-induced exogenous increases in district per-pupil expenditures
load(file = "processed_data/LCFF_induced_exogenous_PPE_01_Total Expenditures.rda")
df_pred <- df_xy_pred



###'######################################################################
###'
###' Number of schools within district
###'
###'

### Generate dataframe
df_NSchools <- df %>% 
  group_by(CountyCode, DistrictCode, DistrictName, DOC) %>%
  summarise(NSchools = n_distinct(SchoolCode))

tabdf(df_NSchools, DOC)
tabdf(df_NSchools, NSchools)
tabdf_plot(df_NSchools, NSchools, limits = c(0, 100))


### Unified districts
df_NSchools_Unified <- df_NSchools %>%
  filter(DOC %in% "Unified School District")
tabdf(df_NSchools_Unified, NSchools)
tabdf_plot(df_NSchools_Unified, NSchools, limits = c(0, 100))


### Elementary districts
df_NSchools_Elementary <- df_NSchools %>%
  filter(DOC %in% "Elementary School District")
tabdf(df_NSchools_Elementary, NSchools)
tabdf_plot(df_NSchools_Elementary, NSchools, limits = c(0, 50))


### High districts
df_NSchools_High <- df_NSchools %>%
  filter(DOC %in% "High School District")
tabdf(df_NSchools_High, NSchools)
tabdf_plot(df_NSchools_High, NSchools, limits = c(0, 50))



###'######################################################################
###'
###' Prepare analytical sample
###'
###'

### Select only necessary variables
df_vars <- df %>%
  select(ends_with("Code"), AcademicYear, 
         PCT_New_teach)


# ### Remove districts containing less than 10 schools
# df_vars <- df_vars %>%
#   group_by(CountyCode, DistrictCode) %>%
#   mutate(NSchools = n_distinct(SchoolCode)) %>%
#   filter(NSchools >= 10)

length(unique(df_vars$DistrictCode))



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

### Define "clusters": District-by-Year
district_vec <- as.vector(unique(df_vars$DistrictCode))
year_vec <- sort(as.vector(unique(df_vars$AcademicYear)))


### Define quantile grid
quantile_grid <- seq(from = 0.2, to = 0.8, by = 0.1)


### Define empty array to collect coefficients
tau_array <- array(NA, dim = c(length(district_vec), 
                               length(year_vec), 
                               length(quantile_grid)))  

colnames(tau_array) <- paste0("Yr", year_vec)
rownames(tau_array) <- district_vec


### Calculate conditional quantiles for each distirct by year cluster
for (i in seq_along(district_vec)){      # (1) Loop over districts
  for (j in seq_along(year_vec)){        # (2) Loop over years
    for (k in seq_along(quantile_grid)){ # (3) Loop over quantile grids
      
      ### Subset dataframe for district i and year j
      df_sub <- df_vars %>%
        filter(DistrictCode == district_vec[i]) %>%
        filter(AcademicYear == year_vec[j])
      
      
      ### Generate conditions to skip analysis
      cond_skip <- sum(!complete.cases(df_sub)) == nrow(df_sub)
      
      
      if (cond_skip == TRUE){
        
        tau_array[i, j, k] <- NA
      
      } else if (cond_skip == FALSE){
        
        ### Fit the standard quantile regression model
        qr_fit <- rq(PCT_New_teach ~ 1, 
                     data = df_sub, 
                     tau = quantile_grid[k])
        
        
        ### Extract unconditional quantile
        tau_array[i, j, k] <- as.vector(qr_fit[["coefficients"]][1])
        
      }
    }
    
    ### Print the progress
    cat(paste0(" District ", i, " Year ", j, " completed", "\n"))
    
  }
}  # End of loop



###'######################################################################
###'
###' Tidy up the resulting array
###' 
###' - Convert array into the long dataset
###'
###'

### Convert multidimensional array into long form data frame
tau_df <- array2df(tau_array, 
                   levels = list(Dcode = district_vec, 
                                 Fiscalyear = year_vec + 2000, 
                                 quantile = quantile_grid), 
                   label.x = "value")


### Reorder rows and columns
tau_df <- tau_df %>%
  select(Dcode, quantile, Fiscalyear, value) %>%
  arrange(Dcode, quantile, Fiscalyear) %>%
  mutate(Dcode = as.numeric(as.character(Dcode)), 
         Fiscalyear = as.numeric(as.character(Fiscalyear)))



###'######################################################################
###'
###' Plot by Simulated IV
###'
###'

### Merge simulated IV
classmode(tau_df, everything())
classmode(df_pred, everything())

tau_df <- tau_df %>%
  left_join(df_pred, by = c("Fiscalyear", "Dcode"))


### Plot
tau_df_sum <- tau_df %>%
  group_by(Fiscalyear, quantile, decile) %>%
  summarise(mean_value = mean(value, na.rm = TRUE))

plot_trend_grp(tau_df_sum, Fiscalyear, mean_value, quantile,
               yline = c(2013)) + facet_wrap(.~ decile)


tabdf(tau_df, decile)



# ###'######################################################################
# ###'
# ###' Plot by 15 years average FRPM
# ###'
# ###'
# 
# ### Merge 15 years average FRPM
# names(df)
# tabdf(df, PCT_FRPM_15bin)
# df_FRPM_Dmean <- df %>%
#   group_by(CountyCode, DistrictCode) %>%
#   summarise(PCT_FRPM_Dmean = mean(PCT_FRPM_15mean, na.rm = TRUE)) %>%
#   mutate(PCT_FRPM_Dmean_bin = ntile(PCT_FRPM_Dmean, 5))
# 
# tau_df <- tau_df %>%
#   left_join(df_FRPM_Dmean, by = c("Dcode" = "DistrictCode"))
# 
# 
# ### Plot
# tau_df_sum <- tau_df %>%
#   group_by(Fiscalyear, quantile, PCT_FRPM_Dmean_bin) %>%
#   summarise(mean_value = mean(value, na.rm = TRUE))
# 
# plot_trend_grp(tau_df_sum, Fiscalyear, mean_value, quantile, yline = c(2013)) + 
#   facet_wrap(.~ PCT_FRPM_Dmean_bin)



###'######################################################################
###'
###' Step 3. Group-level 2SLS estimation
###' 
###' (ii) regress the estimated group-specific effects on group-level covariates 
###'      using either 2SLS, if the group-level covariates are endogenous, 
###'      or OLS, if the group-level covariates are exogenous, 
###'      either of which cases would render standard quantile regression inconsistent. 
###'      
###'      

### Merge two sources of information
classmode(tau_df, everything())
classmode(df_pred, everything())

df_tau_pred <- tau_df %>%
  left_join(df_pred, by = c("Dcode", "Fiscalyear"))


### Prepare empty dataframe to collect estimates
collect_est <- data.frame()


### Fit the 2nd stage model of 2SLS-IV regression

for (i in seq_along(quantile_grid)){
  
  ### Subset the estimated values for each quantile
  df_for_model <- df_tau_pred %>%
    filter(quantile == quantile_grid[i])
  
  
  ### Fit the 2nd stage model of 2SLS-IV
  fit_step3 <- lm(value ~ log(exo_mod1) + pred_mod2 +  
                    factor(Dcode) + factor(Fiscalyear), 
                  data = df_for_model)
  
  
  ### Extract the estimated effect of predicted PPE
  ppe_effect <- summary(fit_step3)$coefficients[2, ]
  collect_est <- bind_rows(collect_est, ppe_effect)

}


### Plot the collected estimates
collect_est$tau <- factor(paste0("tau = ", quantile_grid))
names(collect_est) <- c("est", "se", "t_val", "p_val", "tau")


p <- ggplot(data = collect_est, 
            aes(x = tau, y = est)) +
  geom_pointrange(aes(ymin = est - se*1.96, 
                      ymax = est + se*1.96), 
                  position = position_jitter(width = 0.1, height = 0)) + 
  geom_hline(yintercept = 0, size = 0.5, col = "red") + 
  theme_bw()




