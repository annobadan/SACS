
###'######################################################################
###'
###' Model Estimation
###' 
###' (1) Prediction models for the per-pupil expenditures/revenues
###' 
###'     => first-stage model of IV
###'     
###'     
###' Replicate/evaluate the Johnson & Tanner's research design
###' 
###' (1) Event study (interrupted time series)
###' 
###' 
###' (2) A simulated instrumental variable approach 
###'     using the LCFF funding formula
###' 
###' 
###' 20180813 JoonHo Lee
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


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import pre-cleaned datasets
###'
###'

### Per-pupil expenditures & revenues
load(file = "processed_data/list_expenditures_def_1_2.RData")
load(file = "processed_data/list_revenues.rda")


### LCFF funding snapshot data: FY13-14, 14-15, 15-16
load(file = "processed_data/funding_snapshot_3year.RData")



###'######################################################################
###'
###' Construct the simulated instrumental variable (Z_{d})
###' 
###' (1) Per-pupil supplemental grant: Supp_{d} = Base_{d} * 0.2 * UPP
###' 
###' (2) Per-pupil concentration grant: Conc_{d} = Base_{d} * max[UPP - 0.55, 0]
###' 
###' (3) Simulated instrumental variable: SimIV_{d} = Supp_{d} + Conc_{d} 
###' 
###' => The state's allocation of Supplemental and Concentration Grants is
###'    the focal point of our use of the funding formula 
###'    to isolate exogenous changes in district-level revenue 
###'    caused by the state policy change
###'
###' (4) Formula weight: 0.20 * UPP + max[UPP - 0.55, 0]
###'
###'

### Remove charter school entries
df <- fund1314 %>%
  filter(LEA_type == "School District")


### Generate simulated IV and formula weight
df <- df %>%
  mutate(formula_weight = 0.2 * Unduplicated + 
           ifelse(Unduplicated > 0.55, 0.5 * (Unduplicated - 0.55), 0), 
         SimSupp = Base * 0.2 * Unduplicated, 
         SimConc = ifelse(Unduplicated > 0.55, 0.5 * Base * (Unduplicated - 0.55), 0),
         SimIV = SimSupp + SimConc, 
         Supp_Conc = Supplemental + Concentration) %>%
  select(CountyCode, DistrictCode, LEA, Total_ADA, Unduplicated, formula_weight,  
         Base, Supplemental, SimSupp, Concentration, SimConc, Supp_Conc, SimIV, 
         everything())























