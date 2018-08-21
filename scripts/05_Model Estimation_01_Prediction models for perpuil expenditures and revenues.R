
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
library(car)


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
###' *** The meaning of the simulated IV
###' => The reform-induced changes in district spending
###' => Supplemental and concentration grant
###' 
###' Importantly, these reform-induced changes in district spending, 
###' which are credibly identified from the funding formula 
###' (and which serve as the instrumental variables), 
###' are unrelated to changes in child family and neighborhood characteristics 
###' conditional on the baseline level of disadvantage in each district.
###' (After controlling for UPP => Conditional exogeneity)
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


### Round the simulated values for the visual comparison
cols_to_round <- which(names(df) %in% c("Base", "SimSupp", "SimConc", "SimIV"))

df <- df %>% 
  mutate_at(cols_to_round, round, 0)


### Are the simulated values consistent with the provided values?
df_nonzeroBase <- df %>% 
  filter(Base != 0)

all.equal(df_nonzeroBase$SimSupp, df_nonzeroBase$Supplemental)
all.equal(df_nonzeroBase$SimConc, df_nonzeroBase$Concentration)
all.equal(df_nonzeroBase$SimIV, df_nonzeroBase$Supp_Conc)


### Check the distribution of funding formula weights
ggplot(df, aes(formula_weight)) +
  geom_density()



###'######################################################################
###'
###' The first stage model predicting average per-pupil spending
###' 
###' in district "d" in year "t"
###'
###'
###'

### Merge FY1314 funding snapshot to per-pupil expenditure panel data

assign(names(list_expenditures_def_1)[[1]], list_expenditures_def_1[[1]])

df <- df %>%
  mutate(Ccode = as.numeric(CountyCode), 
         Dcode = as.numeric(DistrictCode)) %>%
  select(Ccode, Dcode, everything()) %>%
  select(-(CountyCode:DistrictCode))

total_exp <- left_join(total_exp, df, by = c("Ccode", "Dcode"))


###  

p_trend <- scatterplot(sum_value_PP_16 ~ Fiscalyear, 
                       boxplots = FALSE, 
                       smooth = TRUE, 
                       reg.line = FALSE, 
                       data = total_exp)




















