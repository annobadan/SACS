
###'######################################################################
###'
###' Construct the simulated instrumental variable (Z_{d})
###' 
###' : Creating measures of dosage based on PREREFORM characteristics
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


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import dataset
###'
###' : LCFF Funding Snapshot - FY1314, 1415, 1516, 1617, 1718
###'
###'

### Load the dataset
load(file = "processed_data/funding_snapshot_13to17.rda")


### Assign brief name
df <- funding_snapshot_13to17



###'######################################################################
###'
###' Subset only FY1314
###' 
###' - Basic strategy:
###'   Correlating outcomes with only 
###'   the PREDICTED reform-induced variation in spending
###'   rather than all ACTUAL funding over years (FY1314-1617)
###' 
###' - We predict dosage in district d using only characteristics of the district
###'   PRIOR to the initial LCFF. 
###'
###' - This excludes any changes in school spending that might have been caused by
###'   other district-level changes that could directly affect student outcomes.
###'   
###'   : i.e. removes the confounding influence of unobserved factors
###'     that may determine actual school spending and affect student outcomes
###'
###'

### Filter only school districts in FY1314
df_1314 <- df %>%
  filter(LEA_type == "School District") %>%
  filter(Fiscalyear == 2013)



###'######################################################################
###'
###' Select only necessary variables & reshape to wide format
###'
###'

### Prune FY1314 long data
df_1314_long <- df_1314 %>%
  select(-Scode, -County, -Charter_Num, -LEA_type) %>%  # drop unnecessary variables
  filter(category == "LCFF Target Entitlement") %>%  # filter only target entitlement
  select(-category)


### Rename factors for "variable"
table(df_1314_long$variable)

variable <- unique(df_1314_long$variable)
key <- c("Base", "Supplemental", "Concentration", "Small_School", "Add_On", "Total_Target")   

rename_df <- tibble(variable, key)
df_1314_long <- left_join(df_1314_long, rename_df, by = c("variable"))


### Reshape to wide format
df_1314_wide <- df_1314_long %>% 
  select(Ccode, Dcode, LEA, starts_with("ADA"), UPP, key, value) %>%
  spread(key, value) %>%
  select(Ccode:UPP, Base, Supplemental, Concentration, Small_School, Add_On, Total_Target)



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

### Generate simulated IV and formula weight
df_1314_SimIV <- df_1314_wide %>%
  mutate(formula_weight = 0.2 * UPP + ifelse(UPP > 0.55, 0.5 * (UPP - 0.55), 0), 
         SimSupp = Base * 0.2 * UPP, 
         SimConc = ifelse(UPP > 0.55, 0.5 * Base * (UPP - 0.55), 0),
         SimIV = SimSupp + SimConc, 
         ActualIV = Supplemental + Concentration) %>%
  select(Ccode:UPP, formula_weight,  
         Base, Supplemental, SimSupp, Concentration, SimConc, ActualIV, SimIV, 
         everything()) %>%
  mutate_at(.vars = vars(Base, SimSupp, SimConc, SimIV), 
            .funs = function(x){round(x, 0)})


### Are the simulated values consistent with the provided values?
df_nonzeroBase <- df_1314_SimIV %>% filter(Base != 0)
all.equal(df_nonzeroBase$SimSupp, df_nonzeroBase$Supplemental)
all.equal(df_nonzeroBase$SimConc, df_nonzeroBase$Concentration)
all.equal(df_nonzeroBase$SimIV, df_nonzeroBase$ActualIV)


###' Check the distribution of funding formula weights
ggplot(df_1314_SimIV, aes(formula_weight)) + geom_density()


###' Generate variables for categorizing simulated IV
###' quartile, quintile, decile
df_1314_SimIV <- df_1314_SimIV %>%
  mutate(two_quantile = ntile(SimIV, 2), 
         quartile = ntile(SimIV, 4), 
         quintile = ntile(SimIV, 5), 
         decile = ntile(SimIV, 10))



###'######################################################################
###'
###' Save the resulting dataframe
###' 
###' 

setwd(work_dir)
save(df_1314_SimIV, file = "processed_data/df_1314_SimIV.rda")


