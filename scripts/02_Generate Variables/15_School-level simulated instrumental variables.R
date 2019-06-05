
###'######################################################################
###'
###' Task    : Generate School-level Simulated Instrumental Variable
###' 
###'           "Predicted" or "Intended" amount of LCFF funding at school-level
###'           
###'                      
###' Category: Generate variables
###' 
###' Data    : df_Ultimate_Merged
###'            
###' Date    : 2019-06-04
###'           
###' Author  : JoonHo Lee (joonho@berkeley.edu)
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
data_folder <- c("D:/OneDrive/Data")
data_dir <- file.path(data_folder, "LCFF")


### Call libraries
library(tidyverse)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Read in df_Ultimate_Merged dataset
###'
###'

### Load the df_Ultimate_Merged dataset
setwd(data_dir)
load(file = "df_Ultimate_Merged.rda")
df <- df_to_save; rm(df_to_save)



###'######################################################################
###'
###' Subset only necessary variables and observations 
###' to generate the simulated IV variables
###'
###' => Exclude Non-Traditional Schools
###' 
###' * LCFF funding formula applies to school districts and charter schools
###'
###'

df_sub <- df %>%
  filter(!is.na(Traditional_SOC)) %>%
  select(CountyCode, DistrictCode, SchoolCode, AcademicYear, 
         Traditional_SOC, Charter, 
         Total_Enroll, N_KDGN:N_ADULT, PCT_CALPADS_UPC)



###'######################################################################
###'
###' Generate grade span variables
###' 
###' - K-3: $6,845
###' - 4-6: $6,947   
###' - 7-8: $7,154   
###' - 9-12: $8,289
###' 
###' 

names(df_sub)

df_sub <- df_sub %>%
  mutate(N_GR_Kto3 = N_KDGN + N_GR_1 + N_GR_2 + N_GR_3, 
         N_GR_4to6 = N_GR_4 + N_GR_5 + N_GR_6, 
         N_GR_7to8 = N_GR_7 + N_GR_8, 
         N_GR_9to12 = N_GR_9 + N_GR_10 + N_GR_11 + N_GR_12, 
         N_GR_Total = N_GR_Kto3 + N_GR_4to6 + N_GR_7to8 + N_GR_9to12 + 
           N_UNGR_ELM + N_UNGR_SEC)  # Exclude N_Adult 

df_temp <- df_sub %>%
  select(Traditional_SOC, Total_Enroll, N_GR_Total) %>%
  mutate(diff = Total_Enroll - N_GR_Total) 

tabdf(df_temp, diff)



###'######################################################################
###'
###' Calculate the (predicted school-level) "Base" Grant first 
###' 
###' (1) Target base rates (per ADA)   
###' 
###' - K-3: $6,845
###' - 4-6: $6,947   
###' - 7-8: $7,154   
###' - 9-12: $8,289
###' 
###' 
###' (2) Base rate adjustments   
###' - K-3: 10.4 percent of base rate   
###' - 9-12: 2.6 percent of base rate
###' 
###' (3) What to do with the "ungraded" enrollment?
###'     Error would be significant if we miss any count of students. 
###'     Reflect ungraded categories anyway.  
###'     
###' - N_UNGR_ELM: Average of K-3 and 4-6 = $6,896
###' - N_UNGR_SEC: Average of 7-8 and 9-12 = $7,721.5  
###'
###'

df_sub <- df_sub %>%
  mutate(Base = 6845*N_GR_Kto3*1.104 + 
                6947*N_GR_4to6 +
                7154*N_GR_7to8 + 
                8289*N_GR_9to12*1.026 + 
                6896*N_UNGR_ELM + 
                7721.5*N_UNGR_SEC) 



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

### Generate UPP from PCT_CALPADS_UPC
summary(df_sub$PCT_CALPADS_UPC)

df_sub <- df_sub %>%
  mutate(UPP = PCT_CALPADS_UPC/100)

names(df_sub)


### Generate simulated IV and formula weight
df_simIV <- df_sub %>%
  mutate(formula_weight = 0.2 * UPP + ifelse(UPP > 0.55, 0.5 * (UPP - 0.55), 0), 
         Supplemental = Base * 0.2 * UPP, 
         Concentration = ifelse(UPP > 0.55, 0.5 * Base * (UPP - 0.55), 0),
         SimIV = Supplemental + Concentration) 

apply(df_simIV %>% select(Base, Supplemental, Concentration), 2, summary)



###'######################################################################
###'
###' Generate Per-pupil quantities
###'
###' by dividing them with N_GR_Total, Not Total_Enroll
###'
###' Because Total_Enroll may contains "N_Adult"
###'
###'

df_simIV <- df_simIV %>% 
  mutate(Base_PP = Base/N_GR_Total, 
         Supplemental_PP = Supplemental/N_GR_Total, 
         Concentration_PP = Concentration/N_GR_Total, 
         SimIV_PP = SimIV/N_GR_Total)

apply(df_simIV %>% select(contains("_PP")), 2, summary) 



###'######################################################################
###'
###' Generate CPI-U deflated per-pupil values
###' 
###' and finalize the dataset to export
###'
###'

### Select only necessary variables and reorder them
names(df_simIV)

df_final <- df_simIV %>%
  select(CountyCode, DistrictCode, SchoolCode, AcademicYear, 
         Traditional_SOC, Charter, 
         UPP, formula_weight, 
         Base, Supplemental, Concentration, SimIV, 
         Base_PP, Supplemental_PP, Concentration_PP, SimIV_PP)

names(df_final)


### Convert to long data format
df_final_long <- df_final %>%
  gather(key = key, value = value, Base:SimIV_PP, 
         factor_key = TRUE)


### Identify and remove duplicated rows
nrow(df_final_long)        # 1016120
n_distinct(df_final_long)  # 1016072

df_final_long <- df_final_long %>%
  distinct()

nrow(df_final_long)


### Convert to 2016 dollars
df_final_long <- CPI_converter(data = df_final_long, 
                               year_to = 2016, 
                               year_from = AcademicYear, 
                               value)


### Reshape to the wide format
tabdf(df_final_long, key)
levels(df_final_long$key)

df_final_wide <- df_final_long %>%
  select(-value) %>%
  spread(key = key, value = value_16)



###'######################################################################
###'
###' Merge with the original df_Ultimate_Merged
###'
###'

### Merge!
match_key <- df_final_wide %>% select(CountyCode:Charter) %>% names()

df_merged <- full_join_track(df, df_final_wide, by = match_key, .merge = TRUE)


###' Investigate unmerged cases
###' Unmerged cases are all non-traditional schools
tabdf(df_merged, .merge)

df_temp <- df_merged %>%
  filter(.merge == "left_only")

df_unmerged <- df_temp %>%
  group_by(SOC, AcademicYear) %>%
  count()


### Remove .merge variable and reorder the merged variables
df_varnames <- data.frame(names(df_merged))

df_merged <- df_merged %>%
  select(-.merge) %>%
  select(CDSCode:PCT_CALPADS_UPC, UPP:SimIV_PP, everything())


### Save the resulting dataframe
setwd(data_dir)
dualsave(df_merged, "df_Ultimate_Merged")


### Replace the df_Ultimate_SchoolInfo
df_schoolinfo <- df_merged %>%
  select(CDSCode:SimIV_PP)

setwd(data_dir)
dualsave(df_schoolinfo, "df_Ultimate_SchoolInfo")
