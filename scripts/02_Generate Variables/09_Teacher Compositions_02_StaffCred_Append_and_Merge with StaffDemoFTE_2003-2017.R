
###'######################################################################
###'
###' Generate Variables
###' 
###' School-level Teacher Compositions 2003-2017
###' 
###' - Merge "df_Teacher_Composition_DemoFTE_2003_2017" with 
###'  "df_Staff_Cred_2003_2017" data
###' 
###' 
###' 20181127 JoonHo Lee
###' 20181223 JoonHo Lee - Update
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
data_dir <- c("D:/Data/LCFF/Staff_Data/Certificated_Staff/Staff_Demographic")


### Call libraries
library(tidyverse)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import datasets to append & merge
###'
###'

setwd(data_dir)

### df_StaffCred_2003_2008.rda
load(file = "df_StaffCred_2003_2008_by_Subject.rda")
df_StaffCred0308 <- df_to_save


### df_StaffCred_2012_2017.rda
load(file = "df_StaffCred_2012_2017_by_Subject.rda")
df_StaffCred1217 <- df_to_save


### df_Teacher_Composition_DemoFTE_2003_2017.rda
load(file = "df_Teacher_Composition_DemoFTE_2003_2017.rda")
df_DemoFTE <- df_to_save



###'######################################################################
###'
###' Append df_StaffCred_2003-2008 and df_StaffCred_2012-2017
###'
###'

all.equal(names(df_StaffCred0308), names(df_StaffCred1217))

df_StaffCred0317 <- bind_rows(df_StaffCred0308, df_StaffCred1217) %>%
  arrange(CountyCode, DistrictCode, SchoolCode, Unified_Name, AcademicYear)

dualsave(df_StaffCred0317, "df_Teacher_Composition_StaffCred_2003-2008 and 2012-2017")



###'######################################################################
###'
###' Covert to wide dataset
###'
###'

### Check distribution of Unified_Name
tabdf(df_StaffCred0317, Unified_Name)


### Filter only necessary subjects
subject_vec <- c("Total", 
                 "Elementary/Self-Contained Classroom/Multiple Subject", 
                 "Mathematics", 
                 "English", 
                 "English Language Development (ELD) ONLY", 
                 "Specially Designed Academic Instruction in English (SDAIE) ONLY", 
                 "English Language Development (ELD) AND Specially Designed Academic Instruction in English (SDAIE)", 
                 "Primary Language Instruction (BCLAD or equivalents) and SDAIE and ELD", 
                 "Reading Specialist/Certificate", 
                 "Languages Other Than English", 
                 "Social Science", 
                 "Special Education", 
                 "Career Technical Education/Vocational")


df_StaffCred0317_sub <- df_StaffCred0317 %>%
  filter(Unified_Name %in% subject_vec)


### Convert to wide dataset
subj_label_vec <- c("Total", "SelfCon", "Math", 
                    "English", "ELD", "SDAIE", "ELD_SDAIE", 
                    "BCLAD_ELD_SDAIE", "Reading", "Foreign", 
                    "SocSci", "SPED", "CTE")

varnames <- names(df_StaffCred0317_sub)[-(1:5)]

subj <- rep(subj_label_vec, each = length(varnames))
varnames_vec <- rep(varnames, length(subj_label_vec))

level_vec_extended <- paste(varnames_vec, subj, sep = "_")


df_subj <- data.frame(subject_vec, subj_label_vec)


df_StaffCred0317_sub_wide <- df_StaffCred0317_sub %>%
  gather(key = key, value = value, N_RecID:PCT_Waiver) %>% 
  rename(subject_vec = Unified_Name) %>%
  left_join(df_subj, by = c("subject_vec")) %>%
  dplyr::select(-subject_vec) %>%
  unite(variable, key, subj_label_vec) %>% 
  mutate(variable = factor(variable, levels = level_vec_extended)) %>%
  spread(variable, value)
  


###'######################################################################
###'
###' Merge DemoFTE + StaffCred 2003-2017
###'
###'

df_merge <- df_DemoFTE %>%
  full_join_track(df_StaffCred0317_sub_wide, 
                  by = c("CountyCode", "DistrictCode", 
                         "SchoolCode", "AcademicYear"), 
                  .merge = FALSE)


dualsave(df_merge, "df_Teacher_Composition_DemoFTE_StaffCred_2003_2017")


