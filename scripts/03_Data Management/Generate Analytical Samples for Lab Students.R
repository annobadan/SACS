
###'######################################################################
###'
###' Generate analytical samples for Lab students
###'
###' 
###' 20181122 JoonHo Lee
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


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import precleaned datasets
###'
###'

setwd(work_dir)


### Student compositions + Teacher compositions 2003-2017
load(file = "processed_data/df_Teacher_Student_K12_character.rda")
df <- df_to_save


### Only student compositions
load("processed_data/df_student_composition_0317.rda")
df_std <- df_student_composition


### PCT_EL
load("processed_data/Reclass_Rate_by_School_2005-2017.rda")
df_EL <- df_to_save


### Smarter Balanced Assessment Data
load(file = "processed_data/sba_All_3years.rda")
df_sba <- sba_All_3years


### df_student_teacher_compositions 2003-2017
load(file = "processed_data/df_student_teacher_composition_2003_2017.rda")
df_std_tch_comp <- df_to_save



###'######################################################################
###'
###' Morgan Bessette
###'
###'

match_key <- c("CountyCode", "DistrictCode", "SchoolCode", "AcademicYear")


### Extract 1) basic information, 2) outcome, and 3) main predictor, 4) Teacher control
df_Morgan <- df %>%
  select(match_key,  
         ends_with("Name"), DOC, SOC, EIL, 
         GSoffered, GSserved, Charter, 
         Total_Enroll, 
         TA_Rate_Susp, mean_yrs_teach, 
         PCT_Master_Plus, mean_FTE_teacher)

names(df_Morgan)

### Extract necessary student composition variables
names(df_std)
df_std_vars <- df_std %>%
  select(match_key,  
         PCT_Latino, PCT_Black, PCT_White, PCT_FRPM)


### Extract PCT_EL
names(df_EL)
df_std_vars_EL <- df_EL %>%
  select(match_key,   
         PCT_EL_Total) %>%
  mutate(AcademicYear = AcademicYear - 2000)


### Merge them all
df_Morgan_merged <- df_Morgan %>%
  left_join(df_std_vars, by = match_key) %>%
  left_join(df_std_vars_EL, by = match_key) 


### Delete missing values
idx <- complete.cases(df_Morgan_merged$TA_Rate_Susp)
df_Morgan_final <- df_Morgan_merged[idx, ]


### Save the resulting data
dualsave(df_Morgan_final, "processed_data/Lab_students/data_Morgan")



###'######################################################################
###'
###' Jesus Camacho
###'
###'

### Extract basic information
df_Jesus <- df %>%
  select(match_key,  
         ends_with("Name"), DOC, SOC, EIL, 
         GSoffered, GSserved, Charter, 
         Total_Enroll, 
         PCT_Dropout_Total)

names(df_Jesus)


### Merge with student composition data
df_Jesus_merged <- df_Jesus %>%
  left_join(df_std_vars, by = match_key)


### Delete missing values
idx <- complete.cases(df_Jesus_merged$PCT_Dropout_Total)
df_Jesus_final <- df_Jesus_merged[idx, ]


### Save the resulting data
dualsave(df_Jesus_final, "processed_data/Lab_students/data_Jesus")



###'######################################################################
###'
###' Nicholas Saldivar
###'
###'

### Extract basic information
df_Nic <- df_std_tch_comp %>%
  select(match_key,  
         ends_with("Name"), DOC, SOC, EIL, 
         GSoffered, GSserved, Charter, 
         Total_Enroll, PCT_Latino, PCT_Black, PCT_White, PCT_FRPM, 
         mean_yrs_teach, PCT_New_teach, PCT_Master_Plus)

names(df_Nic)


### SBA scores
df_sba_math <- sba_All_3years %>%
  filter(Grade == "All Grades") %>% 
  filter(TestSubject == "Mathematics") %>% 
  mutate(AcademicYear = TestYear - 2000 - 1) %>%
  select(match_key, 
         Students_Tested, 
         PCT_Stdrd_Met_and_Above) %>%
  rename(PCT_Stdrd_Met_and_Above_Math = PCT_Stdrd_Met_and_Above) %>%
  arrange(CountyCode, DistrictCode, SchoolCode, AcademicYear)


df_sba_ELA <- sba_All_3years %>%
  filter(Grade == "All Grades") %>% 
  filter(TestSubject == "English Language Arts/Literacy") %>% 
  mutate(AcademicYear = TestYear - 2000 - 1) %>%
  select(match_key, 
         PCT_Stdrd_Met_and_Above) %>%
  rename(PCT_Stdrd_Met_and_Above_ELA = PCT_Stdrd_Met_and_Above) %>%
  arrange(CountyCode, DistrictCode, SchoolCode, AcademicYear)


### Merge them all
df_Nic_merged <- df_Nic %>%
  # left_join(df_std_vars, by = match_key) %>%
  left_join(df_sba_math, by = match_key) %>%
  left_join(df_sba_ELA, by = match_key)


### Delete missing values
idx <- complete.cases(df_Nic_merged$PCT_Stdrd_Met_and_Above_ELA)
df_Nic_final <- df_Nic_merged[idx, ]


### Save the resulting data
dualsave(df_Nic_final, "processed_data/Lab_students/data_Nic")

