
###'######################################################################
###'
###' Generate Variables
###' 
###' School-level Student Compositions 
###' 
###' (6) Suspension and Expulsion 
###' 
###' - 6.2. Expulsion 
###' 
###' 
###' 20181107 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character/05_Expulsion_Suspension/Expulsion")


### Call libraries
library(tidyverse)
library(foreign)
library(rlang)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Loop over years
###'
###' 

setwd(data_dir)

years <- c(sprintf("%02d", seq(11, 16)))

meta_list <- list()


for (i in seq_along(years)){
  
  
  ###'######################################################################
  ###'
  ###' Load dataset: FEP by Grade and Language
  ###'
  ###'
  
  year_num <- years[i]
  
  year_acad <- paste0(year_num, as.numeric(year_num) + 1)
  
  load(file = paste0("exp", year_acad, "_cleaned", ".rda"))
  
  
  
  ###'######################################################################
  ###'
  ###' (1) PCT_Exps_Pupils: 
  ###' 
  ###'   Unduplicated Count of Students expelled / Cumulative Enrollment
  ###'   
  ###'    
  ###' (2) PCT_Exps_Pupils_defy: 
  ###'   
  ###'   Unduplicated Count of Students expelled (Defiance-Only) / Cumulative Enrollment 
  ###'
  ###'
  ###'
  
  classmode(df, everything())
  
  df <- df %>%
    mutate(PCT_Exps_Pupils = 100*(N_pupil_Exps_Undup/Enrollment_Cum), 
           PCT_Exps_Pupils_defy = 100*(N_pupil_Exps_Defy_Undup/Enrollment_Cum))
  
  
  
  ###'######################################################################
  ###'
  ###' Proportions of each expulsion type
  ###' 
  ###' => divide the count of each expulsion type by Total count of expulsions 
  ###' 
  ###'
  ###' (1) PCT_Exps_Injury: expulsion Count Violent Incident (Injury)
  ###' 
  ###' (2) PCT_Exps_NonInjury: expulsion Count Violent Incident (No Injury)
  ###' 
  ###' (3) PCT_Exps_Weapon: expulsion Count Weapons Possession
  ###' 
  ###' (4) PCT_Exps_Drug: expulsion Count Illicit Drug-Related
  ###' 
  ###' (5) PCT_Exps_Defiance: expulsion Count Defiance-Only
  ###' 
  ###' (6) PCT_Exps_Other: expulsion Count Other Reasons
  ###'
  ###'
  
  df_counts <- df %>%
    select(starts_with("N_Exps")) %>%
    mutate_all(.funs = function(x) 100*(x/df$Total_Exps)) 
  
  names(df_counts) <- gsub("N_", "PCT_", names(df_counts))
  
  df <- df %>%
    cbind.data.frame(df_counts)
  
  
  
  ###'######################################################################
  ###'
  ###' Transform to long data format
  ###'
  ###'
  
  classmode(df, everything())
  
  
  ### Convert Academic Year to numeric
  df$AcademicYear <- 2000 + as.numeric(year_num)
  
  
  ### Delete Errata_Flag
  df <- df %>% select(-Errata_Flag)
  
  
  ### Define factor order 
  df_temp <- df %>% select(Enrollment_Cum:PCT_Exps_Other)
  vars <- names(df_temp)
  
  
  ### Convert to long data format
  df_long <- df %>%
    gather(key = key, value = value, vars) %>%
    mutate(key = factor(key, levels = vars)) %>%
    arrange(Aggregate_Level, CountyCode, DistrictCode, SchoolCode, 
            Category, key)
  
  
  # ### Save the long data
  # dualsave(df_long, paste0("susp", year_acad, "_managed_long"))
  
  
  
  ###'######################################################################
  ###'
  ###' Reshape to wide format
  ###'
  ###'
  
  df_wide <- df_long %>% 
    unite(varname, Category, key) %>% 
    select(-starts_with("Category_")) %>%
    spread(key = varname, value = value, fill = NA)
  
  # dualsave(df_wide, paste0("susp", year_acad, "_managed_wide"))
  
  
  
  ###'######################################################################
  ###'
  ###' Embed into the list
  ###'
  ###'
  
  meta_list[[i]] <- df_wide
  
  
  
  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("Year ", year_num, " completed", "\n"))
  
}



###'######################################################################
###'
###' Append to longitudinal dataset
###'
###'

df_bind <- bind_rows(meta_list)

classmode(df_bind, everything())

df_bind <- df_bind %>%
  select(Aggregate_Level, CountyCode, DistrictCode, SchoolCode, AcademicYear, 
         everything()) %>% 
  arrange(Aggregate_Level, CountyCode, DistrictCode, SchoolCode, AcademicYear)


### Save the resulting dataframe
setwd(data_dir)
dualsave(df_bind, paste0("Exps_managed_wide", "_2011_2016" ))
