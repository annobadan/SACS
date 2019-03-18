
###'######################################################################
###'
###' Generate Variables
###' 
###' School-level Student Compositions 
###' 
###' (10) Cohort Outcomes:
###' 
###' - Graduation Rate
###' - Dropout Rate
###' - Still Enrolled Rate 
###' - Special Education Rate 
###' - SPED Completer Rate
###' - GED Rate 
###' 
###' 
###' Note: Extract only "Rate"s because NAs are produced excessively
###' to protect student privacy where there are ten or fewer students.
###' Summation of Counts(N_Students) are meaningless
###' 
###' 
###' 20181109 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Public_K-12_Character/09_Cohort_Outcome")


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

years <- c(sprintf("%02d", seq(9, 15)))

CDS_vars <- c("CountyCode", "DistrictCode", "SchoolCode")

meta_list <- list()


for (i in seq_along(years)){
  
  
  ###'######################################################################
  ###'
  ###' Load dataset
  ###'
  ###'
  
  year_num <- years[i]
  
  load(file = paste0("cohort", year_num, "_cleaned", ".rda"))

  classmode(df, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Dataframe #1. School-level aggregates 
  ###'
  ###'

  df_sch <- df %>%
    filter(AggLevel == "School") %>%              # Filter only school-level data
    filter(Subgroup_Program %in% c("Total")) %>%  # Filter only Total of all subgroups
    filter(Subgroup_Race %in% c("Total")) %>%     # Filter only Total of all races
    select(CDS_vars, Subgroup_Program, starts_with("Rate_")) %>%
    rename(Category = Subgroup_Program) %>%
    select(CDS_vars, Category, everything())
    

  
  ###'######################################################################
  ###'
  ###' Dataframe #2. School-level aggregates: by Gender
  ###' 
  ###' => Calculated already. Only extraction needed.
  ###'
  ###'

  df_gender <- df %>%
    filter(AggLevel == "School") %>%                      # Filter only school-level data
    filter(Subgroup_Gender %in% c("male", "female")) %>%  # Filter male and female
    filter(Subgroup_Race %in% c("Total")) %>%     # Filter Total of all races/ethnicities
    select(CDS_vars, Subgroup_Gender, starts_with("Rate_")) %>%
    rename(Category = Subgroup_Gender) %>%
    select(CDS_vars, Category, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Dataframe #3. School-level aggregates: by Race/Ethnicity
  ###' 
  ###' => Calculated already. Only extraction needed.
  ###'
  ###'
  
  df_race <- df %>%
    filter(AggLevel == "School") %>%              # Filter only school-level data
    filter(Subgroup_Gender %in% c("Total")) %>%   # Filter Total of all subgroups
    filter(!Subgroup_Race %in% c("Total")) %>%    # Filter out Total of all races
    select(CDS_vars, Subgroup_Race, starts_with("Rate_")) %>%
    rename(Category = Subgroup_Race) %>%
    select(CDS_vars, Category, everything())
    
  
  
  ###'######################################################################
  ###'
  ###' Dataframe #4. School-level aggregates: by Program Participation
  ###' 
  ###' => Calculated already. Only extraction needed.
  ###'
  ###'
  
  df_program <- df %>%
    filter(AggLevel == "School") %>%               # Filter only school-level data
    filter(Subgroup_Race %in% c("Total")) %>%      # Filter Total of all races/ethnicities
    filter(!Subgroup_Program %in% c("Total")) %>%  # Filter out Total of all subgroups
    filter(!is.na(Subgroup_Program)) %>%           # Filter out NAs
    select(CDS_vars, Subgroup_Program, starts_with("Rate_")) %>%
    rename(Category = Subgroup_Program) %>%
    select(CDS_vars, Category, everything())
    
  
  
  ###'######################################################################
  ###'
  ###' Combine dataframe
  ###'
  ###'
  
  ### Bind rows
  temp_list <- list(df_sch, df_gender, df_race, df_program)
  df_bind <- reduce(temp_list, bind_rows)
  classmode(df_bind, everything())
  
  
  ### Arrange the binded dataframe
  category_levels <- c("Total", "male", "female", 
                       "White", "Hispanic or Latino", "African American", 
                       "Asian", "Filipino", "American Indian or Alaska Native", 
                       "Pacific Islander", "Two or More Races", "Not reported",  
                       "English Learners", "Migrant Education", 
                       "Socioeconomically Disadvantaged", "Special Education", 
                       "Homeless", "Foster")
  
  df_bind <- df_bind %>%
    mutate(Category = factor(Category, levels = category_levels)) %>% 
    arrange(CountyCode, DistrictCode, SchoolCode, Category)
  
  
  ### Add AcademicYear (Graduation Year: Class of ~)
  df_bind <- df_bind %>%
    mutate(ClassOf = 2000 + as.numeric(year_num)) %>%
    select(CountyCode, DistrictCode, SchoolCode, ClassOf, everything())
    
  
  
  ###'######################################################################
  ###'
  ###' Embed into the list
  ###'
  ###'
  
  meta_list[[i]] <- df_bind
  
  
  
  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("Year ", year_num, " completed", "\n"))
  
}



###'######################################################################
###'
###' Independent prearation for the dataframe for the class of 2016
###'
###'

### Load the cleaned dataframe
setwd(data_dir)
year_num <- 16
load(file = paste0("acgr", year_num, "_cleaned", ".rda"))
classmode(df, everything())


### Generate function to convert to pre2016 dataframe
convert_to_pre2016 <- function(df){
  
  # (1) Select only necessary variables
  df_temp <- df %>%
    select(ends_with("Code"), Category, 
           Rate_RegularHSGrad, 
           Rate_Dropout, 
           Rate_SPEDCertificate, 
           Rate_StillEnrolled, 
           Rate_GEDCompleter)
  
  #' (2) Rename variables
  #' 
  #' Rate_Cohort_Graduates 
  #' Rate_Cohort_Dropouts 
  #' Rate_SPED_Complete 
  #' Rate_Still_Enrolled 
  #' Rate_GED
  
  df_temp <- df_temp %>%
    rename(Rate_Cohort_Graduates = Rate_RegularHSGrad, 
           Rate_Cohort_Dropouts = Rate_Dropout, 
           Rate_SPED_Complete = Rate_SPEDCertificate, 
           Rate_Still_Enrolled = Rate_StillEnrolled, 
           Rate_GED = Rate_GEDCompleter)
  
  
  ### (3) Delete rows with all NAs
  mat_temp <- df_temp %>%
    select(starts_with("Rate_"))
  
  var_counts <- length(names(mat_temp))
  NA_counts <- rowSums(is.na(mat_temp))
  idx <- !(NA_counts == var_counts)
  
  df_temp <- df_temp[idx, ]
  
  
  ### Return the resulting dataframe
  return(df_temp)
}


### Dataframe #1. School-level Totals
df_sch <- df %>%
  filter(AggLevel %in% c("School")) %>%
  filter(CharterYN %in% c("All") & DASSYN %in% c("All")) %>%
  filter(Category %in% c("TA")) %>%
  select(CountyCode, DistrictCode, SchoolCode, Category, 
         starts_with("Rate_")) %>%
  mutate(Category = "Total") %>% 
  convert_to_pre2016()


### Dataframe #2. School-level Totals: By Gender
df_gender <- df %>%
  filter(AggLevel %in% c("School")) %>% 
  filter(CharterYN %in% c("All") & DASSYN %in% c("All")) %>% 
  filter(Category %in% c("GM", "GF")) %>%
  mutate(Category = if_else(Category == "GM", "male", "female", 
                            missing = NA_character_)) %>%
  convert_to_pre2016()


### Dataframe #3. School-level Totals: By Race/Ethnicity
df_race <- df %>%
  filter(AggLevel %in% c("School")) %>% 
  filter(CharterYN %in% c("All") & DASSYN %in% c("All")) %>% 
  filter(!is.na(Category_Race)) %>%
  filter(Category_Race != "Total") %>% 
  select(-Category) %>%
  rename(Category = Category_Race) %>%
  convert_to_pre2016()


### Dataframe #4. School-level Totals: By Program participation   
df_program <- df %>%
  filter(AggLevel %in% c("School")) %>% 
  filter(CharterYN %in% c("All") & DASSYN %in% c("All")) %>% 
  filter(!is.na(Category_Program)) %>%
  filter(Category_Program != "Total") %>% 
  select(-Category) %>%
  rename(Category = Category_Program) %>%
  convert_to_pre2016()


### Bind rows
temp_list <- list(df_sch, df_gender, df_race, df_program)
df_bind_16 <- reduce(temp_list, bind_rows)
classmode(df_bind_16, everything())


### Arrange the binded dataframe
category_levels <- c("Total", "male", "female", 
                     "White", "Hispanic or Latino", "African American", 
                     "Asian", "Filipino", "American Indian or Alaska Native", 
                     "Pacific Islander", "Two or More Races", "Not reported",  
                     "English Learners", "Migrant Education", 
                     "Socioeconomically Disadvantaged", "Special Education", 
                     "Homeless", "Foster")

df_bind_16 <- df_bind_16 %>%
  mutate(Category = recode(Category, 
                           "Migrant" = "Migrant Education", 
                           "Students with Disabilities" = "Special Education")) %>%
  mutate(Category = factor(Category, levels = category_levels)) %>% 
  arrange(CountyCode, DistrictCode, SchoolCode, Category)


### Add AcademicYear (Graduation Year: Class of ~)
df_bind_16 <- df_bind_16 %>%
  mutate(ClassOf = 2000 + as.numeric(year_num)) %>%
  select(CountyCode, DistrictCode, SchoolCode, ClassOf, everything())




###'######################################################################
###'
###' Bind and save 2009-2015 dataframe and 2016 dataframe
###'
###'

### Bind rows!
df_bind_0915 <- bind_rows(meta_list)
df_bind <- bind_rows(df_bind_0915, df_bind_16)


### Arrange rows
df_bind <- df_bind %>%
  arrange(CountyCode, DistrictCode, SchoolCode, Category, ClassOf)


### Save the resulting long data
dualsave(df_bind, "Cohort_Outcomes_managed_2009_2016")

  
