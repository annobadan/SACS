
###'######################################################################
###'
###' Data Visualization
###' 
###' Change of School-level Teacher Compositions
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


### Set a directory containing large data files
data_dir <- c("D:/Data/LCFF/Staff_Data/Certificated_Staff/Staff_Demographic")


### Call libraries
library(tidyverse)
library(foreign)
library(rlang)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import the prepared dataset
###'
###'

### Import the prepared teacher composition datasets
setwd(paste0(data_dir, "/", "school-level teacher composition"))

load(file = "df_StaffType_0317.rda")
df_StaffType <- df_bind; rm(df_bind)

load(file = "df_Teacher_Gender_0317.rda")
df_Gender <- df_bind; rm(df_bind)

load(file = "df_Teacher_Race_0317.rda")
df_Race <- df_bind; rm(df_bind)

load(file = "df_Teacher_Education_0317.rda")
df_Education <- df_bind; rm(df_bind)

load(file = "df_Teacher_Years_0317.rda")
df_Years <- df_bind; rm(df_bind)

load(file = "df_Teacher_New_0317.rda")
df_New <- df_bind; rm(df_bind)

load(file = "df_Teacher_EmployStatus_0317.rda")
df_Employ <- df_bind; rm(df_bind)

load(file = "df_JobClassification_0317.rda")
df_JobClass <- df_bind; rm(df_bind)

load(file = "df_FTE_0317.rda")
df_FTE <- df_bind; rm(df_bind)


### Import the cleaned student composition dataset
setwd(work_dir)
load(file = "processed_data/df_student_composition_0317_Traditional.rda")



###'######################################################################
###'
###' Merge school-level teacher composition datasets
###'
###'

merge_key <- c("CountyCode", "DistrictCode", "SchoolCode", "AcademicYear")
sumstats <- c("N", "mean", "median", "sd")

### Merge df_StaffType + df_Gender
df_Teacher <- df_StaffType %>% 
  rename(N_Total_Staffs = subtotal) %>%
  select(-table) %>%
  left_join(df_Gender, by = merge_key) %>%
  rename(N_Total_Teachers = subtotal) %>%
  select(-table)


### Merge df_Race
df_Teacher <- df_Teacher %>%
  left_join(df_Race, by = merge_key) %>%
  select(-table, -subtotal)


### Merge df_Education
df_Teacher <- df_Teacher %>%
  left_join(df_Education, by = merge_key) %>%
  select(-table, -subtotal)


### Merge df_Years
df_Years1 <- df_Years %>%
  filter(table == "Years Teaching") %>%
  select(merge_key, sumstats) 
names(df_Years1) <- c(merge_key, paste0(sumstats, "_", "yrs_teach"))

df_Years2 <- df_Years %>%
  filter(table == "Years in District") %>%
  select(merge_key, sumstats) 
names(df_Years2) <- c(merge_key, paste0(sumstats, "_", "yrs_in_dist"))

df_Teacher <- df_Teacher %>% 
  left_join(df_Years1, by = merge_key) %>%
  left_join(df_Years2, by = merge_key)
  

### Merge df_New
new_vars <- c("N_New", "PCT_New")
  
df_New1 <- df_New %>%
  filter(table == "New Teaching") %>%
  select(merge_key, new_vars) 
names(df_New1) <- c(merge_key, paste0(new_vars, "_", "teach"))

df_New2 <- df_New %>%
  filter(table == "New in District") %>%
  select(merge_key, new_vars) 
names(df_New2) <- c(merge_key, paste0(new_vars, "_", "in_dist"))

df_Teacher <- df_Teacher %>% 
  left_join(df_New1, by = merge_key) %>%
  left_join(df_New2, by = merge_key)


### Merge df_EmployStatus
df_Teacher <- df_Teacher %>%
  left_join(df_Employ, by = merge_key) %>%
  select(-table, -subtotal)


### Merge df_FTE
df_FTE1 <- df_FTE %>%
  filter(table == "FTE_All Staffs") %>%
  select(merge_key, sumstats) 
names(df_FTE1) <- c(merge_key, paste0(sumstats, "_", "FTE_all"))

df_FTE2 <- df_FTE %>%
  filter(table == "FTE_Teachers") %>%
  select(merge_key, sumstats) 
names(df_FTE2) <- c(merge_key, paste0(sumstats, "_", "FTE_teacher"))

df_Teacher <- df_Teacher %>% 
  left_join(df_FTE1, by = merge_key) %>%
  left_join(df_FTE2, by = merge_key)



###'######################################################################
###'
###' Merge to school-level student composition dataset
###'
###'

df_Teacher_Student <- df %>%
  select(-(N_male:PCT_Two_more)) %>%
  left_join(df_Teacher, by = c("CountyCode", "DistrictCode", "SchoolCode", 
                               "AcademicYear"))


df_Teacher_Student <- df_Teacher_Student %>%
  mutate(ST_Ratio = Total_Enroll/N_Total_Teachers, 
         SS_Ratio = Total_Enroll/N_Total_Staffs)

setwd(work_dir)
save(df_Teacher_Student, file = "processed_data/df_Teacher_Student_compositions_0317.rda")
write.dta(df_Teacher_Student, file = "processed_data/df_Teacher_Student_compositions_0317.dta")


###'######################################################################
###'
###' Prepare a for loop
###'
###'

### Variable names to investigate
vars_temp <- names(df_Teacher)[!grepl("N_", names(df_Teacher))]
vars <- vars_temp[-(1:4)]
var_names <- c("N_Total_Staffs", "N_Total_Teachers", 
               "ST_Ratio", "SS_Ratio", vars)


### Elements for ggplot
title_vec <- c("Total Number of Staffs",
               "Total Number of Teachers", 
               "Student-teacher Ratio", 
               "Student-staff Ratio", 
               "% Teachers", "% Administrators", "% Pupil Services", 
               "% Female Teachers", "% Male Teachers", 
               "% White Teachers", "% Hispanic/Latino Teachers", 
               "% African American Teachers", 
               "% Asian Teachers", 
               "% Filipino Teachers", 
               "% Pacific Islander Teachers", 
               "% American Indian or Alaska Native Teachers", 
               "% Two or More Races Teachers", 
               "% Master's Degree or above", 
               "% Bachelor's Degree or below", 
               "Years Teaching - Mean", 
               "Years Teaching - Median",
               "Years Teaching - Standard Deviation", 
               "Years in District - Mean", 
               "Years in District - Median", 
               "Years in District - Standard Deviation", 
               "% New in Teaching (Novice Teachers)", 
               "% New in District", 
               "% Long term substitute or temporary employee", 
               "% Probationary Teachers", 
               "% Tenured Teachers", 
               "% Other Teachers", 
               "Full-time Equivalent (FTE) duties of All Staffs - Mean",
               "Full-time Equivalent (FTE) duties of All Staffs - Median", 
               "Full-time Equivalent (FTE) duties of All Staffs - Standard Deviation", 
               "Full-time Equivalent (FTE) duties of Teachers - Mean", 
               "Full-time Equivalent (FTE) duties of Teachers - Median", 
               "Full-time Equivalent (FTE) duties of Teachers - Standard Deviation")


subtitle <- "By School Type and Poverty Level (% FRPM)"

xlab <- "Academic Year"

ylab <- "Average Weighted by Total School Enrollment"



###'######################################################################
###'
###' Implement the for loop
###'
###'

for (i in seq_along(var_names)){
  
  ### Assign variable name
  df_temp <- df_Teacher_Student %>%
    mutate(AcademicYear = AcademicYear + 2000)
  
  idx <- which(names(df_temp) == var_names[i])
  names(df_temp)[idx] <- "variable"
  
  
  ###' Generate data to plot
  ###' SOC by Poverty Level
  
  df_plot <- df_temp %>%
    group_by(AcademicYear, SOC, PCT_FRPM_15bin) %>%
    summarise(mean_value = round(mean(variable, na.rm = TRUE), 2)) 

  df_plot <- df_plot %>%
    filter(SOC != "K-12 Schools")
  
  
  ### Plot!
  p1 <- plot_trend_grp_facet(df_plot, 
                             x = AcademicYear, 
                             y = mean_value, 
                             group = PCT_FRPM_15bin, 
                             facet_formula = . ~ SOC, 
                             yline = 2013, 
                             ylim = auto_ylim(df_plot$mean_value)) + 
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  
  
  labels <- labs(title = paste0(title_vec[i], " 2003-2017"), 
                 subtitle = subtitle, 
                 caption = NULL, 
                 x = xlab, y = ylab)
  
  p2 <- p1 + labels 
  
  
  ### Save as pdf file
  ggsave(paste0("figures/", "Teacher_Composition_",
                sprintf("%02d", i), "_", var_names[i],".pdf"), p2, 
         width = 17, height = 9)
  
}

