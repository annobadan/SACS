
###'######################################################################
###'
###' Task    : Statewide Student Assessment. 
###'           01. Smarter BalancedSmarter Balanced Assessments (SBA)
###'           Visualize Gaps in SBAC Scores 
###'           Plot trends by group factors
###'                      
###' Category: Visualization 
###' 
###' Data    : Statewide Student Assessment
###'           Smarter Balanced Assessments (SBA)
###'           splited panel dataframes + Gap_df
###'           2014-15 ~ 2017-18 
###'            
###' Date    : 2019-05-17
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
data_dir <- c("D:/Data/LCFF/Statewide_Student_Assessment/CAASPP/Smarter Balanced Assessments")


### Saving path
save_path <- file.path(data_dir, "splitted_panel_df", "plot_trends_factor")


### Call libraries
library(tidyverse)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Load the managed statewide school information datasets
###'
###'

### Load the California Statewide school info dataset
setwd("D:/Data/LCFF")
load(file = "df_Ultimate_SchoolInfo.rda")
df_schinfo <- df_to_save; rm(df_to_save)


###' Filter only data from 2014
###' Because the SBAC data is available from Academic Year 2014 
df_schinfo <- df_schinfo %>%
  filter(AcademicYear >= 2014)

tabdf(df_schinfo, AcademicYear)



###'######################################################################
###'
###' Add UPP_level categories to the School info dataset
###'
###' "Absolute" levels, not "relative" levels
###'
###'

### Define breaks and labels
breaks <- c(0, 59, 89, 100)

labels <- c("Low TSP", 
            "Middle TSP", 
            "High TSP")


### Generate the quartiles of PCT_CALPADS_UPC
df_UPP <- df_schinfo %>%
  select(CountyCode, DistrictCode, SchoolCode, 
         Traditional_SOC, PCT_CALPADS_UPC) %>%
  distinct() %>%
  group_by(CountyCode, DistrictCode, SchoolCode) %>%
  summarise(Traditional_SOC = first(Traditional_SOC), 
            mean_UPP = mean(PCT_CALPADS_UPC, na.rm = TRUE)) %>%
  mutate(UPP_level = cut(mean_UPP, breaks = breaks, labels = labels))


### Check the distribution
tbl_temp <- df_UPP %>%
  group_by(Traditional_SOC, UPP_level) %>%
  drop_na() %>%
  count()

tbl_temp


### Merge with the original df_LAUSDinfo
df_schinfo <- df_schinfo %>%
  full_join_track(df_UPP, by = c("CountyCode", "DistrictCode", "SchoolCode", 
                                 "Traditional_SOC"))



###'######################################################################
###'
###' schoolinfo_merger():
###'
###' => A function to merge the school information 
###'    and investigate the merged datasets
###'    
###'    

#### Default settings
match_key <- c("CountyCode", "DistrictCode", "SchoolCode", "AcademicYear")


schoolinfo_merger <- function(df_schinfo, 
                              df_SBA, 
                              match_key, 
                              folder_dir){
  
  ### Merge the two datasets: return .merge variable as well 
  df_merged <- df_schinfo %>%
    full_join_track(df_SBA, by = match_key, .merge = TRUE)
  
  
  ### Extract the unmerged cases 
  df_schinfo_only <- df_merged %>%
    filter(.merge == "left_only")
  
  df_SBA_only <- df_merged %>%
    filter(.merge == "right_only")
  
  
  ### Save the datasets
  setwd(folder_dir)
  dualsave(df_schinfo_only, paste0("df_schinfo_only"))
  dualsave(df_SBA_only, paste0("df_SBA_only"))
  
  
  ### Return only matched cases
  df_merged %>%
    filter(.merge == "matched")
  
}



###'######################################################################
###'
###' plotsave_trend_by_UPP():
###' 
###' A function to visualize trends summarized by school poverty level
###' 
###' 

plotsave_trend_by_UPP <- function(df_merged_long, 
                                  save_path, 
                                  filename = filename,
                                  factor_labels){
  
  ### Generate data for plotting
  df_plot <- df_merged_long %>%
    group_by(Traditional_SOC, UPP_level, variable, Subgrp2, AcademicYear) %>%
    summarise(value = mean(value, na.rm = TRUE)) %>%
    drop_na() %>% 
    ungroup() 
  
  save_name <- filename
  
  
  ### Save the df_plot
  setwd(save_path)
  dualsave(df_plot, filename = paste0("df_plot"))
  
  
  ### Loop over Traditional_SOC and variable
  SOC_vec <- as.character(unique(df_plot$Traditional_SOC))
  variable_vec <- levels(df_merged_long$variable)
  
  
  for (i in seq_along(SOC_vec)){
    for (j in seq_along(variable_vec)){
      
      ### Filter the relevant cases
      df_temp <- df_plot %>% 
        filter(Traditional_SOC == SOC_vec[i]) %>%
        filter(variable == variable_vec[j]) %>%
        mutate(UPP_level = factor(UPP_level, 
                                  levels = levels(df_plot$UPP_level), 
                                  labels = factor_labels))
      
      
      ### Plot!
      p <- plot_trend_grp(df_temp, AcademicYear, value, Subgrp2, 
                          sprintf = "%.1f") + 
        facet_wrap(.~ UPP_level) + 
        # scale_color_manual(values = color_palette[seq(unique(df_plot$Subgrp2))]) + 
        labs(title = paste0(SOC_vec[i], ": ", variable_vec[j]),  
             subtitle = save_name,  
             caption = NULL, 
             y = "Percentage (%)",  
             x = "Academic Year")
      
      
      ### Save the resulting plot
      SOC_brief_vec <- c("Elementary", "High", "Middle", "K-12")
      ggsave(file.path(save_path, 
                       paste0(sprintf("%02d", i), "_", SOC_brief_vec[i], "_",  
                              sprintf("%02d", j), "_", variable_vec[j], 
                              ".pdf")), 
             p, width = 9, height = 6)
      
      
      
    } # End of loop over 
  } # End of loop over SOC
}

### Default factor labels
factor_labels <- c("Low TSP (Under 59%)", 
                   "Middle TSP (60% to 89%)", 
                   "High TSP (90% or more)")



###'######################################################################
###'
###' Execute looping over each dataframe
###' 
###' (1) Statewide
###'
###'

###' Define a pre-defined list containing
###' (1) Filename
###' (2) subgroups to plot (with order)

list_01 <- list("02_Gender", 
                c("Males", "Females"))

list_02 <- list("03_English-Language Fluency", 
                c("English Learner", 
                  "Fluent-English Proficient and English Only"))

list_03 <- list("05_Economic Status", 
                c("Economically Disadvantaged", 
                  "Not Economically Disadvantaged"))

list_04 <- list("06_Ethnicity", 
                c("White", 
                  "Hispanic or Latino",
                  "Black or African American", 
                  "Asian"))

list_05 <- list("08_Disability Status", 
                c("Students with Disability", 
                  "Students with No Reported Disability"))

list_06 <- list("09_Ethnicity for Economically Disadvantaged", 
                c("White", 
                  "Hispanic or Latino",
                  "Black or African American", 
                  "Asian"))

list_07 <- list("10_Ethnicity for Not Economically Disadvantaged", 
                c("White", 
                  "Hispanic or Latino",
                  "Black or African American", 
                  "Asian"))


### Save the list of these lists
list_filename_subgrp <- list()

for (k in seq(1, 7)){
  
  list_filename_subgrp[[k]] <- get(paste0("list_", sprintf("%02d", k)))
  
}

setwd(file.path(data_dir, "splitted_panel_df", "plot_trends_factor"))
save(list_filename_subgrp, file = "list_filename_subgrp.rda")


### Call list_gap_definitions
setwd(file.path(data_dir, "splitted_panel_df", "plot_trends_factor"))
load(file = "list_filename_subgrp.rda")


### Define subject vector
subject_vec <- c("SBA_ELA", "SBA_Math")


for (i in seq(1, length(list_filename_subgrp))){  # Loop over pre-defined lists
  
  for (j in seq_along(subject_vec)){  # Loop over subjects
    
    ### Call the list
    list_temp <- list_filename_subgrp[[i]] 
      
    
    ### Load the generated gap dataframe
    splitted <- list_temp[[1]] %>% str_split_fixed("_", n = 2)
    subgrp_name <- splitted[1, 2] 
    
    filename <- paste0(sprintf("%02d", j), "_", subject_vec[j], 
                       "_08_GR_All_", 
                       list_temp[[1]])
    
    setwd(file.path(data_dir, "splitted_panel_df"))
    load(file = paste0(filename, ".rda"))
    df_SBA <- df_to_save; rm(df_to_save)
    
    
    ### Create a new folder
    folder_dir <- file.path(save_path, filename)
    
    dir.create(folder_dir, showWarnings = FALSE)
    
    setwd(folder_dir)
    
    
    ### Merge school information dataset
    match_key <- c("CountyCode", "DistrictCode", "SchoolCode", "AcademicYear")
    
    df_merged <- schoolinfo_merger(df_schinfo, 
                                   df_SBA, 
                                   match_key = match_key, 
                                   folder_dir = folder_dir) %>%
      arrange(CountyCode, DistrictCode, SchoolCode, AcademicYear)
    
    
    ### Sort only Traditional Non-Charter Schools & Save the dataset
    tabdf(df_merged, SOC)
    tabdf(df_merged, Traditional_SOC)
    tabdf(df_merged, Charter)
    
    df_merged <- df_merged %>% 
      filter(!is.na(Traditional_SOC)) %>%
      filter(Charter == 0)
    
    setwd(folder_dir)
    dualsave(df_merged, 
             paste0("df_merged", "_Traditional Non-charters"))
    
    
    ### How many schools are in the merged dataset?
    tbl_N_Schools <- df_merged %>%
      group_by(Traditional_SOC, Subgrp2, UPP_level, AcademicYear) %>% 
      summarise(N_Schools = n_distinct(CountyCode, DistrictCode, SchoolCode))
    
    tbl_N_Schools
    
    
    ### Filter only relevant subgroups
    idx <- str_trim(df_merged$Subgrp2) %in% list_temp[[2]]
    df_merged <- df_merged[idx, ]
    
    
    ### Visualize trends by school poverty level and save the results
    yvar_list <- df_SBA %>% select(contains("PCT_")) %>% names()
    
    df_merged_long <- df_merged %>%
      gather(key = variable, value = value, 
             yvar_list, 
             factor_key = TRUE)
    
    
    factor_labels <- c("Low TSP (Under 59%)", 
                       "Middle TSP (60% to 89%)", 
                       "High TSP (90% or more)")
    
    
    plotsave_trend_by_UPP(df_merged_long, 
                          save_path = folder_dir, 
                          filename = filename, 
                          factor_labels = factor_labels) 
    
    
    ### Print the progress
    cat(paste0(filename, "\n"))
    

  }  # End of loop over subjects
}    # End of loop over pre-defined lists




###'######################################################################
###'
###' Execute looping over each dataframe
###' 
###' (2) LAUSD Only
###'
###'

### Call list_gap_definitions
setwd(file.path(data_dir, "splitted_panel_df", "plot_trends_factor"))
load(file = "list_filename_subgrp.rda")


### Define subject vector
subject_vec <- c("SBA_ELA", "SBA_Math")


for (i in seq(1, length(list_filename_subgrp))){  # Loop over pre-defined lists
  
  for (j in seq_along(subject_vec)){  # Loop over subjects
    
    ### Call the list
    list_temp <- list_filename_subgrp[[i]] 
    
    
    ### Load the generated gap dataframe
    splitted <- list_temp[[1]] %>% str_split_fixed("_", n = 2)
    subgrp_name <- splitted[1, 2] 
    
    filename <- paste0(sprintf("%02d", j), "_", subject_vec[j], 
                       "_08_GR_All_", 
                       list_temp[[1]])
    
    setwd(file.path(data_dir, "splitted_panel_df"))
    load(file = paste0(filename, ".rda"))
    df_SBA <- df_to_save; rm(df_to_save)
    
    
    ### Create a new folder
    folder_dir <- file.path(save_path, paste0(filename, "_LAUSD"))
    
    dir.create(folder_dir, showWarnings = FALSE)
    
    setwd(folder_dir)
    
    
    ### Merge school information dataset
    match_key <- c("CountyCode", "DistrictCode", "SchoolCode", "AcademicYear")
    
    df_merged <- schoolinfo_merger(df_schinfo, 
                                   df_SBA, 
                                   match_key = match_key, 
                                   folder_dir = folder_dir) %>%
      arrange(CountyCode, DistrictCode, SchoolCode, AcademicYear)
    
    
    ### Subset only LAUSD
    df_merged <- df_merged %>%
      filter(DistrictCode == 64733)
    
    
    ### Sort only Traditional Non-Charter Schools & Save the dataset
    tabdf(df_merged, SOC)
    tabdf(df_merged, Traditional_SOC)
    tabdf(df_merged, Charter)
    
    df_merged <- df_merged %>% 
      filter(!is.na(Traditional_SOC)) %>%
      filter(Charter == 0)
    
    setwd(folder_dir)
    dualsave(df_merged, 
             paste0("df_merged", "_Traditional Non-charters"))
    
    
    ### How many schools are in the merged dataset?
    tbl_N_Schools <- df_merged %>%
      group_by(Traditional_SOC, Subgrp2, UPP_level, AcademicYear) %>% 
      summarise(N_Schools = n_distinct(CountyCode, DistrictCode, SchoolCode))
    
    tbl_N_Schools
    
    
    ### Filter only relevant subgroups
    idx <- str_trim(df_merged$Subgrp2) %in% list_temp[[2]]
    df_merged <- df_merged[idx, ]
    
    
    ### Visualize trends by school poverty level and save the results
    yvar_list <- df_SBA %>% select(contains("PCT_")) %>% names()
    
    df_merged_long <- df_merged %>%
      gather(key = variable, value = value, 
             yvar_list, 
             factor_key = TRUE)
    
    
    factor_labels <- c("Low TSP (Under 59%)", 
                       "Middle TSP (60% to 89%)", 
                       "High TSP (90% or more)")
    
    
    plotsave_trend_by_UPP(df_merged_long, 
                          save_path = folder_dir, 
                          filename = filename, 
                          factor_labels = factor_labels) 
    
    
    ### Print the progress
    cat(paste0(filename, "\n"))
    
    
  }  # End of loop over subjects
}    # End of loop over pre-defined lists

