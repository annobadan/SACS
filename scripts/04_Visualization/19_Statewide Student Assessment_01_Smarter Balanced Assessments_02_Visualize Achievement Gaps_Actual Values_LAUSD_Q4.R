
###'######################################################################
###'
###' Task    : Statewide Student Assessment. 
###'           01. Smarter BalancedSmarter Balanced Assessments (SBA)
###'           Visualize Gaps in SBAC Scores 
###'           Plot trends by group factors
###'           
###'           by UPP Quartiles only for LAUSD
###'                      
###' Category: Visualization 
###' 
###' Data    : Statewide Student Assessment
###'           Smarter Balanced Assessments (SBA)
###'           splited panel dataframes
###'           2014-15 ~ 2017-18 
###'            
###' Date    : 2019-05-17
###'           2019-06-11 Update with UPP_Q4
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
# data_folder <- c("D:/OneDrive/Data")
# data_folder <- c("C:/Users/joonh/OneDrive/Data")
# 
# data_dir <- file.path(data_folder, 
#                       "LCFF", 
#                       "Statewide_Student_Assessment", 
#                       "CAASPP", 
#                       "Smarter Balanced Assessments")


### Saving path
save_path <- file.path("C:", "Work_Space", "plot_trends_factor")


### Call libraries
library(tidyverse)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Prepare loops over pre-defined combinations
###'
###'

setwd(save_path)

temp_vec <- gsub("./", "", list.dirs())[-1] 

folder_vec <- temp_vec[grepl("_LAUSD", temp_vec)]

folder_vec



###'######################################################################
###'
###' Start loop over folders (i)
###'
###'

for (i in seq_along(folder_vec)){
  
  
  ### Extract folder name
  folder_name <- folder_vec[i]
  
  
  ### Extract names of components from the folder name
  folder_name
  
  vec_temp <- folder_name %>%
    str_split("[:digit:]", simplify = TRUE)
  
  subj_name <- vec_temp[3] %>% str_sub(2, -2) %>%
    str_remove("SBA_")
  
  subgrp_category <- vec_temp[7] %>% str_sub(2, -1) %>%
    str_remove("_LAUSD")
  

  ### Set data containing working directory
  setwd(file.path(save_path, folder_name))
  
  
  
  ###'######################################################################
  ###'
  ###' Load the managed data for visualization.rda
  ###'
  ###'
  
  load(file = "df_merged_Traditional Non-charters.rda")
  df <- df_to_save; rm(df_to_save)
  names(df)
  
  
  
  ###'######################################################################
  ###'
  ###' Extract looping elements
  ###'
  ###'
  
  ### Subgroups
  tabdf(df, Subgrp2)
  subgrp_vec <- unique(df$Subgrp2) %>% str_trim()
  
  
  ### Traditional SOC
  tabdf(df, Traditional_SOC)
  SOC_vec <- setdiff(as.character(unique(df$Traditional_SOC)), "K-12 Schools")
  SOC_vec <- sort(SOC_vec)
  SOC_vec_short <- c("Elementary", "High", "Middle")  
  
  
  
  ###'######################################################################
  ###'
  ###' Start loop over SOC (j)
  ###'
  ###'
  
  for (j in seq_along(SOC_vec)){
    
    ### Subset relevant SOC cases
    SOC_name <- SOC_vec[j]
    SOC_name_short <- SOC_vec_short[j]
    
    df_sub <- df %>%
      filter(Traditional_SOC == SOC_name)
    
    
    ### Need to subset AcademicYears? => No
    tabdf(df_sub, AcademicYear)
    
    
    
    ###'######################################################################
    ###'
    ###' Generate Quartile variables
    ###'
    ###'
    
    ### Extract only UPP variable 
    df_temp <- df_sub %>%
      select(SchoolCode, AcademicYear, PCT_CALPADS_UPC) %>%
      distinct() %>%
      group_by(SchoolCode) %>%
      mutate(mean_UPP = mean(PCT_CALPADS_UPC, na.rm = TRUE)) %>%
      ungroup()
    
    
    ### Generate Quartile variable
    df_temp2 <- df_temp %>%
      group_by(SchoolCode) %>%
      select(mean_UPP) %>%
      distinct() %>%
      ungroup() %>%
      mutate(UPP_Q4_factor = ntile(mean_UPP, 4)) %>%
      mutate(UPP_Q4_factor = factor(UPP_Q4_factor, 
                                    levels = c(1, 2, 3, 4), 
                                    labels = paste0("Q", c(1, 2, 3, 4)))) %>%
      drop_na()
    
    
    ### Generate a variable containing the range of mean_UPP
    df_temp3 <- df_temp2 %>%
      group_by(UPP_Q4_factor) %>%
      arrange(UPP_Q4_factor, mean_UPP) %>%
      mutate(Min = min(mean_UPP, na.rm = TRUE), 
             Max = max(mean_UPP, na.rm = TRUE), 
             UPP_Q4 = paste0(UPP_Q4_factor, " (", 
                             sprintf("%.1f", round(Min, 1)), "%", "-",
                             sprintf("%.1f", round(Max, 1)), "%", ")")) %>%
      ungroup() %>%
      select(-Min, -Max, -mean_UPP, -UPP_Q4_factor)
    
    
    ### Convert UPP_Q4 into a factor
    classmode(df_temp3, everything())
    tabdf(df_temp3, UPP_Q4)
    
    level_vec <- unique(df_temp3$UPP_Q4)
    
    df_temp3 <- df_temp3 %>%
      mutate(UPP_Q4 = factor(UPP_Q4, 
                             levels = level_vec, 
                             labels = level_vec))
    
    levels(df_temp3$UPP_Q4)
    
    
    ### Merge into original dataset
    idxl <- !(names(df) %in% c("UPP_Q4"))
    df <- df[, idxl]
    
    df_merged <- full_join_track(df_sub, df_temp3, 
                                 by = c("SchoolCode")) %>%
      select(SchoolCode:PCT_FRPM, 
             PCT_CALPADS_UPC, mean_UPP, UPP_level, UPP_Q4, 
             everything())
    
    
    # ### Save the resulting dataframe
    # setwd(file.path(save_path, folder_name))
    # dualsave(df_merged, 
    #          paste0("df_merged_", ))
    
    
    
    ###'######################################################################
    ###'
    ###' Prepare a dataset to plot
    ###' 
    ###' - Generate a data frame summarizing mean values by UPP_Q4
    ###'
    ###'
    
    tbl_sum <- df_merged %>%
      group_by(Subgrp2, UPP_Q4, AcademicYear) %>%
      summarise_at(vars(PCT_Exceeded:.merge), mean, na.rm = TRUE) 
    
    
    df_plot <- df_merged %>%
      group_by(Subgrp2, UPP_Q4, AcademicYear) %>%
      summarise(N_schools = n_distinct(SchoolCode)) %>%
      full_join_track(tbl_sum, by = c("Subgrp2", "UPP_Q4", "AcademicYear")) %>%
      select(Subgrp2, UPP_Q4, AcademicYear, N_schools, everything()) %>% 
      select(-".merge") %>%
      arrange(Subgrp2, UPP_Q4, AcademicYear) %>%
      ungroup()
    
    
    
    ###'######################################################################
    ###'
    ###' Start loop over outcome variables
    ###' 
    ###' 
    
    y_names_vec <- df_plot %>% select(contains("PCT_")) %>% names()
    
    for (k in seq_along(y_names_vec)){
      
      ### Select only necessary variables
      y_name <- y_names_vec[k]
      
      df_plot_sub <- df_plot %>%
        select(Subgrp2, UPP_Q4, AcademicYear, y_name)
      
      
      ### Plot 1. Including all 4 quartiles
      colpal_seq <- seq(unique(df_plot_sub$Subgrp2))
      
      p <- plot_trend_grp(df_plot_sub, AcademicYear, 
                          y_name, 
                          Subgrp2, 
                          sprintf = "%.1f") +
        facet_wrap(.~ UPP_Q4, scales = "fixed", nrow = 1) + 
        scale_color_manual(values = rev(color_palette[colpal_seq])) +
        labs(title = paste0("LAUSD ", SOC_name, "s"), 
             subtitle = paste0("SBA: ", subj_name, 
                               ", Subgroup: ", subgrp_category, 
                               ", Outcome: ", y_name), 
             caption = NULL, 
             y = "value",  
             x = "AcademicYear")
      
      
      setwd(file.path(save_path, folder_name))
      plot_name <- paste(sprintf("%02d", j), SOC_name_short, 
                         sprintf("%02d", k), y_name, sep = "_")
      
      ggsave(paste0(plot_name, "_UPP_Q4.pdf"), p, 
             width = 11, height = 5)
      
      save(p, file = paste0(plot_name, "_UPP_Q4.rda"))
      
      
      
      ### Plot 2. Including only Q1 and Q4
      df_plot_sub2 <- df_plot_sub %>%
        filter(grepl("Q1", UPP_Q4) | grepl("Q4", UPP_Q4))
      
      
      colpal_seq <- seq(unique(df_plot_sub$Subgrp2))
      
      p <- plot_trend_grp(df_plot_sub2, AcademicYear, 
                          y_name, 
                          Subgrp2, 
                          sprintf = "%.1f") +
        facet_wrap(.~ UPP_Q4, scales = "fixed", nrow = 1) + 
        scale_color_manual(values = rev(color_palette[colpal_seq])) +
        labs(title = paste0("LAUSD ", SOC_name, "s"), 
             subtitle = paste0("SBA: ", subj_name, 
                               ", Subgroup: ", subgrp_category, 
                               ", Outcome: ", y_name), 
             caption = NULL, 
             y = "value",  
             x = "AcademicYear")
      
      
      ggsave(paste0(plot_name, "_UPP_Q4_Q1N4.pdf"), p, 
             width = 8, height = 5)
      
      save(p, file = paste0(plot_name, "_UPP_Q4_Q1N4.rda"))
      
      
      
      ###'######################################################################
      ###' 
      ###' Print the progress  
      ###' 
      
      cat(paste(sprintf("%02d", i), folder_name,  
                sprintf("%02d", j), SOC_name, 
                sprintf("%02d", k), y_name, sep = "_"), 
          " => completed", "\n")
      
      
    }  ### End of loops over outcome variable list
  } ### End of loops over SOC_vec
}   ### End of loops over folder list


