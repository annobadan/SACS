
###'######################################################################
###'
###' Task    : Statewide Student Assessment. 
###'           01. Smarter BalancedSmarter Balanced Assessments (SBA)
###'           Visualize Gaps in SBAC Scores 
###'           
###'           
###' Category: Visualization 
###' 
###' Data    : Statewide Student Assessment
###'           Smarter Balanced Assessments (SBA)
###'           splited panel dataframes + Gap_df
###'           2014-15 ~ 2017-18 
###'            
###'          
###' Date    : 2019-05-08
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
save_path <- file.path(data_dir, "splitted_panel_df", "Gap_df")


### Call libraries
library(tidyverse)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Load the managed datasets
###'
###'

### Load the California Statewide school info dataset
setwd("D:/Data/LCFF")
load(file = "df_Ultimate_SchoolInfo.rda")
df_schinfo <- df_to_save; rm(df_to_save)


### Filter only data from 2013
df_schinfo <- df_schinfo %>%
  filter(AcademicYear >= 2013)



###'######################################################################
###'
###' Add UPP_level categories to the School info dataset
###'
###'

### Define breaks and labels
breaks <- c(0, 69, 89, 100)

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


# ###'######################################################################
# ###'
# ###' Filter only LAUDS school information
# ###'
# ###'
# 
# idx <- grepl("Los Angeles Unified", df_schinfo$DistrictName)
# LAUSD_Name <- unique(df_schinfo$DistrictName[idx])
# LAUSD_Code <- unique(df_schinfo$DistrictCode[idx])
# 
# df_LAUSDinfo <- df_schinfo %>%
#   filter(DistrictCode == LAUSD_Code) %>%
#   filter(AcademicYear >= 2013)
# 
# head(df_LAUSDinfo)



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
                              df_SBA_Gap, 
                              match_key, 
                              save_path){
  
  ### Merge the two datasets: return .merge variable as well 
  df_merged <- df_schinfo %>%
    full_join_track(df_SBA_Gap, by = match_key, .merge = TRUE)
  
  
  ### Extract the unmerged cases 
  df_schinfo_only <- df_merged %>%
    filter(.merge == "left_only")
  
  df_SBA_Gap_only <- df_merged %>%
    filter(.merge == "right_only")
  
  
  ### Save the datasets
  setwd(save_path)
  save_name <- paste("Gap_df", 
                     unique(df_SBA_Gap$TestID),
                     unique(df_SBA_Gap$Grade), 
                     str_trim(unique(df_SBA_Gap$Subgrp1)), 
                     sep = "_")
  dualsave(df_schinfo_only, paste0(save_name, "_", "df_schinfo_only"))
  dualsave(df_SBA_Gap_only, paste0(save_name, "_", "df_SBA_Gap_only"))
  
  
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
                                  save_path){
  
  ### Generate data for plotting
  df_plot <- df_merged_long %>%
    group_by(Traditional_SOC, UPP_level, Gap_variable, AcademicYear) %>%
    summarise(value = mean(Gap_value, na.rm = TRUE)) %>%
    drop_na() %>% 
    ungroup() 
  
  save_name <- paste("Gap_df", 
                     unique(df_merged_long$TestID),
                     unique(df_merged_long$Grade), 
                     str_trim(unique(df_merged_long$Subgrp1)), 
                     sep = "_")
  
  
  ### Save the df_plot
  setwd(save_path)
  dualsave(df_plot, filename = paste0(save_name, "_", "df_plot"))
  
  
  ### Loop over Traditional_SOC and variable
  SOC_vec <- as.character(unique(df_plot$Traditional_SOC))
  Gap_variable_vec <- levels(df_merged_long$Gap_variable)
  
  
  for (i in seq_along(SOC_vec)){
    for (j in seq_along(Gap_variable_vec)){
      
      ### Filter the relevant cases
      df_temp <- df_plot %>% 
        filter(Traditional_SOC == SOC_vec[i]) %>%
        filter(Gap_variable == Gap_variable_vec[j]) %>%
        mutate(UPP_level = factor(UPP_level, 
                                  levels = levels(df_plot$UPP_level), 
                                  labels = c("Low TSP (Under 69%)", 
                                             "Middle TSP (70% to 89%)", 
                                             "High TSP (90% or more)")))
      
      
      ### Plot!
      p <- plot_trend_grp(df_temp, AcademicYear, value, UPP_level) +
        scale_color_manual(values = rev(color_palette[seq(unique(df_plot$UPP_level))])) + 
        labs(title = paste0(SOC_vec[i], ": ", Gap_variable_vec[j]),  
             subtitle = paste0("Measure: ", save_name),  
             caption = NULL, 
             y = "Achievement Gap",  
             x = "Academic Year")
      
      
      ### Save the resulting plot
      SOC_brief_vec <- c("Elementary", "High", "Middle", "K-12")
      ggsave(file.path(save_path, 
                       paste0(save_name, "_", 
                              sprintf("%02d", i), "_", SOC_brief_vec[i], "_",  
                              sprintf("%02d", j), "_", Gap_variable_vec[j], 
                              ".pdf")), 
             p, width = 6, height = 6)
      
      
      
    } # End of loop over 
  } # End of loop over SOC
}




###'######################################################################
###'
###' (1) SBA_ELA / GR_ALL
###' 
###' Gap = Not Economically Disadvantaged - Economically Disadvantaged
###'
###' 

### Load the dataset
setwd(file.path(data_dir, "splitted_panel_df", "Gap_df"))
load(file = "Gap_df_SBA_ELA_GR_All_Economic Status.rda")
df_SBA <- df_to_save; rm(df_to_save)


### Merge school information dataset
df_merged <- schoolinfo_merger(df_schinfo, 
                               df_SBA, 
                               match_key = match_key, 
                               save_path = save_path) %>%
  arrange(CountyCode, DistrictCode, SchoolCode, AcademicYear)


### Sort only Traditional Non-Charter Schools & Save the dataset
tabdf(df_merged, SOC)
tabdf(df_merged, Traditional_SOC)
tabdf(df_merged, Charter)

df_merged <- df_merged %>% 
  filter(!is.na(Traditional_SOC)) %>%
  filter(Charter == 0)

setwd(save_path)
save_name <- paste("Gap_df", 
                   unique(df_SBA$TestID),
                   unique(df_SBA$Grade), 
                   str_trim(unique(df_SBA$Subgrp1)), 
                   sep = "_")

dualsave(df_merged, 
         paste0(save_name, "_df_merged", "_Traditional Non-charters"))


### Visualize trends by school poverty level and save the results
df_merged_long <- df_merged %>%
  gather(key = Gap_variable, value = Gap_value, contains("Gap_"), 
         factor_key = TRUE)

plotsave_trend_by_UPP(df_merged_long, save_path = save_path) 











