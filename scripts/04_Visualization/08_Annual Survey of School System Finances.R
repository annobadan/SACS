
###'######################################################################
###'
###' Data Visualization
###' 
###' Comparison of California with all other US states
###' 
###' => F33 Survey data: Annual Survey of School System Finances
###' 
###' 
###' 20180826 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Financial/Annual Survey of School System Finances")


### Call libraries
library(tidyverse)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import the pre-cleaned data frame
###'
###'

### Individual unit tables
load(file = "processed_data/Individual_Unit_Tables_df.rda")


### Variable name and description
vars <- read.csv(file = "tables/F33_Variable_Description.csv", as.is = TRUE)



###'######################################################################
###'
###' Filter only Elementary/Secondary school system
###' 
###' 1. Elementary School System Only
###' 2. Secondary School Sytstem Only
###' 3. Elementary-Secondary School System
###' 
###' 

indv_units_df %>%
  group_by(SCHLEV, School_Level) %>%
  count()


df <- indv_units_df %>%
  filter(SCHLEV %in% c(1, 2, 3))


df %>%
  group_by(SCHLEV, School_Level) %>%
  count()



###'######################################################################
###'
###' Convert to the real 2016 dollars using the CPI-U deflator
###'
###'

### Select variables to convert

vars_dollars <- vars$Data.Item[8:46]

vars_perpupil <- vars$Data.Item[56:66] 

vars_to_convert <- c(vars_dollars, vars_perpupil)

idx <- which(names(df) %in% vars_to_convert)



### Loop over the selected variable list

temp_df <- df

for (i in seq_along(idx)){
  
  original_varname <- names(temp_df)[idx[i]] 
  
  names(temp_df)[idx[i]] <- "selected_var"
  
  temp_df <- CPI_converter(temp_df, 2016, Fiscalyear, selected_var)
  
  names(temp_df)[idx[i]] <- original_varname
  
  idx_convrtd <- which(names(temp_df) == "selected_var_16")
  
  names(temp_df)[idx_convrtd] <- paste0(original_varname, "_16")
  
  temp_df[, idx_convrtd] <- round(temp_df[, idx_convrtd], 0)

}

df <- temp_df



###'######################################################################
###'
###' Plot trends
###' 
###' - Factor by School Level
###' - Loop #1. STATE
###' - Loop #2. Variables
###' 
###' 

### Prepare dataset

idx_PP <- grep("PP", vars$Data.Item)
vars_perpupil <- paste0(vars$Data.Item[idx_PP], "_16") 
vars_perpupil_label <- vars$Description[idx_PP]

idx_PCT <- grep("PCT", vars$Data.Item)
vars_percent <- vars$Data.Item[idx_PCT]
vars_percent_label <- vars$Description[idx_PCT]

vars_vec <- c(vars_perpupil, vars_percent)
vars_label_vec <- c(vars_perpupil_label, vars_percent_label)

state_vec <- unique(df$STATE_NAME)


indv_units_df_wtd_means <- data.frame()


for (i in seq_along(state_vec)){   # Loop over 51 states
  
  for (j in seq_along(vars_vec)) { # Loop over 20 variables
    
    ### Generate dataset
    
    idx <- which(names(df) == vars_vec[j])
    
    original_name <- names(df)[idx]
    
    names(df)[idx] <- "selected_var"
    
    df_plot <- df %>%
      filter(STATE_NAME == state_vec[i]) %>% 
      get_weighted_mean(x = Fiscalyear, 
                        y = selected_var, 
                        weight = ENROLL, 
                        STATE_NAME, 
                        School_Level) 
    
    names(df)[idx] <- original_name
    
    df_plot$variable <- vars_vec[j]
    
    df_plot <- select(df_plot, Fiscalyear, STATE_NAME, School_Level, 
                      variable, mean_value)
    
    
    ### Append to the previous data frame
    indv_units_df_wtd_means <- bind_rows(indv_units_df_wtd_means, df_plot)
  }
}


### Save the resulting dataset => Visualize with Shiny Web Application
save(indv_units_df_wtd_means, file = "processed_data/indv_units_df_wtd_means.rda")




# ### Get weighted mean: weighted by district ADA
# 
# for (i in seq_along(state_vec)){   # Loop over 51 states
#   
#   for (j in seq_along(vars_vec)) { # Loop over 20 variables
#     
#     ### Generate dataset
#     
#     idx <- which(names(df) == vars_vec[j])
#     
#     original_name <- names(df)[idx]
#     
#     names(df)[idx] <- "selected_var"
#     
#     df_plot <- df %>%
#       filter(STATE_NAME == state_vec[i]) %>% 
#       get_weighted_mean(x = Fiscalyear, 
#                         y = selected_var, 
#                         weight = ENROLL, 
#                         STATE_NAME, 
#                         School_Level) 
#     
#     names(df)[idx] <- original_name
#     
#     
#     ### Plot: factored by School level
#     p <- plot_trend_grp(df_plot, Fiscalyear, mean_value, School_Level, yline = 2013, 
#                         ylim = auto_ylim(df_plot$mean_value)) 
#     
#     labels <- labs(title = state_vec[i], 
#                    subtitle = paste0(vars_label_vec[j], ", in real 2016 dollars"), 
#                    caption = "Source: Annual Survey of School System Finances", 
#                    y = vars_label_vec[j],   
#                    x = "Fiscal Years") 
#     
#     p <- p + labels
#     
#     
#     ### Save as pdf file
#     filename <- paste0(vars_label_vec[j], "_", state_vec[i])
#     ggsave(paste0("figures/", filename, ".pdf"), p, width = 9, height = 6)
#     
#   }
# }

