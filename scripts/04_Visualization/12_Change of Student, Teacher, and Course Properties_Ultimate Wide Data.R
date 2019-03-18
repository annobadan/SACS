
###'######################################################################
###'
###' Ultimate visualization
###' 
###' Change of student, teacher, course properties
###'
###' => Make a snippet before building a shiny app  
###'
###'
###' 20190101 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF")


### Call libraries
library(tidyverse)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import the cleaned dataset
###'
###'

### Ultimate wide dataset 2003-2017
setwd(data_dir)
load(file = "df_Ultimate_Merged.rda")
df <- df_to_save; rm(df_to_save)



###'######################################################################
###'
###' Prepare a loop
###'
###'

### Import variable names and labels
setwd(work_dir)
df_y <- read.csv(file = "tables/Outcome variable-names and labels.csv", 
                 header = FALSE)

names(df_y) <- c("y_name", "y_label")
classmode(df_y, everything())


### Assign vectors y names and labels
y_names_vec <- as.character(df_y$y_name)

y_labels_vec <- as.character(df_y$y_label)

all.equal(length(y_names_vec), length(y_labels_vec))



###'######################################################################
###'
###' Subset dataframe 
###'
###'

names(df)

### Subset variables for saving memory
df <- df %>%
  dplyr::select(CDSCode:PCT_CALPADS_UPC, 
                y_names_vec)


### Restrict to only traditional schools
tabdf(df, Traditional_DOC)
tabdf(df, Traditional_SOC)

df_sub <- df %>%
  filter(Traditional_SOC %in% c("Elementary School", 
                                "High School", 
                                "Intermediate/Middle/Junior High"))


### Exclude Charters! (about 9%)
tabdf(df_sub, Charter)
df_sub_traditional <- df_sub %>%
  filter(Charter == 0)



###'######################################################################
###'
###' Implement the for loop over all variables: (1) All 5 Quintiles
###' 
###'

for (i in 124:length(y_names_vec)){
  
  ### Generate temporary dataframe
  df_temp <- df_sub_traditional 
  
  
  ### Assign variable name
  idx <- which(names(df_temp) == y_names_vec[i])
  names(df_temp)[idx] <- "variable" 
  

  ###' Generate data to plot
  ###' Traditional School Type (level) by Poverty Level
  
  df_plot <- df_temp %>%
    group_by(AcademicYear, Traditional_SOC, PCT_FRPM_15bin) %>%
    summarise(mean_value = round(mean(variable, na.rm = TRUE), 2)) 
  
  
  ### Remove rows containing NaN and NA
  df_plot_nonmiss <- df_plot[complete.cases(df_plot),]
  
  
  ### Plot!
  p <- plot_trend_grp_facet(df_plot_nonmiss, 
                            x = AcademicYear, 
                            y = mean_value, 
                            group = PCT_FRPM_15bin, 
                            facet_formula = . ~ Traditional_SOC, 
                            yline = 2013, 
                            ylim = auto_ylim(df_plot$mean_value)) + 
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) 
    # geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed")
    
  
  
  labels <- labs(title = y_labels_vec[i], 
                 subtitle = NULL, 
                 caption = NULL, 
                 x = "Academic Year", y = y_labels_vec[i])
  
  p <- p + labels 
  
  
  ### Save as pdf file
  ggsave(paste0("figures/", "Ultimate_Wide_Plot_",
                sprintf("%02d", i), "_", y_names_vec[i],".pdf"), p, 
         width = 17, height = 9)



  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("Outcome Variable: ", 
             sprintf("%02d", i), "_", y_labels_vec[i], 
             " => completed", "\n"))
  
  
  ###'######################################################################
  ###' 
  ###' End for loop over 
  ###' outcome variables: i
  ###' 
  ###' 

}




###'######################################################################
###'
###' Implement the for loop over 123 variables: (2) Only Quintiles 1 and 5
###'
###'

for (i in 124:length(y_names_vec)){
  
  ### Generate temporary dataframe
  df_temp <- df_sub_traditional
  
  
  ### Assign variable name
  idx <- which(names(df_temp) == y_names_vec[i])
  names(df_temp)[idx] <- "variable" 
  
  
  ###' Generate data to plot
  ###' Traditional School Type (level) by Poverty Level
  
  tabdf(df_temp, PCT_FRPM_15bin)
  
  df_plot <- df_temp %>%
    filter(PCT_FRPM_15bin %in% c("Quintile 1", "Quintile 5")) %>%
    group_by(AcademicYear, Traditional_SOC, PCT_FRPM_15bin) %>%
    summarise(mean_value = round(mean(variable, na.rm = TRUE), 2)) 
  
  
  ### Remove rows containing NaN and NA
  df_plot_nonmiss <- df_plot[complete.cases(df_plot),]
  
  
  ### Plot!
  p <- plot_trend_grp_facet(df_plot_nonmiss, 
                            x = AcademicYear, 
                            y = mean_value, 
                            group = PCT_FRPM_15bin, 
                            facet_formula = . ~ Traditional_SOC, 
                            yline = 2013, 
                            ylim = auto_ylim(df_plot$mean_value)) + 
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) 
  # geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed")
  
  
  
  labels <- labs(title = y_labels_vec[i], 
                 subtitle = NULL, 
                 caption = NULL, 
                 x = "Academic Year", y = y_labels_vec[i])
  
  p <- p + labels 
  
  
  ### Save as pdf file
  ggsave(paste0("figures/", "Ultimate_Wide_Plot_Q1_Q5_",
                sprintf("%02d", i), "_", y_names_vec[i],".pdf"), p, 
         width = 17, height = 9)
  
  
  
  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("Outcome Variable: ", 
             sprintf("%02d", i), "_", y_labels_vec[i], 
             " => completed", "\n"))
  
  
  ###'######################################################################
  ###' 
  ###' End for loop over 
  ###' outcome variables: i
  ###' 
  ###' 
  
}

