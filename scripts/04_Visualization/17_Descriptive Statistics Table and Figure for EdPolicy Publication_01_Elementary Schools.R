
###'######################################################################
###'
###' Descriptive statistics table and plots
###' 
###' for Educational Policy Paper Revision
###' 
###' (1) Elementary Schools only in LAUSD
###' 
###' 
###' 20190206 JoonHo Lee
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
library(cowplot)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Define functions for plotting
###'
###'

setwd(work_dir)

source(file = "scripts/04_Visualization/14_Define helper functions for building plots for AERJ publication.R")



###'######################################################################
###'
###' Import datasets
###'
###'

### Ultimate wide dataset 2003-2017
# setwd(data_dir)
load(file = "df_Ultimate_Merged.rda")
df <- df_to_save; rm(df_to_save)


# ###' District information on actual/predicted per-pupil expenditures
# ###' LCFF-induced exogenous increases in district per-pupil expenditures
# setwd(work_dir)
# load(file = "processed_data/LCFF_induced_exogenous_PPE_2003_2016.rda")
# df_pred <- df_to_save; rm(df_to_save)



###'######################################################################
###'
###' Select only necessary variables: Elementary schools
###'
###'

### Import variable names and labels
setwd(work_dir)
df_y <- read.csv(file = "tables/Outcome variable-names and labels.csv", 
                 header = FALSE)

names(df_y) <- c("y_name", "y_label")
classmode(df_y, everything())


###' Subset only necessary y variables
###' Exclude subjects other than ELA and Math for now

df_y$y_name
to_exclude <- c("_Social", "_Science", "FoLan", "_CTE", "Other", "Art", "PE")
vars_to_include <- !grepl(paste(to_exclude, collapse = "|"), df_y$y_name) 
tinker_cond <- df_y$y_name %in% c("PCT_Other",
                                  "N_Subject_Other", 
                                  "N_Course_Other", 
                                  "N_Period_Other")

df_y_sub <- df_y %>%
  filter(vars_to_include | tinker_cond)


### Assign vectors y names and labels
y_names_vec <- as.character(df_y_sub$y_name)

y_labels_vec <- as.character(df_y_sub$y_label)

all.equal(length(y_names_vec), length(y_labels_vec))


### Subset variables for saving memory
names(df)

df <- df %>%
  dplyr::select(CDSCode:PCT_CALPADS_UPC, 
                y_names_vec)



###'######################################################################
###'
###' Descriptive statistics (School-year panel)
###' 
###'    Elementary Schools  
###' 
###' (1) Prepare analysis
###'
###'

### Subset elementary schools only in LAUSD (Traditional public)
df_elem <- df %>%
  filter(Traditional_SOC %in% c("Elementary School")) %>% 
  filter(DistrictCode == 64733) %>%
  filter(Charter == 0)


### Generate Quintiles based on PCT_FRPM_15mean
df_elem_bin <- df_elem %>%
  group_by(CountyCode, DistrictCode, SchoolCode) %>%
  summarise(PCT_FRPM_15mean = first(PCT_FRPM_15mean))

classmode(df_elem_bin, everything())


df_elem_bin <- df_elem_bin %>%
  ungroup() %>%
  mutate(bin = ntile(PCT_FRPM_15mean, 5))

tabdf(df_elem_bin, bin)


### Join with the original dataframe
df_elem <- df_elem %>%
  left_join(df_elem_bin, by = c("CountyCode", "DistrictCode", "SchoolCode"))

tabdf(df_elem, bin)



###'######################################################################
###'
###' Descriptive statistics (School-year panel)
###' 
###'    Elementary Schools  
###' 
###' (2) Subgroup analysis for Q1, Q5
###'
###'

### Table (1) Summarize Subgroup Number of observations
tbl_desc_N <- df_elem %>%
  filter(bin %in% c(1, 5)) %>%
  group_by(Traditional_SOC, bin) %>%
  summarise(N = n(), 
            N_Schools = n_distinct(SchoolCode))


### Table (2) Summarize Subgroup Means
vars_to_desc <- names(df_elem %>% select(Total_Enroll, PCT_FRPM, PCT_New_in_dist, 
                                         mean_yrs_in_dist, PCT_New_teach, PCT_Tenured,
                                         PCT_Longterm_Sub, PCT_Probationary, 
                                         PCT_White_Tch, PCT_Master_Plus, 
                                         MN_size_SelfCon, MN_size_ELA, MN_size_Math, 
                                         MN_Period_ELA, MN_Period_Math, 
                                         N_Course_Total, PCT_Always_AG_ELA, 
                                         PCT_Always_AG_Math, PCT_Not_AG_ELA, 
                                         PCT_Not_AG_Math, PCT_AP_ELA, PCT_AP_Math, 
                                         NewT_EL_SelfCon, NewT_EL_ELA, NewT_EL_Math, 
                                         Tenure_EL_SelfCon, Tenure_EL_ELA, Tenure_EL_Math, 
                                         AGApprvd_EL_ELA, AGApprvd_EL_Math, 
                                         AP_EL_ELA, AP_EL_Math))

tbl_desc_Sub <- df_elem %>%
  filter(bin %in% c(1, 5)) %>%
  group_by(Traditional_SOC, bin) %>%
  summarise_at(.vars = vars_to_desc, 
               .funs = function(x) mean(x, na.rm = TRUE))


tbl_desc_Sub_transpose <- tbl_desc_Sub %>%
  gather(key = key, value = value, Total_Enroll:AP_EL_Math) %>%
  spread(key = bin, value = value) %>%
  rename(variable = key, Q1 = "1", Q5 = "5") %>%
  arrange(match(variable, vars_to_desc))


write.csv(tbl_desc_Sub_transpose, file = "tbl_desc_Sub_LAUSD.csv")



###'######################################################################
###'
###' Descriptive statistics (School-year panel)
###' 
###'    Elementary Schools  
###' 
###' (3) Entact group analysis: Mean and SD
###'
###'

### Observations
tbl_desc_N <- df_elem %>%
  group_by(Traditional_SOC) %>%
  summarise(N = n(), 
            N_Schools = n_distinct(SchoolCode))


### Mean
tbl_desc_Mean <- df_elem %>%
  group_by(Traditional_SOC) %>%
  summarise_at(.vars = vars_to_desc, 
               .funs = function(x) mean(x, na.rm = TRUE)) %>%
  gather(key = key, value = value, Total_Enroll:AP_EL_Math)

write.csv(tbl_desc_Mean, file = "tbl_desc_Mean_LAUSD.csv")


### sd
tbl_desc_sd <- df_elem %>%
  group_by(Traditional_SOC) %>%
  summarise_at(.vars = vars_to_desc, 
               .funs = function(x) sd(x, na.rm = TRUE)) %>%
  gather(key = key, value = value, Total_Enroll:AP_EL_Math)


write.csv(tbl_desc_sd, file = "tbl_desc_sd_LAUSD.csv")



###'######################################################################
###'
###' Descriptive statistics (School-year panel)
###' 
###'    Elementary Schools  
###' 
###' (4) Generate Growth Plots: Teacher Staffing
###'
###'

### (1) % New in district
p1 <- plot_LCFF_means(df_elem, PCT_New_in_dist, bin, 
                      ylim = c(0, 10), yref = 9.2, 
                      title = "% Teachers newly hired in the district")


### (2) % Novice teachers
p2 <- plot_LCFF_means(df_elem, PCT_New_teach, bin, 
                      ylim = c(0, 10), yref = 9.2, 
                      title = "% Novice teachers")


### (3) % Long-term substitutes/temporary employees
p3 <- plot_LCFF_means(df_elem, PCT_Longterm_Sub, bin, 
                      ylim = c(0, 10), yref = 9.2, 
                      title = "% Long-term substitutes/temporary employees")


### (4) % Probationary teachers
p4 <- plot_LCFF_means(df_elem, PCT_Probationary, bin, 
                      ylim = c(0, 15), yref = 14, 
                      title = "% Probationary teachers")


### (5) % Tenured teachers
p5 <- plot_LCFF_means(df_elem, PCT_Tenured, bin, 
                      ylim = c(80, 100), yref = 98.5, 
                      title = "% Tenured teachers")


### (6) % White teachers
p6 <- plot_LCFF_means(df_elem, PCT_White_Tch, bin, 
                      ylim = c(0, 100), yref = 98, 
                      title = "% White teachers")


### (7) % Teachers holding a master's degree or above
p7 <- plot_LCFF_means(df_elem, PCT_Master_Plus, bin, 
                      ylim = c(10, 55), yref = 54, 
                      title = "% Teachers holding a master's degree or above")


### (8) School average class size (Homeroom class)
p8 <- plot_LCFF_means(df_elem, MN_size_SelfCon, bin, 
                      ylim = c(17.5, 25), yref = 24, 
                      title = "School average class size (Homeroom class)")

setwd("~/SACS/figures/21_Educational Policy Publication")
save(p8, file = "p8_homeroom_classsize.rda")



### Combine as a row
p_row1 <- plot_grid(p1 + theme(legend.position="none"),
                    p2 + theme(legend.position="none"),
                    nrow = 1, labels = c("A", "B"), align = 'h')

p_row2 <- plot_grid(p4 + theme(legend.position="none"), 
                    p5 + theme(legend.position="none"), 
                    nrow = 1, labels = c("C", "D"), align = 'h')

legend <- get_legend(p8 + theme(legend.position="bottom"))


p_row_legend <- plot_grid(p_row1, legend,
                          p_row2, legend, 
                          ncol = 1, rel_heights = c(1, .1, 1, .1))

setwd("~/SACS/figures/21_Educational Policy Publication")
ggsave("Figure_01_Descriptive_01_Teacher Staffing_Elementary.pdf", 
       p_row_legend, width = 8, height = 8)






