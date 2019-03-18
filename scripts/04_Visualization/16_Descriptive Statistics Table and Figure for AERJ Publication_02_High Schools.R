
###'######################################################################
###'
###' Descriptive statistics table and plots
###' 
###' (2) High Schools
###' 
###' 
###' 20190109 JoonHo Lee
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
###' Select only necessary variables: High schools
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
###'    High Schools  
###' 
###' (1) Prepare analysis
###'
###'

### Subset High schools (Traditional public)
df_high <- df %>%
  filter(Traditional_SOC %in% c("High School")) %>%
  filter(Charter == 0)


### Generate Quintiles based on PCT_FRPM_15mean
df_high_bin <- df_high %>%
  group_by(CountyCode, DistrictCode, SchoolCode) %>%
  summarise(PCT_FRPM_15mean = first(PCT_FRPM_15mean))

classmode(df_high_bin, everything())


df_high_bin <- df_high_bin %>%
  ungroup() %>%
  mutate(bin = ntile(PCT_FRPM_15mean, 5))

tabdf(df_high_bin, bin)


### Join with the original dataframe
df_high <- df_high %>%
  left_join(df_high_bin, by = c("CountyCode", "DistrictCode", "SchoolCode"))

tabdf(df_high, bin)



###'######################################################################
###'
###' Descriptive statistics (School-year panel)
###' 
###'    High Schools  
###' 
###' (2) Subgroup analysis for Q1, Q2
###'
###'

### Table (1) Summarize Subgroup Number of observations
tbl_desc_N <- df_high %>%
  filter(bin %in% c(1, 5)) %>%
  group_by(Traditional_SOC, bin) %>%
  summarise(N = n(), 
            N_Schools = n_distinct(SchoolCode))


### Table (2) Summarize Subgroup Means
vars_to_desc <- names(df_high %>% select(Total_Enroll, PCT_FRPM, PCT_New_in_dist, 
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

tbl_desc_Sub <- df_high %>%
  filter(bin %in% c(1, 5)) %>%
  group_by(Traditional_SOC, bin) %>%
  summarise_at(.vars = vars_to_desc, 
               .funs = function(x) mean(x, na.rm = TRUE))


tbl_desc_Sub_transpose <- tbl_desc_Sub %>%
  gather(key = key, value = value, Total_Enroll:AP_EL_Math) %>%
  spread(key = bin, value = value) %>%
  rename(variable = key, Q1 = "1", Q5 = "5") %>%
  arrange(match(variable, vars_to_desc))


write.csv(tbl_desc_Sub_transpose, file = "tbl_desc_Sub.csv")



###'######################################################################
###'
###' Descriptive statistics (School-year panel)
###' 
###'    High Schools  
###' 
###' (3) Entact group analysis: Mean and SD
###'
###'

### Observations
tbl_desc_N <- df_high %>%
  group_by(Traditional_SOC) %>%
  summarise(N = n(), 
            N_Schools = n_distinct(SchoolCode))


### Mean
tbl_desc_Mean <- df_high %>%
  group_by(Traditional_SOC) %>%
  summarise_at(.vars = vars_to_desc, 
               .funs = function(x) mean(x, na.rm = TRUE)) %>%
  gather(key = key, value = value, Total_Enroll:AP_EL_Math)

write.csv(tbl_desc_Mean, file = "tbl_desc_Mean.csv")


### sd
tbl_desc_sd <- df_high %>%
  group_by(Traditional_SOC) %>%
  summarise_at(.vars = vars_to_desc, 
               .funs = function(x) sd(x, na.rm = TRUE)) %>%
  gather(key = key, value = value, Total_Enroll:AP_EL_Math)


write.csv(tbl_desc_sd, file = "tbl_desc_sd.csv")



###'######################################################################
###'
###' Descriptive statistics (School-year panel)
###' 
###'    High schools
###' 
###' (4) Generate Growth Plots - Teaching Staffs and Organizational Properties
###'
###'

### Generate a code snippet with % New in district
tabdf(df_high, bin)
tabdf(df_high, Charter)


### Import 
setwd("~/SACS/figures/20_AERJ Publication")
load(file = "p8_homeroom_classsize.rda")
p_homeroom_size <- p8; rm(p8)


### (1) % New in district
p1 <- plot_LCFF_means(df_high, PCT_New_in_dist, bin, 
                      ylim = c(0, 20), yref = 18, 
                      title = "% Teachers newly hired in the district")


### (2) % Novice teachers
p2 <- plot_LCFF_means(df_high, PCT_New_teach, bin, 
                      ylim = c(0, 15), yref = 14, 
                      title = "% Novice teachers")


### (3) % Long-term substitutes/temporary employees
p3 <- plot_LCFF_means(df_high, PCT_Longterm_Sub, bin, 
                      ylim = c(0, 15), yref = 14, 
                      title = "% Long-term substitutes/temporary employees")


### (4) % Probationary teachers
p4 <- plot_LCFF_means(df_high, PCT_Probationary, bin, 
                      ylim = c(0, 30), yref = 28, 
                      title = "% Probationary teachers")


### (5) % Tenured teachers
p5 <- plot_LCFF_means(df_high, PCT_Tenured, bin, 
                      ylim = c(50, 100), yref = 97, 
                      title = "% Tenured teachers")


### (6) % White teachers
p6 <- plot_LCFF_means(df_high, PCT_White_Tch, bin, 
                      ylim = c(0, 100), yref = 93, 
                      title = "% White teachers")


### (7) % Teachers holding a master's degree or above
p7 <- plot_LCFF_means(df_high, PCT_Master_Plus, bin, 
                      ylim = c(20, 60), yref = 57, 
                      title = "% Teachers holding a master's degree or above")


### (8) Total number of courses offered in the high school
p8 <- plot_LCFF_means(df_high, N_Course_Total, bin, 
                      ylim = c(40, 110), yref = 105, 
                      title = "Total number of courses offered (High)")


### (9) Class periods assigned to teachers (Math)
p9 <- plot_LCFF_means(df_high, MN_Period_Math, bin, 
                      ylim = c(3.5, 4.70), yref = 4.62, 
                      title = "Class periods assigned to math teachers")


### (10) % Classes always approved as A-G (Math)
p10 <- plot_LCFF_means(df_high, PCT_Always_AG_Math, bin, 
                       ylim = c(40, 80), yref = 77, 
                       title = "% Math classes always approved as A-G")


### Combine as a row
p_row1 <- plot_grid(p_homeroom_size + theme(legend.position="none"),
                    p8 + theme(legend.position="none"), 
                    nrow = 1, labels = c("A", "B"), align = 'h')

legend <- get_legend(p8 + theme(legend.position="bottom"))

p_row2 <- plot_grid(p9 + theme(legend.position="none"),
                    p10 + theme(legend.position="none"), 
                    nrow = 1, labels = c("C", "D"), align = 'h')

legend <- get_legend(p9 + theme(legend.position="bottom"))


p_row_legend <- plot_grid(p_row1, legend,
                          p_row2, legend, 
                          ncol = 1, rel_heights = c(1, .1, 1, .1))

setwd("~/SACS/figures/20_AERJ Publication")
ggsave("Figure_01_Descriptive_02_Class Properties_Elementary and High.pdf", 
       p_row_legend, width = 8, height = 8)




###'######################################################################
###'
###' Descriptive statistics (School-year panel)
###' 
###'    High schools
###' 
###' (5) Generate Growth Plots 
###' 
###'  - EL's access to experienced teacher and rigorous curriculum path 
###'
###'

### (1) %EL taught by the novice - %EL taught by the experienced - Math
p1 <- plot_LCFF_means2(df_high, NewT_EL_Math, bin, 
                       ylim = c(-2, 15), yref = 13.5, 
                       subtitle = "Avg. %EL in classes taught by the novice - \n Avg. %EL in classes taught by the experienced", 
                       title = "High school math classes") + 
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")



### (2) %EL taught by the novice - %EL taught by the experienced - ELA
p2 <- plot_LCFF_means2(df_high, NewT_EL_ELA, bin, 
                       ylim = c(-2, 15), yref = 13.5, 
                       subtitle = "Avg. %EL in classes taught by the novice- \n Avg. %EL in classes taught by the experienced", 
                       title = "High school ELA classes") + 
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")



### (3) %EL taught by the non-tenured - %EL taught by the tenured - Math
tabdf(df_high, Tenure_EL_Math)
summary(df_high$Tenure_EL_Math)

df_high <- df_high %>% 
  mutate(NonTenure_EL_Math = -1 * Tenure_EL_Math, 
         NonTenure_EL_ELA = -1 * Tenure_EL_ELA)

summary(df_high$NonTenure_EL_Math)

p3 <- plot_LCFF_means2(df_high, NonTenure_EL_Math, bin, 
                       ylim = c(-2, 15), yref = 13.5, 
                       subtitle = "Avg. %EL in classes taught by the nontenured - \n Avg %EL in classes taught by the tenured", 
                       title = "High school math classes") + 
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")



### (4) %EL taught by the non-tenured - %EL taught by the tenured - ELA
summary(df_high$Tenure_EL_ELA)
summary(df_high$NonTenure_EL_ELA)

p4 <- plot_LCFF_means2(df_high, NonTenure_EL_ELA, bin, 
                       ylim = c(-2, 15), yref = 13.5, 
                       subtitle = "Avg. %EL in classes taught by the nontenured - \n Avg. %EL in classes taught by the tenured", 
                       title = "High school ELA classes") + 
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")


### (5) %EL enrolled in classes approved as A-G – %EL enrolled in classes not approved as A-G
###     - Math
p5 <- plot_LCFF_means2(df_high, AGApprvd_EL_Math, bin, 
                       ylim = c(-25, 2), yref = 2, 
                       subtitle = "Avg. %EL in A-G classes – \n Avg. %EL in non-A-G classes", 
                       title = "High school math classes") + 
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")



### (6) %EL enrolled in classes approved as A-G – %EL enrolled in classes not approved as A-G
###     - ELA
p6 <- plot_LCFF_means2(df_high, AGApprvd_EL_ELA, bin, 
                       ylim = c(-50, 2), yref = 2, 
                       subtitle = "Avg. %EL in A-G classes – \n Avg. %EL in non-A-G classes", 
                       title = "High school ELA classes") + 
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")


### Combine plots: Only two
legend <- get_legend(p3 + theme(legend.position="bottom"))

p_row1 <- plot_grid(p3 + theme(legend.position="none"), 
                    p5 + theme(legend.position="none"), 
                    nrow = 1, 
                    labels = c("A", "B"), align = "h")

p_row1_legend <- plot_grid(p_row1, legend, ncol = 1, rel_heights = c(1, .1))

setwd("~/SACS/figures/20_AERJ Publication")
ggsave("Figure_01_Descriptive_03_EL's access to resources.pdf", 
       p_row1_legend, width = 8, height = 4)




### Combine plots

legend <- get_legend(p1 + theme(legend.position="bottom"))

p_row1 <- plot_grid(p1 + theme(legend.position="none"), 
                    p3 + theme(legend.position="none"), 
                    nrow = 1, 
                    labels = c("A", "B"), align = "h")

p_row1_legend <- plot_grid(p_row1, legend, ncol = 1, rel_heights = c(1, .1))


p_row2 <- plot_grid(p5 + theme(legend.position="none"),
                    p6 + theme(legend.position="none"), 
                    nrow = 1, labels = c("C", "D"), align = 'h')

p_row2_legend <- plot_grid(p_row2, legend, ncol = 1, rel_heights = c(1, .1))


final_plot_grid <- plot_grid(p_row1_legend,
                             p_row2_legend,
                             ncol = 1, 
                             align = 'v', axis = 'l', 
                             rel_heights = c(1, 1))


setwd("~/SACS/figures/20_AERJ Publication")
ggsave("Figure_01_Descriptive_03_EL's access to resources.pdf", 
       final_plot_grid, width = 8, height = 8)


