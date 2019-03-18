
###'######################################################################
###'
###' Estimation Step 1 & Step 2 Visualization
###'
###' 
###' 20190111 JoonHo Lee
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
###' Import datasets
###'
###'

### The predicted counterfactual trends of district per-pupil revenue
setwd(work_dir)
load(file = "processed_data/Predicted_district_per-pupil_revenues.rda")
df_rev <- df_prepost


###' District information on actual/predicted per-pupil expenditures
###' LCFF-induced exogenous increases in district per-pupil expenditures
setwd(work_dir)
load(file = "processed_data/LCFF_induced_exogenous_PPE_2003_2016.rda")
df_exp <- df_to_save; rm(df_to_save)
df <- df_exp


### FY1314 Simuated IV
load(file = "processed_data/df_1314_SimIV.rda")
df_SimIV <- df_1314_SimIV


###'######################################################################
###'
###' Visualization #1.
###'
###' Estimation Step 1. 
###' 
###' => Predicting the counterfactual trends of district per-pupil revenue 
###'    if LCFF had not occurred  
###'    
###' => Plot Actual vs. Predicted per-pupil revenues
###'
###'

### Generate actual vs predicted data to plot (along with Fiscalyear)
df_pred_vs_actual <- df_prepost %>%
  group_by(Fiscalyear) %>%
  summarise(actual_rev = mean(state_rev_PP_16, na.rm = TRUE), 
            predicted_rev_mod1 = mean(pred_mod1, na.rm = TRUE), 
            predicted_rev_mod2 = mean(pred_mod2, na.rm = TRUE)) %>%
  mutate_at(.vars = c("actual_rev", "predicted_rev_mod1", "predicted_rev_mod2"), 
            .funs = round) %>%
  gather(actual_vs_pred, mean_rev_PP_16, 2:4)


###' Predictions from mod2 is much closer to actual than those from mod1 
###' Have smaller residuals
df_pred_diff <- df_prepost %>%
  filter(Fiscalyear < 2013) %>%
  mutate(pred_diff_mod1 = state_rev_PP_16 - pred_mod1, 
         pred_diff_mod2 = state_rev_PP_16 - pred_mod2)

summary(df_pred_diff$pred_diff_mod1)
summary(df_pred_diff$pred_diff_mod2)


### Plot only predictions from model 2
df_plot <- df_pred_vs_actual %>% 
  filter(actual_vs_pred != "predicted_rev_mod1") 

df_plot <- df_plot %>%
  mutate(actual_vs_pred = factor(actual_vs_pred, 
                                 levels = c("actual_rev",  "predicted_rev_mod2"), 
                                 labels = c("Observed district per-pupil revenues", 
                                            "Counterfactual predictions")))


### Plot!
yref <- 15000
ylim <- c(9000, 15400)
rec_y_incr <- (ylim[2] - ylim[1])/17

p <- ggplot(aes(x = Fiscalyear, y = mean_rev_PP_16, group = actual_vs_pred), 
            data = df_plot) + 
  
  # Add point, line, and text for coefficients
  geom_path(aes(linetype = actual_vs_pred, color = actual_vs_pred), size = 0.5) + 
  geom_point(aes(shape = actual_vs_pred, color = actual_vs_pred), size = 1.5) + 
  # geom_text(aes(label = comma(mean_rev_PP_16), 
  #               hjust = 0.5, vjust = 2.0), size = 2.3) +
  
  # Scales
  scale_x_continuous(breaks = seq(2003, 2016, by = 1)) + 
  scale_y_continuous(limits = ylim, labels = comma, 
                     breaks = seq(9000, 15000, by = 1000)) + 
  
  # Add x-axis reference lines and annotate periods
  geom_vline(xintercept = c(2007, 2012), color = "gray", linetype = "dashed") + 
  annotate("text", x = c(2004.5, 2009.5, 2014.5), y = yref, 
           label = c("Pre-recession", 
                     "Recession", 
                     "LCFF"), 
           color = "black", size = 4) + 
  annotate("rect", xmin = 2012, xmax = 2017, 
           ymin = ylim[1], 
           ymax = yref + rec_y_incr,
           alpha = .1) + 
  annotate("text", x = c(2007), y = 9200, 
           label = c("in-sample training with the pre-LCFF data"), 
           color = "black", size = 3) + 
  annotate("text", x = c(2014.5), y = 9400, 
           label = c("counterfactual extrapolation \n with the post-LCFF data"), 
           color = "black", size = 3) + 
  # annotate("segment", x = 2014.5, y = 9700, 
  #          xend = 2013, yend = 10461, 
  #          color = "black", size = 0.05, linetype = "dashed") + 
  
  # Theme
  theme_classic() + 
  theme(panel.background = element_blank(),
        panel.grid = element_blank(), 
        legend.position = "bottom", 
        legend.direction = "horizontal", 
        legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.0)) + 
  
  # Labels
  labs(title = NULL, 
       subtitle = "Observed vs. Predicted district per-pupil revenues from the State", 
       caption = NULL, 
       y = "Amount in real 2016 dollars",  
       x = "Year") + 
  
  # Mannual color and shapes
  scale_color_manual(values = c("dodgerblue1", "firebrick1")) + 
  scale_shape_manual(values = c(16, 17)) + 
  
  # Legend
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))


### Save the resulting plot
setwd("~/SACS/figures/20_AERJ Publication")
ggsave("Figure_01_Descriptive_04_Estimation Step 1_Counterfactual Predictions.pdf", 
       p, width = 6, height = 5)



###'######################################################################
###'
###' Visualization #2.
###'
###' Estimation Step 2. 
###' 
###' => Estimating the LCFF-induced exogenous increases 
###'    in district per-pupil expenditure
###'    
###'

### Generate new variables
tabdf(df_exp, decile)

df_exp <- df_exp %>%
  mutate(diff = .fitted - pred_mod2, 
    Q1Q5 = if_else(decile %in% c(1, 2), "Q1 mean (Low-poverty districts)", 
                         if_else(decile %in% c(9, 10), "Q5 mean (High-poverty districts)", 
                                NA_character_)))

df_plot <- df_exp %>%
  filter(factor %in% c("Total Expenditures")) %>%
  group_by(Fiscalyear, decile) %>%
  summarise(mean_value = mean(diff, na.rm = TRUE))

df_plot <- df_plot[complete.cases(df_plot), ]


### Plot!
p <- ggplot(aes(x = Fiscalyear, y = mean_value, group = decile), 
            data = df_plot) + 
  
  # Add point, line, and text for coefficients
  geom_path(aes(linetype = decile, color = decile), size = 0.5) + 
  geom_point(aes(shape = decile, color = decile), size = 1.5) + 
  facet_wrap(.~ decile, scales = "free")










