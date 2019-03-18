
###'######################################################################
###'
###' Task    : Relating Kindergarten Program Type with School Poverty variables
###'           Generate snippet
###'           
###' Category: Visualization
###' 
###' Data    : CBEDS Data about Schools & Districts
###'           2015-16, 2016-17, 2017-18
###' 
###' Date    : 2019-02-14
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
data_dir <- c("D:/Data/LCFF/Staff_Data/Certificated_Staff/CBEDS Data")


### Call libraries
library(tidyverse)
library(readxl)
library(Hmisc)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Load the managed dataset
###'
###'

setwd(data_dir)
load(file = "CBEDS_School_Data_01_Kindergarten_Program_Type_3-year-long_managed.rda")
df_elem <- df_to_save



###'######################################################################
###'
###' Subset elementary schools
###' 
###' Condition: Elementary schools serving Kindergarten Grade Kids
###' 
###' Do not condition on school with all 3-year data
###'
###'

### Check distributions
tabdf(df_elem, AcademicYear)
tabdf(df_elem, SOC)
tabdf(df_elem, Charter)

tabdf(df_elem, Kinder_Offered)
tabdf(df_elem, Kinder_Served)


### Subset only elementary schools if Kinder_Offered == 1
df_elem_served <- df_elem %>%
  filter(Kinder_Served == "Served")



###'######################################################################
###'
###' Generate a simplified version of factors
###'
###'

tabdf(df_elem_served, AcademicYear)
tabdf(df_elem_served, Kindergarten3)
tabdf(df_elem_served, quartile)

classmode(df_elem_served, AcademicYear)
classmode(df_elem_served, Kindergarten3)
classmode(df_elem_served, quartile)


df_elem_served <- df_elem_served %>%
  mutate(
    
    AcademicYear = case_when(
      
      AcademicYear == 2016 ~ NA_real_, 
      TRUE ~ AcademicYear
      
    ), 
    
    AcademicYear = factor(AcademicYear, levels = c(2015, 2017)), 
    
    Kindergarten3 = case_when(
      
      Kindergarten3 == "None" ~ NA_character_, 
      TRUE ~ as.character(as_factor(Kindergarten3))
      
    ), 
    
    Kindergarten3 = factor(Kindergarten3, levels = c("Full day", "Part day")), 
    
    quartile = case_when(
      
     quartile %in% c("Quartile2", "Quartile3") ~ NA_character_, 
     TRUE ~ as.character(as_factor(quartile))
      
    ),
    
    quartile = factor(quartile, 
                      levels = c("Quartile1", "Quartile4"))
  )



###'######################################################################
###'
###' Plot 1: Overall Pattern 
###' 
###'

setwd(work_dir)

### (1) Kindergarten Program Type

df_plot <- df_elem_served %>%
  group_by(Kindergarten3, AcademicYear) %>%
  summarise(N = n()) %>%
  drop_na


p <- plot_proportions_grp(df_plot, AcademicYear, N, Kindergarten3) + 
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))


p <- p + labs(title = "Kindergarten Program Type", 
              subtitle = "Traditional Elementary Schools in California", 
              x = "Academic Year", 
              y = "Number of elementary schools", 
              caption = NULL)

ggsave("figures/Kindergarten_01_Overall_01_Kindergarten.pdf", p, 
       width = 6, height = 5)


### (2) Transitional Kindergarten Program Type

df_plot <- df_elem_served %>%
  group_by(Transitional_K3, AcademicYear) %>%
  summarise(N = n()) %>%
  drop_na

p <- plot_proportions_grp(df_plot, AcademicYear, N, Transitional_K3) + 
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) 

p <- p + labs(title = "Transitional Kindergarten Program Type", 
              subtitle = "Traditional Elementary Schools in California", 
              x = "Academic Year", 
              y = "Number of elementary schools", 
              caption = NULL)

ggsave("figures/Kindergarten_01_Overall_02_Transitonal Kindergarten.pdf", p, 
       width = 6, height = 5)



###'######################################################################
###'
###' Plot 2: Split plot by student poverty level
###'
###' (1) Kindergarten Program Type
###'
###'

df_plot <- df_elem_served %>%
  group_by(quartile, Kindergarten3, AcademicYear) %>%
  summarise(N = n()) %>%
  ungroup() %>%
  group_by(quartile, AcademicYear) %>%
  mutate(group_sum = sum(N, na.rm = TRUE), 
         percent = N/group_sum * 100, 
         # don't need to calculate the label positions from ggplot 2.1.0 
         # position = cumsum(amount) - 0.5 * amount,  
         label_text = paste0(sprintf("%.1f", percent), "%")) %>%
  drop_na

group_total <- df_plot %>%
  group_by(quartile, AcademicYear) %>%
  summarise(group_sum = first(group_sum))


p <- ggplot(df_plot) + 
  aes(x = AcademicYear, y = N, fill = Kindergarten3) + 
  geom_bar(position = position_stack(reverse = TRUE), 
           stat = "identity", width = 0.7) +
  geom_text(aes(label = label_text), 
            position = position_stack(vjust = 0.5, reverse = TRUE), size = 3) + 
  geom_text(data = group_total,
            aes(x = AcademicYear, y = group_sum + mean(df_plot$group_sum)/30,
                label = comma(group_sum), fill = NULL),
            size = 3) + 
  facet_grid(.~ quartile, scales = "fixed") +
  # scale_x_continuous(breaks = seq(min(df_plot$AcademicYear), max(df_plot$AcademicYear), 
  #                                 by = 1)) + 
  scale_y_continuous(labels = comma) + 
  theme_trend + temp_labels + 
  scale_color_manual(values = color_palette[seq(unique(df_plot$Kindergarten3))]) + 
  scale_shape_manual(values = shape_palette[seq(unique(df_plot$Kindergarten3))]) + 
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) + 
  scale_fill_brewer(palette = "Paired")


p <- p + labs(title = "Kindergarten Program Type by School Poverty Levels", 
              subtitle = "Traditional Elementary Schools in California", 
              x = "Academic Year", 
              y = "Number of elementary schools", 
              caption = "Note: Highest (lowest) poverty schools are those in Quartile 4 (Quartile 1) of school-level distributions of 3-year mean percentage of FRPM students (2015-17). 
Quartile 1: 0% ~ 37.1%, Quartile 2: 37.1% ~ 70.3%, Quartile 3: 70.3% ~ 86.8%, Quartile 4: 86.8% ~ 100%.") + 
  theme(
    plot.caption = element_text(hjust = 0)
  )

ggsave("figures/Kindergarten_02_By FRPM Quartiles_01_Kindergarten.pdf", p, 
       width = 10, height = 5)



###'######################################################################
###'
###' Plot 2: Split plot by student poverty level
###'
###' (2) Transitional Kindergarten Program Type
###'
###'

df_plot <- df_elem_served %>%
  group_by(quartile, Transitional_K3, AcademicYear) %>%
  summarise(N = n()) %>%
  ungroup() %>%
  group_by(quartile, AcademicYear) %>%
  mutate(group_sum = sum(N, na.rm = TRUE), 
         percent = N/group_sum * 100, 
         # don't need to calculate the label positions from ggplot 2.1.0 
         # position = cumsum(amount) - 0.5 * amount,  
         label_text = paste0(sprintf("%.1f", percent), "%")) %>%
  drop_na

group_total <- df_plot %>%
  group_by(quartile, AcademicYear) %>%
  summarise(group_sum = first(group_sum))


p <- ggplot(df_plot) + 
  aes(x = AcademicYear, y = N, fill = Transitional_K3) + 
  geom_bar(position = position_stack(reverse = TRUE), 
           stat = "identity", width = 0.7) +
  geom_text(aes(label = label_text), 
            position = position_stack(vjust = 0.5, reverse = TRUE), size = 3) + 
  geom_text(data = group_total,
            aes(x = AcademicYear, y = group_sum + mean(df_plot$group_sum)/30,
                label = comma(group_sum), fill = NULL),
            size = 3) + 
  facet_grid(.~ quartile, scales = "fixed") +
  # scale_x_continuous(breaks = seq(min(df_plot$AcademicYear), max(df_plot$AcademicYear), 
  #                                 by = 1)) + 
  scale_y_continuous(labels = comma) + 
  theme_trend + temp_labels + 
  scale_color_manual(values = color_palette[seq(unique(df_plot$Transitional_K3))]) + 
  scale_shape_manual(values = shape_palette[seq(unique(df_plot$Transitional_K3))]) + 
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) + 
  scale_fill_brewer(palette = "Paired")


p <- p + labs(title = "Transitional Kindergarten Program Type by School Poverty Levels", 
              subtitle = "Traditional Elementary Schools in California", 
              x = "Academic Year", 
              y = "Number of elementary schools", 
              caption = "Note: Highest (lowest) poverty schools are those in Quartile 4 (Quartile 1) of school-level distributions of 3-year mean percentage of FRPM students (2015-17). 
Quartile 1: 0% ~ 37.1%, Quartile 2: 37.1% ~ 70.3%, Quartile 3: 70.3% ~ 86.8%, Quartile 4: 86.8% ~ 100%.") + 
  theme(
    plot.caption = element_text(hjust = 0)
  )

ggsave("figures/Kindergarten_02_By FRPM Quartiles_02_Transitional Kindergarten.pdf", p, 
       width = 10, height = 5)





###'######################################################################
###'
###' Plot 3: Split plot by student poverty level
###'
###' => Only for Charter Schools
###'
###' (1) Kindergarten Program Type
###'
###'

df_plot <- df_elem_served %>%
  filter(Charter == 1) %>%  # Filter only charter schools
  group_by(quartile, Kindergarten3, AcademicYear) %>%
  summarise(N = n()) %>%
  ungroup() %>%
  group_by(quartile, AcademicYear) %>%
  mutate(group_sum = sum(N, na.rm = TRUE), 
         percent = N/group_sum * 100, 
         # don't need to calculate the label positions from ggplot 2.1.0 
         # position = cumsum(amount) - 0.5 * amount,  
         label_text = paste0(sprintf("%.1f", percent), "%")) %>%
  drop_na

group_total <- df_plot %>%
  group_by(quartile, AcademicYear) %>%
  summarise(group_sum = first(group_sum))


p <- ggplot(df_plot) + 
  aes(x = AcademicYear, y = N, fill = Kindergarten3) + 
  geom_bar(position = position_stack(reverse = TRUE), 
           stat = "identity", width = 0.7) +
  geom_text(aes(label = label_text), 
            position = position_stack(vjust = 0.5, reverse = TRUE), size = 3) + 
  geom_text(data = group_total,
            aes(x = AcademicYear, y = group_sum + mean(df_plot$group_sum)/30,
                label = comma(group_sum), fill = NULL),
            size = 3) + 
  facet_wrap(.~ quartile, scales = "free", nrow = 1) +
  # scale_x_continuous(breaks = seq(min(df_plot$AcademicYear), max(df_plot$AcademicYear), 
  #                                 by = 1)) + 
  scale_y_continuous(labels = comma) + 
  theme_trend + temp_labels + 
  scale_color_manual(values = color_palette[seq(unique(df_plot$Kindergarten3))]) + 
  scale_shape_manual(values = shape_palette[seq(unique(df_plot$Kindergarten3))]) + 
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) + 
  scale_fill_brewer(palette = "Paired")


p <- p + labs(title = "Kindergarten Program Type by School Poverty Levels", 
              subtitle = "Charter Elementary Schools in California", 
              x = "Academic Year", 
              y = "Number of elementary schools", 
              caption = "Note: Highest (lowest) poverty schools are those in Quartile 4 (Quartile 1) of school-level distributions of 3-year mean percentage of FRPM students (2015-17). 
Quartile 1: 0% ~ 37.1%, Quartile 2: 37.1% ~ 70.3%, Quartile 3: 70.3% ~ 86.8%, Quartile 4: 86.8% ~ 100%.") + 
  theme(
    plot.caption = element_text(hjust = 0)
  )

ggsave("figures/Kindergarten_03_By FRPM Quartiles_01_Kindergarten_Only Charter.pdf", p, 
       width = 10, height = 5)



###'######################################################################
###'
###' Plot 3: Split plot by student poverty level
###' 
###' => Only for Charter Schools
###'
###' (2) Transitional Kindergarten Program Type
###'
###'

df_plot <- df_elem_served %>%
  filter(Charter == 1) %>%  # Filter only charter schools
  group_by(quartile, Transitional_K3, AcademicYear) %>%
  summarise(N = n()) %>%
  ungroup() %>%
  group_by(quartile, AcademicYear) %>%
  mutate(group_sum = sum(N, na.rm = TRUE), 
         percent = N/group_sum * 100, 
         # don't need to calculate the label positions from ggplot 2.1.0 
         # position = cumsum(amount) - 0.5 * amount,  
         label_text = paste0(sprintf("%.1f", percent), "%")) %>%
  drop_na

group_total <- df_plot %>%
  group_by(quartile, AcademicYear) %>%
  summarise(group_sum = first(group_sum))


p <- ggplot(df_plot) + 
  aes(x = AcademicYear, y = N, fill = Transitional_K3) + 
  geom_bar(position = position_stack(reverse = TRUE), 
           stat = "identity", width = 0.7) +
  geom_text(aes(label = label_text), 
            position = position_stack(vjust = 0.5, reverse = TRUE), size = 3) + 
  geom_text(data = group_total,
            aes(x = AcademicYear, y = group_sum + mean(df_plot$group_sum)/30,
                label = comma(group_sum), fill = NULL),
            size = 3) + 
  facet_wrap(.~ quartile, scales = "free", nrow = 1) +
  # scale_x_continuous(breaks = seq(min(df_plot$AcademicYear), max(df_plot$AcademicYear), 
  #                                 by = 1)) + 
  scale_y_continuous(labels = comma) + 
  theme_trend + temp_labels + 
  scale_color_manual(values = color_palette[seq(unique(df_plot$Transitional_K3))]) + 
  scale_shape_manual(values = shape_palette[seq(unique(df_plot$Transitional_K3))]) + 
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) + 
  scale_fill_brewer(palette = "Paired")


p <- p + labs(title = "Transitional Kindergarten Program Type by School Poverty Levels", 
              subtitle = "Charter Elementary Schools in California", 
              x = "Academic Year", 
              y = "Number of elementary schools", 
              caption = "Note: Highest (lowest) poverty schools are those in Quartile 4 (Quartile 1) of school-level distributions of 3-year mean percentage of FRPM students (2015-17). 
Quartile 1: 0% ~ 37.1%, Quartile 2: 37.1% ~ 70.3%, Quartile 3: 70.3% ~ 86.8%, Quartile 4: 86.8% ~ 100%.") + 
  theme(
    plot.caption = element_text(hjust = 0)
  )

ggsave("figures/Kindergarten_03_By FRPM Quartiles_02_Transitional Kindergarten_Only Charter.pdf", p, 
       width = 10, height = 5)



###'######################################################################
###'
###' Plot 4: Bivariate relationship between
###'
###' (1) Kindergarten Program Type
###' 
###' (2) Transitional Kindergarten Program Type
###'
###'

df_plot <- df_elem_served %>%
  group_by(Kindergarten3, Transitional_K3, AcademicYear) %>%
  summarise(N = n()) %>%
  ungroup() %>%
  group_by(Kindergarten3, AcademicYear) %>%
  mutate(group_sum = sum(N, na.rm = TRUE), 
         percent = N/group_sum * 100, 
         # don't need to calculate the label positions from ggplot 2.1.0 
         # position = cumsum(amount) - 0.5 * amount,  
         label_text = paste0(sprintf("%.1f", percent), "%")) %>%
  drop_na

group_total <- df_plot %>%
  group_by(Kindergarten3, AcademicYear) %>%
  summarise(group_sum = first(group_sum))


p <- ggplot(df_plot) + 
  aes(x = AcademicYear, y = N, fill = Transitional_K3) + 
  geom_bar(position = position_stack(reverse = TRUE), 
           stat = "identity", width = 0.7) +
  geom_text(aes(label = label_text), 
            position = position_stack(vjust = 0.5, reverse = TRUE), size = 3) + 
  # geom_text(data = group_total,
  #           aes(x = AcademicYear, y = group_sum + mean(df_plot$group_sum)/30,
  #               label = comma(group_sum), fill = NULL),
  #           size = 3) + 
  facet_wrap(~ Kindergarten3, scales = "free") +
  # scale_x_continuous(breaks = seq(min(df_plot$AcademicYear), max(df_plot$AcademicYear), 
  #                                 by = 1)) + 
  scale_y_continuous(labels = comma) + 
  theme_bw() + 
  theme(panel.background = element_blank(),
        panel.grid = element_blank(), 
        legend.position = "bottom", 
        legend.direction = "horizontal") + 
  temp_labels + 
  scale_color_manual(values = color_palette[seq(unique(df_plot$Kindergarten3))]) + 
  scale_shape_manual(values = shape_palette[seq(unique(df_plot$Kindergarten3))]) + 
  guides(title = "Transitional Kindergarten Type", 
         fill = guide_legend(nrow = 1, byrow = TRUE)) + 
  scale_fill_brewer(palette = "Paired")


p <- p + labs(title = "Percentages of Transitional Kingergarten Program Types within each Kindergarten Program", 
              subtitle = "Traditional Elementary Schools in California", 
              x = "Academic Year", 
              y = "Number of elementary schools", 
              caption = NULL)

ggsave("figures/Kindergarten_04_Transiontal_K Programs within each Kindergarten Program.pdf", p, 
       width = 10, height = 6)



###'######################################################################
###'
###' Tables 5: Relating two variables
###' 
###' (1) Kindergarten enrollment growth rate
###' 
###' (2) Kindergarten program type change
###'
###'

### Generate Kindergarten enrollment growth variable

df_elem_enrollment <- df_elem_served %>%
  dplyr::select(CountyCode, DistrictCode, SchoolCode, AcademicYear, 
                Total_Enroll, N_KDGN) %>%
  filter(AcademicYear != 2016) %>%
  gather(key = Total_or_KDGN, value = Enrollment, Total_Enroll, N_KDGN) %>%
  unite(key, Total_or_KDGN, AcademicYear) %>%
  spread(key = key, value = Enrollment) %>%
  mutate(N_KDGN_diff = N_KDGN_2017 - N_KDGN_2015, 
         N_KDGN_bin = ntile(N_KDGN_diff, n = 4), 
         Total_Enroll_diff = Total_Enroll_2017 - Total_Enroll_2015, 
         Total_Enroll_bin = ntile(Total_Enroll_diff, n = 4)) 

### Generate a dataframe for summarizing Kindergarten program type
df_elem_kinder <- df_elem_served %>%
  dplyr::select(CountyCode, DistrictCode, SchoolCode, AcademicYear, 
                Kindergarten3, Transitional_K3) %>%
  filter(AcademicYear != 2016) %>%
  gather(key = Kinder_or_Trans, value = Program_Type, Kindergarten3, Transitional_K3) %>%
  unite(key, Kinder_or_Trans, AcademicYear) %>%
  spread(key = key, value = Program_Type)


###' Merge the two resulting dataframes &
###' Generate Program change variables
df_temp <- df_elem_kinder %>%
  left_join(df_elem_enrollment, by = c("CountyCode", "DistrictCode", "SchoolCode")) %>% 
  drop_na(contains("Kindergarten3_"), contains("Transitional_K3_")) %>%
  mutate(Kinder_Change = paste0(Kindergarten3_2015, "-", Kindergarten3_2017), 
         TransK_Change = paste0(Transitional_K3_2015, "-", Transitional_K3_2017))

tbl_Kinder_Change <- tabdf(df_temp, Kinder_Change)
tbl_TransK_Change <- tabdf(df_temp, TransK_Change)  

write.csv(tbl_Kinder_Change, "figures/table_Kinder_Change.csv")
write.csv(tbl_TransK_Change, "figures/table_TransK_Change.csv")


### Average "Kindergarten" enrollment growth differences
tbl_Kinder_Change <- df_temp %>%
  group_by(Kinder_Change) %>%
  summarise(N_obs = n_distinct(SchoolCode), 
            min = min(N_KDGN_diff, na.rm = TRUE), 
            Q25 = quantile(N_KDGN_diff, 0.25), 
            mean = mean(N_KDGN_diff, na.rm = TRUE), 
            Q50 = median(N_KDGN_diff, na.rm = TRUE),
            Q75 = quantile(N_KDGN_diff, 0.75), 
            max = max(N_KDGN_diff, na.rm = TRUE))


tbl_TransK_Change <- df_temp %>%
  group_by(TransK_Change) %>%
  summarise(N_obs = n_distinct(SchoolCode), 
            min = min(N_KDGN_diff, na.rm = TRUE), 
            Q25 = quantile(N_KDGN_diff, 0.25), 
            mean = mean(N_KDGN_diff, na.rm = TRUE), 
            Q50 = median(N_KDGN_diff, na.rm = TRUE),
            Q75 = quantile(N_KDGN_diff, 0.75), 
            max = max(N_KDGN_diff, na.rm = TRUE))

write.csv(tbl_Kinder_Change, "figures/table_Kinder_Change_01_Kindergarten Enrollment.csv")
write.csv(tbl_TransK_Change, "figures/table_TransK_Change_01_Kindergarten Enrollment.csv")


### Average "Total" enrollment growth differences
tbl_Kinder_Change <- df_temp %>%
  group_by(Kinder_Change) %>%
  summarise(N_obs = n_distinct(SchoolCode), 
            min = min(Total_Enroll_diff, na.rm = TRUE), 
            Q25 = quantile(Total_Enroll_diff, 0.25), 
            mean = mean(Total_Enroll_diff, na.rm = TRUE), 
            Q50 = median(Total_Enroll_diff, na.rm = TRUE),
            Q75 = quantile(Total_Enroll_diff, 0.75), 
            max = max(Total_Enroll_diff, na.rm = TRUE))


tbl_TransK_Change <- df_temp %>%
  group_by(TransK_Change) %>%
  summarise(N_obs = n_distinct(SchoolCode), 
            min = min(Total_Enroll_diff, na.rm = TRUE), 
            Q25 = quantile(Total_Enroll_diff, 0.25), 
            mean = mean(Total_Enroll_diff, na.rm = TRUE), 
            Q50 = median(Total_Enroll_diff, na.rm = TRUE),
            Q75 = quantile(Total_Enroll_diff, 0.75), 
            max = max(Total_Enroll_diff, na.rm = TRUE))

write.csv(tbl_Kinder_Change, "figures/table_Kinder_Change_02_Total Enrollment.csv")
write.csv(tbl_TransK_Change, "figures/table_TransK_Change_02_Total Enrollment.csv")


