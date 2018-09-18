
###'######################################################################
###'
###' Data Visualization
###' 
###' (1) The State of California's overall and local revenues/expenditures data
###' 
###' 
###' 20180824 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Financial/Annual Financial Data")


### Call libraries
library(tidyverse)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import the prepared dataset
###'
###'

df <- read.csv(file = "processed_data/May_Revision_Budget_2000_to_2016.csv")



###'######################################################################
###'
###' Labels for plotting
###'
###'

labels <- labs(title = NULL,
               subtitle = "Inflation adjusted using the CPI-U deflator (in real 2016 dollars)", 
               caption = "Source: The Statement of General Fund Cash Receipts and Disbursements", 
               y = "Dollar amounts in thousands",  
               x = "Fiscal Year") 



###'######################################################################
###'
###' Cash Receipts: (1) Trends
###'
###'

### Data
df_plot <- df %>%
  filter(Category_1 %in% c("Revenues", "Nonrevenues")) %>%
  group_by(Fiscalyear, Category_1) %>% 
  summarise(mean_value = round(mean(value_16, na.rm = TRUE), 0))


### Plot
p <- plot_trend_grp(df_plot, Fiscalyear, mean_value, Category_1, yline = 2013, 
                     ylim = auto_ylim(df_plot$mean_value)) + labels

p <- p + labs(title = "General Fund Cash Receipts")

ggsave("figures/Cash_Receipts_Trends.pdf", p, width = 12, height = 8)



###'######################################################################
###'
###' Cash Receipts: (2) Proportions: Revenues
###'
###'

### Data 
df_plot <- df %>%
  filter(Category_1 == "Revenues") %>%
  group_by(Fiscalyear, Category_2) %>%
  summarise(mean_value = round(mean(value_16, na.rm = TRUE))) 


df_plot <- df_plot[!is.nan(df_plot$mean_value), ]

df_plot <- group_percent(df_plot, mean_value, Fiscalyear)

group_total <- df_plot %>% 
  group_by(Fiscalyear) %>% 
  summarise(group_sum = first(group_sum))


### Plot
p <- ggplot(df_plot, aes(x = Fiscalyear, y = mean_value, fill = Category_2)) +
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity", width = 0.7) +
  geom_text(aes(label = label_text), position = position_stack(vjust = 0.5, reverse = TRUE), size = 3) + 
  geom_text(data = group_total,  
            aes(x = Fiscalyear, y = group_sum + mean(df_plot$group_sum)/30, 
                label = comma(group_sum), fill = NULL), 
            size = 3) + 
  theme_trend + 
  scale_x_continuous(breaks = seq(min(df_plot$Fiscalyear), 
                                  max(df_plot$Fiscalyear), 
                                  by = 1)) + 
  scale_y_continuous(labels = comma) + 
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) + 
  scale_fill_brewer(palette = "Paired") +
  labels

p <- p + labs(title = "General Fund Cash Receipts: Revenues")

ggsave("figures/Cash_Receipts_Proportions_01_Revenues.pdf", p, width = 14, height = 11)



###'######################################################################
###'
###' Cash Receipts: (3) Proportions: Nonrevenues
###'
###'

### Data 
df_plot <- df %>%
  filter(Category_1 == "Nonrevenues") %>%
  group_by(Fiscalyear, Category_2) %>%
  summarise(mean_value = round(mean(value_16, na.rm = TRUE))) 


df_plot <- df_plot[!is.nan(df_plot$mean_value), ]

df_plot <- group_percent(df_plot, mean_value, Fiscalyear)

group_total <- df_plot %>% 
  group_by(Fiscalyear) %>% 
  summarise(group_sum = first(group_sum))


### Plot
p <- ggplot(df_plot, aes(x = Fiscalyear, y = mean_value, fill = Category_2)) +
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity", width = 0.7) +
  geom_text(aes(label = label_text), position = position_stack(vjust = 0.5, reverse = TRUE), size = 3) + 
  geom_text(data = group_total,  
            aes(x = Fiscalyear, y = group_sum + mean(df_plot$group_sum)/30, 
                label = comma(group_sum), fill = NULL), 
            size = 3) + 
  theme_trend + 
  scale_x_continuous(breaks = seq(min(df_plot$Fiscalyear), 
                                  max(df_plot$Fiscalyear), 
                                  by = 1)) + 
  scale_y_continuous(labels = comma) + 
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) + 
  scale_fill_brewer(palette = "Paired") + 
  labels

p <- p + labs(title = "General Fund Cash Receipts: Nonrevenues")

ggsave("figures/Cash_Receipts_Proportions_02_Nonevenues.pdf", p, width = 14, height = 11)



###'######################################################################
###'
###' Cash Disbursements: (1) Trends - Total Disbursements
###'
###'

### Data
df_plot <- df %>%
  filter(Category_1 %in% c("State Operations", "Local Assistance")) %>%
  group_by(Fiscalyear, Category_1) %>% 
  summarise(mean_value = round(mean(value_16, na.rm = TRUE), 0))


### Plot
p <- plot_trend_grp(df_plot, Fiscalyear, mean_value, Category_1, yline = 2013, 
                    ylim = auto_ylim(df_plot$mean_value)) + labels

p <- p + labs(title = "General Fund Cash Disbursements")

ggsave("figures/Cash_Disbursements_Trends_01_Total Disbursements.pdf", p, width = 12, height = 8)



###'######################################################################
###'
###' Cash Disbursements: (2) Trends - State Operations - Education
###'
###'

### Data
df_plot <- df %>%
  filter(Category_2 %in% c("Education")) %>%
  group_by(Fiscalyear, Category_3) %>% 
  summarise(mean_value = round(mean(value_16, na.rm = TRUE), 0))


### Plot
p <- plot_trend_grp(df_plot, Fiscalyear, mean_value, Category_3, yline = 2013, 
                    ylim = auto_ylim(df_plot$mean_value)) + labels

p <- p + labs(title = "General Fund Cash Disbursements: State Operations - Education")

ggsave("figures/Cash_Disbursements_Trends_02_State Operations_Education.pdf", 
       p, width = 12, height = 8)



###'######################################################################
###'
###' Cash Disbursements: (3) Trends - Local Assistance - Education vs. Non-Education
###'
###'

### Data
df_plot <- df %>%
  filter(Category_1 %in% c("Local Assistance")) %>%
  mutate(Local_Edu = if_else(Category_2 %in% c("Public Schools - K-12", 
                                               "Community Colleges", 
                                               "Contributions to State Teachers' Retirement System", 
                                               "School Facilities Aid", 
                                               "Other Education"), 
                             "Local Assistance - Education", 
                             "Local Assistance - Non-Education")) %>%
  group_by(Fiscalyear, Local_Edu) %>% 
  summarise(mean_value = round(mean(value_16, na.rm = TRUE), 0))


### Plot
p <- plot_trend_grp(df_plot, Fiscalyear, mean_value, Local_Edu, yline = 2013, 
                    ylim = auto_ylim(df_plot$mean_value)) + labels

p <- p + labs(title = "General Fund Cash Disbursements: Local Assistance - Education vs. Non-Education")

ggsave("figures/Cash_Disbursements_Trends_03_Local Assistance_Education vs Non-Education.pdf", 
       p, width = 12, height = 8)



###'######################################################################
###'
###' Cash Disbursements: (4) Trends - Local Assistance - Education
###'
###'

### Data
df_plot <- df %>%
  filter(Category_1 %in% c("Local Assistance")) %>%
  mutate(Local_Edu = if_else(Category_2 %in% c("Public Schools - K-12", 
                                               "Community Colleges", 
                                               "Contributions to State Teachers' Retirement System", 
                                               "School Facilities Aid", 
                                               "Other Education"), 
                             "Local Assistance - Education", 
                             "Local Assistance - Non-Education")) %>%
  filter(Local_Edu == "Local Assistance - Education") %>%
  group_by(Fiscalyear, Category_2) %>% 
  summarise(mean_value = round(mean(value_16, na.rm = TRUE), 0))


### Plot 1
p <- plot_trend_grp(df_plot, Fiscalyear, mean_value, Category_2, yline = 2013, 
                    ylim = auto_ylim(df_plot$mean_value)) + labels

p <- p + labs(title = "General Fund Cash Disbursements: Local Assistance - Education")

ggsave("figures/Cash_Disbursements_Trends_04_Local Assistance_Education.pdf", 
       p, width = 12, height = 8)


### Plot 2: Only Contributions to State Teachers' Retirement System

df_plot_sub <- df_plot %>%
  filter(Category_2 == "Contributions to State Teachers' Retirement System") %>%
  mutate(mean_value_million = round(mean_value/1000, 0))


p <- plot_trend_xy(df_plot_sub, Fiscalyear, mean_value_million, yline = 2013, 
                    ylim = auto_ylim(df_plot_sub$mean_value_million)) + labels

p <- p + labs(title = "General Fund Cash Disbursements: Contributions to State Teachers' Retirement System", 
              subtitle = "Local Assistance - Education", 
              y = "Dollar amounts in millions")

ggsave("figures/Cash_Disbursements_Trends_05_Local Assistance_Contributions to STAR.pdf", 
       p, width = 9, height = 6)






###'######################################################################
###'
###' Cash Disbursements: (3) Proportions: State Operations
###'
###'

### Data 
df_plot <- df %>%
  filter(Category_1 == "State Operations") %>%
  group_by(Fiscalyear, Category_2) %>%
  summarise(mean_value = round(mean(value_16, na.rm = TRUE))) 


df_plot <- df_plot[!is.nan(df_plot$mean_value), ]

df_plot <- group_percent(df_plot, mean_value, Fiscalyear)

group_total <- df_plot %>% 
  group_by(Fiscalyear) %>% 
  summarise(group_sum = first(group_sum))


### Plot
p <- ggplot(df_plot, aes(x = Fiscalyear, y = mean_value, fill = Category_2)) +
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity", width = 0.7) +
  geom_text(aes(label = label_text), position = position_stack(vjust = 0.5, reverse = TRUE), size = 3) + 
  geom_text(data = group_total,  
            aes(x = Fiscalyear, y = group_sum + mean(df_plot$group_sum)/30, 
                label = comma(group_sum), fill = NULL), 
            size = 3) + 
  theme_trend + 
  scale_x_continuous(breaks = seq(min(df_plot$Fiscalyear), 
                                  max(df_plot$Fiscalyear), 
                                  by = 1)) + 
  scale_y_continuous(labels = comma) + 
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) + 
  # scale_fill_brewer(palette = "Paired") + 
  labels

p <- p + labs(title = "General Fund Cash Disbursements: State Operations")

ggsave("figures/Cash_Disbursements_Proportions_01_State Operations.pdf", p, width = 14, height = 11)



###'######################################################################
###'
###' Cash Disbursements: (4) Proportions: Local Assistance
###'
###'

### Data 
df_plot <- df %>%
  filter(Category_1 == "Local Assistance") %>%
  group_by(Fiscalyear, Category_2) %>%
  summarise(mean_value = round(mean(value_16, na.rm = TRUE))) 


df_plot <- df_plot[!is.nan(df_plot$mean_value), ]

df_plot <- group_percent(df_plot, mean_value, Fiscalyear)

group_total <- df_plot %>% 
  group_by(Fiscalyear) %>% 
  summarise(group_sum = first(group_sum))


### Plot
p <- ggplot(df_plot, aes(x = Fiscalyear, y = mean_value, fill = Category_2)) +
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity", width = 0.7) +
  geom_text(aes(label = label_text), position = position_stack(vjust = 0.5, reverse = TRUE), size = 3) + 
  geom_text(data = group_total,  
            aes(x = Fiscalyear, y = group_sum + mean(df_plot$group_sum)/30, 
                label = comma(group_sum), fill = NULL), 
            size = 3) + 
  theme_trend + 
  scale_x_continuous(breaks = seq(min(df_plot$Fiscalyear), 
                                  max(df_plot$Fiscalyear), 
                                  by = 1)) + 
  scale_y_continuous(labels = comma) + 
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) + 
  # scale_fill_brewer(palette = "Paired") +
  labels

p <- p + labs(title = "General Fund Cash Disbursements: Local Assistance")

ggsave("figures/Cash_Disbursements_Proportions_02_Local Assistance.pdf", p, width = 14, height = 11)



###'######################################################################
###'
###' Cash Disbursements: (5) Proportions: State Operations: Education
###'
###'

### Data 
df_plot <- df %>%
  filter(Category_2 == "Education") %>%
  group_by(Fiscalyear, Category_3) %>%
  summarise(mean_value = round(mean(value_16, na.rm = TRUE))) 


df_plot <- df_plot[!is.nan(df_plot$mean_value), ]

df_plot <- group_percent(df_plot, mean_value, Fiscalyear)

group_total <- df_plot %>% 
  group_by(Fiscalyear) %>% 
  summarise(group_sum = first(group_sum))


### Plot
p <- ggplot(df_plot, aes(x = Fiscalyear, y = mean_value, fill = Category_3)) +
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity", width = 0.7) +
  geom_text(aes(label = label_text), position = position_stack(vjust = 0.5, reverse = TRUE), size = 3) + 
  geom_text(data = group_total,  
            aes(x = Fiscalyear, y = group_sum + mean(df_plot$group_sum)/30, 
                label = comma(group_sum), fill = NULL), 
            size = 3) + 
  theme_trend + 
  scale_x_continuous(breaks = seq(min(df_plot$Fiscalyear), 
                                  max(df_plot$Fiscalyear), 
                                  by = 1)) + 
  scale_y_continuous(labels = comma) + 
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) + 
  scale_fill_brewer(palette = "Paired") +
  labels

p <- p + labs(title = "General Fund Cash Disbursements: State Operations - Education")

ggsave("figures/Cash_Disbursements_Proportions_03_State Operations_Education.pdf", 
       p, width = 14, height = 11)



###'######################################################################
###'
###' Cash Disbursements: (6) Proportions: Local Assistance: Education
###'
###'

### Data 
df_plot <- df %>%
  filter(Category_1 %in% c("Local Assistance")) %>%
  mutate(Local_Edu = if_else(Category_2 %in% c("Public Schools - K-12", 
                                               "Community Colleges", 
                                               "Contributions to State Teachers' Retirement System", 
                                               "School Facilities Aid", 
                                               "Other Education"), 
                             "Local Assistance - Education", 
                             "Local Assistance - Non-Education")) %>%
  filter(Local_Edu == "Local Assistance - Education") %>%
  group_by(Fiscalyear, Category_2) %>%
  summarise(mean_value = round(mean(value_16, na.rm = TRUE))) 


df_plot <- df_plot[!is.nan(df_plot$mean_value), ]

df_plot <- group_percent(df_plot, mean_value, Fiscalyear)

group_total <- df_plot %>% 
  group_by(Fiscalyear) %>% 
  summarise(group_sum = first(group_sum))


### Plot
p <- ggplot(df_plot, aes(x = Fiscalyear, y = mean_value, fill = Category_2)) +
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity", width = 0.7) +
  geom_text(aes(label = label_text), position = position_stack(vjust = 0.5, reverse = TRUE), size = 3) + 
  geom_text(data = group_total,  
            aes(x = Fiscalyear, y = group_sum + mean(df_plot$group_sum)/30, 
                label = comma(group_sum), fill = NULL), 
            size = 3) + 
  theme_trend + 
  scale_x_continuous(breaks = seq(min(df_plot$Fiscalyear), 
                                  max(df_plot$Fiscalyear), 
                                  by = 1)) + 
  scale_y_continuous(labels = comma) + 
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) + 
  scale_fill_brewer(palette = "Paired") +
  labels

p <- p + labs(title = "General Fund Cash Disbursements: Local Assistance - Education")

ggsave("figures/Cash_Disbursements_Proportions_04_Local Assistance_Education.pdf", 
       p, width = 14, height = 11)
