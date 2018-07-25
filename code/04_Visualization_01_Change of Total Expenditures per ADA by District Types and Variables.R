
###'######################################################################
###'
###' Data Visualization
###' 
###' (1) Change of Total Expenditure Per Average Daily Attendence
###' 
###' 
###' 20180724 JoonHo Lee
###' 
###' 

###'######################################################################
###'
###' Basic settings
###'
###'

### Remove previous workspace
rm(list=ls())


### Set working directory 
work_dir <- c("~/SACS")
setwd(work_dir)


### Set data containing working directory
data_dir <- c("D:/Data/LCFF/Financial/Annual Financial Data")


### Call libraries
library(readxl)
library(foreign)
library(haven)
library(dplyr)
library(ggplot2)
library(scales)



###'######################################################################
###'
###' Import the cleaned datasets
###' 
###'   

load("data/Total_Expenditures_PerADA_allyears.rda")

df <- df_exp_perADA_allyears  # assign brief name



###'######################################################################
###'
###' Remove districts with insufficient years of data
###' 
###' => Analyze only traditional schools in elementary, high, and unified 
###'    school districts that have been in continuous operation in California
###'    from 2003 through 2017
###'
###'

### Generate the table counting the years of operation
years_of_operation <- df %>% 
  group_by(Ccode, Dcode) %>%
  summarise(Dname = first(Dname), 
            Dtype = first(Dtype), 
            first = first(Fiscalyear), 
            last = last(Fiscalyear), 
            opr_years = n())

write.csv(years_of_operation, file = "table/years_of_operation.csv")


### Merge with the original data and filter only 14 years of operation
df_opr14 <- df %>%
  left_join(select(years_of_operation, -Dname, -Dtype), by = c("Ccode", "Dcode")) %>%
  filter(opr_years == 14)



###'######################################################################
###'
###' Plot #1-1. 
###' 
###' - Change of Total Expenditures per ADA
###' - Overall averages  
###' 
###'

### Generate the data table for plotting
df_plot <- df_opr14 %>% 
  group_by(Fiscalyear) %>%
  summarise(mean_value = round(mean(D1_TotalExp16_PP, na.rm = TRUE), 0))


### Plot

p <- ggplot(data = df_plot, aes(x = Fiscalyear, y = mean_value)) + 
  geom_point(size = 3.0) +  # point
  geom_path(size = 1.0) +  # path  
  geom_text(aes(label = comma(mean_value)), hjust = 0.5, vjust = 1.5) + # value label
  geom_vline(aes(xintercept = 2013), color = "red", linetype = "dashed") +
   
  # Settings
  theme_bw() + 
  theme(panel.background = element_blank(),
        panel.grid = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank()) + 
  
  # Labels
  scale_x_continuous(breaks = pretty_breaks(n = 14)) +
  scale_y_continuous(labels = comma, limits = c(10000, 20000)) +  
  labs(title = "Average total expenditures per student (Definition 1)",  
       y = "Average total expenditures per student (in real 2016 dollars)",  
       x = "Fiscal Year")

filename <- c("Total Expenditure perADA_in 2016 dollar_Definition1.pdf")
ggsave(paste0("figure/", filename, ".pdf"), p, width = 9, height = 5)















