
###'######################################################################
###'
###' Who are the novice teachers in 2015?
###'
###'
###' 20190102 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Staff_Data/Certificated_Staff/Staff_Assignment_and_Course")


### Call libraries
library(tidyverse)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Load the key filters: Traditional_SOC, PCT_FRPM15_bin (Poverty Status)
###' 
###' Save the filter dataframe for later use
###' 
###'

setwd("D:/Data/LCFF/Public_K-12_Character")
load(file = "df_Public_K12_Characters_2003_2017_with_filter.rda")

names(df_to_save)
df_filter <- df_to_save %>%
  dplyr::select(CountyCode:PCT_Black_15bin) %>%
  distinct()

setwd(work_dir)
dualsave(df_filter, file = "processed_data/df_filter_Traditional_SOC_Poverty")



###'######################################################################
###'
###' Loop over years
###'
###'

years <- sprintf("%02d", seq(12, 17))

meta_list <- list()


for (i in seq_along(years)) {
  
  
  ###'######################################################################
  ###'
  ###' Assign year number
  ###' 
  ###' 
  
  year_num <- years[i]

  
  
  ###'######################################################################
  ###'
  ###' Load the 2015 Staff + Course Data
  ###'
  ###'
  
  setwd(data_dir)
  
  load(file = paste0("CoursesTaught", year_num, 
                     "_merged with CourseEnrollment_StaffAssign_StaffDemoFTE.rda"))
  
  df <- df_to_save; rm(df_to_save)
  
  
  
  ###'######################################################################
  ###'
  ###' Merge filter with the analysis data
  ###'
  ###'
  
  names(df)
  
  ### Merge!
  match_key <- c("CountyCode", "DistrictCode", "SchoolCode")
  
  df_merged <- df %>% 
    dplyr::select(-.merge) %>%
    full_join_track(df_filter, by = match_key, .merge = TRUE)
  
  tabdf(df_merged, .merge)
  
  
  ###' Look into the unmerged cases
  ###' Unmerged from the analysis data (left only) => 99.5% of District Offices
  ###' Unmerged from the filter data (right only) => 25.2% of District Offices and nonpublic
  df_right_only <- df_merged %>%
    filter(.merge == "right_only")
  
  tabdf(df_right_only, SchoolCode)
  tabdf(df_right_only, SOC)
  
  df_left_only <- df_merged %>%
    filter(.merge == "left_only")
  
  tabdf(df_left_only, SchoolCode)
  tabdf(df_left_only, SOC)
  
  
  ### Keep only matched cases
  df_matched <- df_merged %>%
    filter(.merge %in% c("matched")) %>%
    dplyr::select(CountyCode, DistrictCode, SchoolCode, 
                  contains("CountyName"), 
                  contains("DistrictName"), 
                  contains("SchoolName"), 
                  DOC:PCT_Black_15bin, 
                  everything()) %>%
    dplyr::select(-contains(".y"))
  
  names(df_matched) <- gsub(".x", "", names(df_matched))
  
  
  
  ###'######################################################################
  ###'
  ###' Filter only Traditional, Non-Charter schools
  ###'
  ###'
  
  tabdf(df_matched, Traditional_SOC)
  tabdf(df_matched, Charter)
  
  ### How many Charters are in each Traditional SOC?
  tbl_SOC_Charter <- df_matched %>%
    group_by(Traditional_SOC, Charter) %>%
    summarise(N = n_distinct(SchoolCode))
  
  
  df_trad <- df_matched %>% 
    filter(Traditional_SOC %in% c("Elementary School", 
                                  "High School", 
                                  "Intermediate/Middle/Junior High")) %>%
    filter(Charter == 0)
  
  
  
  ###'######################################################################
  ###'
  ###' How many novice teachers are in each school?
  ###'
  ###'
  
  tbl_N_novice <- df_trad %>%
    group_by(Traditional_SOC, Teacher_White, NewTeaching, PCT_FRPM_15bin) %>%
    summarise(N = n_distinct(RecID)) %>%
    mutate(AcademicYear = year_num)
  
  meta_list[[i]] <- tbl_N_novice
  
  
  
  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("Year ", year_num, " completed", "\n"))
  
  
  
  ###'######################################################################
  ###' 
  ###' End for loop
  ###' 
  
}



###'######################################################################
###' 
###' Bind rows 
###' 
###' 

df_bind <- bind_rows(meta_list) %>%
  select(Traditional_SOC, Teacher_White, NewTeaching, PCT_FRPM_15bin, AcademicYear, N) %>%
  arrange(Traditional_SOC, Teacher_White, NewTeaching, PCT_FRPM_15bin, AcademicYear)

setwd(work_dir)
write.csv(df_bind, file = "processed_data/Novice_White.csv")


###'######################################################################
###' 
###' Plot!
###' 
###' 

### Generate data to plot
df_plot <- df_bind %>%
  mutate(AcademicYear = as.numeric(AcademicYear)) %>%
  filter(Traditional_SOC == "High School") %>%
  filter(NewTeaching == 0)

df_plot <- df_plot[complete.cases(df_plot), ]


### Plot!
p <- plot_trend_grp_facet(df_plot, 
                          x = AcademicYear, 
                          y = N, 
                          group = PCT_FRPM_15bin, 
                          facet_formula =  . ~ Teacher_White, 
                          yline = 13) + 
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) 
# geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed")









