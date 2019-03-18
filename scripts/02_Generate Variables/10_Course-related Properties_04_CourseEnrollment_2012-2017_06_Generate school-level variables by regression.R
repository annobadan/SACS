
###'######################################################################
###'
###' Generate Variables
###' 
###' Course-related Properties: 
###' 
###' => Course + Staff Synergy data
###' 
###' (1) CoursesTaught + CourseEnrollment + StaffAssign + StaffDemoFTE
###' 
###' - Relate EL, White, Latino, Black, Asian CourseEnrollment with
###' - Teacher and course properties
###'  
###' 
###' 20181213 JoonHo Lee
###' 20181228 JoonHo Lee - Update with unified subject name
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
library(foreign)
library(broom)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Prepare Loops over
###' 
###' 1) i = years
###' 2) j = outcome
###' 3) k = predictor
###'
###'

### Loop over years: 2012-2017
years <- sprintf("%02d", seq(12, 17))


## Loop over outcomes
y_vec <- c("PCT_EL",
           "PCT_White",
           "PCT_Hispanic",
           "PCT_AfrAm",
           "PCT_Asian")

y_label_vec <- c("Percent of English Learners within each class",
                 "Percent of White students within each class",
                 "Percent of Hispanic or Latino students within each class",
                 "Percent of African American students within each class",
                 "Percent of Asian students within each class")


### Loop over predictors
x_vec <- c("Teacher_White", 
           "Teacher_Latino", 
           "Teacher_Black", 
           "Teacher_Asian", 
           "Master_Above", 
           "YearsTeaching", 
           "YearsInDistrict", 
           "NewTeaching", 
           "NewInDistrict", 
           "Teacher_Tenured", 
           "UC_CSU_Approved", 
           "NCLB_Core", 
           "AP")

x_label_vec <- c("White teacher", 
                 "Hispanic or Latino teacher", 
                 "African American teacher", 
                 "Asian teacher", 
                 "Teachers with Master's or higher degree", 
                 "Total years of teaching", 
                 "Total years in district", 
                 "Novice teacher", 
                 "New in district", 
                 "Tenured teacher", 
                 "A-G Approved Course", 
                 "Core academic class under NCLB", 
                 "AP Course")


### List by year
list_error <- list()
list_estimates <- list()
list_sumstats <- list()



###'######################################################################
###'
###' Start loops
###'
###'

for (i in seq_along(years)) {
  
  
  ###'######################################################################
  ###'
  ###' Import precleaned dataset
  ###'
  ###'
  
  setwd(data_dir)
  
  year_num <- years[i]
  
  load(file = paste0("CoursesTaught", year_num, 
       "_merged with CourseEnrollment_StaffAssign_StaffDemoFTE.rda"))
  
  df <- df_to_save; rm(df_to_save)
  
  
  
  ###'######################################################################
  ###'
  ###' Generate empty dataframes to collect results
  ###'
  ###'
  
  tbl_error <- tibble()   ### Error tables
  
  df_estimates <- tibble()  ### Estimates
  
  df_sumstats <- tibble()  ### Quick summary stats of estimates
  

  for (j in seq_along(y_vec)){
    
    for (k in seq_along(x_vec)){
      
      
      ###'######################################################################
      ###'
      ###' Assign outcome and predictor variables
      ###'  
      ###' 
      
      y <- y_vec[j]
      
      x <- x_vec[k]
      
      
      
      ###'######################################################################
      ###'
      ###' Rename x and y in the copied dataset
      ###'
      ###'
      
      df_copy <- df
      
      names(df_copy)[names(df_copy) == y] <- "y"
      
      names(df_copy)[names(df_copy) == x] <- "x"
      
      names(df_copy)

      

      ###'######################################################################
      ###' 
      ###' Nest data by County, District, School, and (Unified) Subject Category
      ###' 
      ###' 
      
      df_nested <- df_copy %>%
        group_by(CountyCode, DistrictCode, SchoolCode, Subject_Category) %>%
        nest()
      
      head(df_nested)
      
      # df_example <- df_nested$data[[1]]
      
      
      
      ###'######################################################################
      ###' 
      ###' Map ordinary linear regression model for each group
      ###' 
      ###' 
      
      safe_lm <- safely(lm)   # define a wrapped function for lm()
      
      fitted_models <- df_nested %>%
        mutate(model = map(.x = df_nested$data, 
                           .f = ~safe_lm(formula = y ~ x, 
                                         data = .x)), 
               model_result = map(model, "result"), 
               model_error = map(model, "error"), 
               idx_error = map(model_error, is.null), 
               idx_error = !simplify(idx_error))
      
      
      
      ###'######################################################################
      ###' 
      ###' Collect error information 
      ###' 
      ###' 

      tbl <- fitted_models %>%
        group_by(Subject_Category, idx_error) %>%
        summarise(Freq = n()) %>%
        mutate(total_n = sum(Freq, na.rm = TRUE),
               Percent = round((Freq/total_n)*100, 1), 
               CumFreq = cumsum(Freq), 
               CumPercent = round((CumFreq/total_n)*100, 1)) %>%
        dplyr::select(-total_n) %>%
        mutate(year = year_num, x = x, y = y) %>%
        dplyr::select(year, x, y, everything())
      
      tbl_error <- bind_rows(tbl_error, tbl)
      
      
      
      ###'######################################################################
      ###' 
      ###' Extract the coefficients of each model 
      ###' Transform into dataframe    
      ###'     
      ###'         
      
      df_result <- fitted_models %>%
        mutate(coef = map(model_result, ~ tidy(.x))) %>%
        unnest(coef)
      
      
      ### Extract only necessary information
      df_result_sub <- df_result %>%
        filter(term == "x") %>%
        dplyr::select(-idx_error, -term) %>%
        mutate(year = year_num, x = x, y = y) %>%
        dplyr::select(year, x, y, everything())
      
      
      ### Collect the resulting dataframe
      df_estimates <- bind_rows(df_estimates, df_result_sub)
      
      
      
      ###'######################################################################
      ###'
      ###' Save quick summary
      ###'
      ###'
      
      sumstats <- df_result_sub %>%
        group_by(Subject_Category) %>%
        summarise(Min = min(estimate), 
                  Q10 = quantile(estimate, probs = 0.10), 
                  Q30 = quantile(estimate, probs = 0.30), 
                  Q50 = quantile(estimate, probs = 0.50), 
                  Mean = mean(estimate), 
                  Q70 = quantile(estimate, probs = 0.70), 
                  Q90 = quantile(estimate, probs = 0.90)) %>%
        mutate(x = x, y = y) %>%
        dplyr::select(x, y, everything())
      
      df_sumstats <- bind_rows(df_sumstats, sumstats)
      
      
      
      ###'######################################################################
      ###' 
      ###' Print the progress  
      ###' 
      
      cat(paste("Year", year_num, y, x, "completed", "\n", sep = " || "))
      
      
      
      ###'######################################################################
      ###' 
      ###' End for loops
      ###' 
      
    }  ### End of loop over x_vec
  }    ### End of loop over y_vec
  
  
  ###'######################################################################
  ###'
  ###' Collect into a list by year
  ###'
  ###'

  list_error[[i]] <- tbl_error  ### Error tables
  list_estimates[[i]] <- df_estimates  ### Estimates
  list_sumstats[[i]] <- df_sumstats  ### Quick summary stats of estimates
  
}  ### End of loop over years



###'######################################################################
###'
###' Save the resulting lists for later use
###'
###'

# ### Temporary saving after completion
# setwd(data_dir)
# save.image(file = "temp_saving2_Unified_Subject_Category.RData")
# 
# 
# ### Load the saved image
# setwd(data_dir)
# load(file = "temp_saving2_Unified_Subject_Category.RData")



###'######################################################################
###'
###' Error information
###'
###'

df_error <- bind_rows(list_error) 

setwd(data_dir)
write.csv(df_error, file = "Within-school teacher sorting regression_01_Error Info.csv")



###'######################################################################
###'
###' Summary Statistics
###'
###'

years <- sprintf("%02d", seq(12, 17))

meta_list_sumstats <- list()

for (l in seq_along(years)){
  
  df_temp <- list_sumstats[[l]] %>%
    mutate(AcademicYear = years[l]) %>%
    dplyr::select(x, y, Subject_Category, AcademicYear, everything())
  
  meta_list_sumstats[[l]] <- df_temp
  
}

df_sumstats <- bind_rows(meta_list_sumstats) %>%
  arrange(x, y, Subject_Category, AcademicYear) 


### Save the resulting dataframe
setwd(data_dir)
dualsave(df_sumstats, "Within-School teacher sorting regression_02_Summary Stats")



###'######################################################################
###'
###' School-level estimates
###'
###'

### Bind rows: Generate long long dataset
df_est_long <- bind_rows(list_estimates) %>%
  arrange(x, y, CountyCode, DistrictCode, SchoolCode, Subject_Category, year) %>%
  dplyr::select(CountyCode, DistrictCode, SchoolCode, x, y, Subject_Category, 
                year, estimate, std.error, statistic, p.value)

setwd(data_dir)
dualsave(df_est_long, "Within-School teacher sorting regression_03_Estimates_Long")


### Generate a brief key combining x, y, and Subject_Category
df_temp <- list_estimates[[1]]

tabdf(df_est_long, x)
tabdf(df_temp, y)
tabdf(df_temp, Subject_Category)

x_lookup <- setNames(c("WhiteT", "LatinoT", "BlackT", "AsianT", "Master", 
                       "YrsT", "YrsInD", "Tenure", "AGApprvd", "NCLB", "AP"), 
                     unique(df_temp$x))

y_lookup <- setNames(c("EL", "WhiteS", "LatinoS", "BlackS", "AsianS"), 
                     unique(df_temp$y))

subj_lookup <- c("Administration" = "Admin", 
                 "Art, Music, Dance, Drama" = "Art", 
                 "Career Technical Education/Vocational" = "CTE", 
                 "Computer Education" = "Computer", 
                 "English Language Arts" = "ELA", 
                 "Foreign Languages" = "FoLan",
                 "History/Social Science" = "Social", 
                 "Mathematics" = "Math", 
                 "Other Instruction-Related Assignments" = "Other", 
                 "PE/Health" = "PE", 
                 "Science" = "Science", 
                 "Self-Contained Class" = "SelfCon",
                 "Special Designated Subjects" = "SpeDesg", 
                 "Special Education" = "SPED") 


### Convert to wide format
df_est_long_copy <- df_est_long

df_est_long_copy$x <- unname(x_lookup[df_est_long_copy$x])
df_est_long_copy$y <- unname(y_lookup[df_est_long_copy$y])
df_est_long_copy$Subject_Category <- unname(subj_lookup[df_est_long_copy$Subject_Category])

df_est_wide <- df_est_long_copy %>%
  dplyr::select(-std.error, -statistic, -p.value) %>%
  rename(AcademicYear = year) %>%
  unite(variable, x, y, Subject_Category) %>%
  spread(key = variable, value = estimate, fill = NA) %>%
  arrange(CountyCode, DistrictCode, SchoolCode, AcademicYear)


### Reorder columns
names(df_est_wide)

df_est_wide <- df_est_wide %>%
  select(CountyCode, DistrictCode, SchoolCode, AcademicYear, 
         contains("_EL_"),
         contains("_WhiteS_"), 
         contains("_LatinoS_"), 
         contains("_BlackS_"), 
         contains("_AsianS_"))


### Save the resulting wide dataset
setwd(data_dir)
dualsave(df_est_wide, "Within-School teacher sorting regression_04_Estimates_Wide")







