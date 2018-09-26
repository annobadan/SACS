
###'######################################################################
###'
###' Helper functions for data analysis
###' 
###' 
###' 20180901 JoonHo Lee
###' 
###' 

### Package dependency
library(tidyverse)
library(scales)



###'######################################################################
###'
###' tabdf(): Tabulate frequencies
###' 
###' Similar to the Stata command "tab"
###'
###'

tabdf <- function(df, 
                  variable){
  
  ### Enquote variables
  x <- enquo(variable)
  
  ### Generate table
  tibble_tbl <- df %>%
    group_by(!!x) %>%
    summarise(Freq = n()) %>%
    ungroup() %>%
    mutate(total_n = sum(Freq, na.rm = TRUE),
           Percent = round((Freq/total_n)*100, 1), 
           CumFreq = cumsum(Freq), 
           CumPercent = round((CumFreq/total_n)*100, 1)) %>%
    select(-total_n) 
  
  ### Display table as data.frame format
  data.frame(tibble_tbl)
  
}



###'######################################################################
###'
###' classmode(): Check classes and modes of selected variables
###'
###'

classmode <- function(df, ...){
  
  ### Enquote variables
  vars <- quos(...)  # any rules for select() works. ex) everything(), ends_with(), etc.
  
  ### Select variables
  df_select <- df %>% 
    select(!!!vars)
  
  ### Return classes and modes
  mat <- cbind(sapply(df_select, class), 
               sapply(df_select, mode))
  
  ### Convert to data.frame format
  df_mat <- data.frame(rownames(mat), mat)
  rownames(df_mat) <- NULL
  names(df_mat) <- c("variable", "class", "mode")
  
  return(df_mat)
}



###'######################################################################
###'
###' empty_as_na(): Convert empty strings to NA
###'
###'

empty_as_na <- function(x){
  
  if("factor" %in% class(x)) x <- as.character(x) 
  
  ## since ifelse wont work with factors
  
  ifelse(as.character(x) !="", x, NA)
}



###'######################################################################
###'
###' operation14(): Remove districts with insufficient years of data
###' 
###' => Analyze only traditional schools in elementary, high, and unified 
###'    school districts that have been in continuous operation (14 years) 
###'    in California from 2003 through 2017

setwd(work_dir)
load("~/SACS/processed_data/years_of_operation.rda")   # Data dependency: years_of_operation.csv

operation14 <- function(df){
  df %>%
    left_join(years_of_operation[, !names(years_of_operation) %in% c("Dname", "Dtype")], 
              by = c("Ccode", "Dcode")) %>%
    filter(opr_years == 14)
}



###'######################################################################
###'
###' get_weighted_mean(): Get weighted district averages
###' 
###' 

get_weighted_mean <- function(df, 
                              x = Fiscalyear, 
                              y = sum_value_PP_16, 
                              weight = K12ADA_C, 
                              ...){
  
  ### Enquote variables
  x <- enquo(x)
  y <- enquo(y)
  weight <- enquo(weight)
  group_var <- quos(...)
  
  df %>% 
    group_by(!!x, !!!group_var) %>%
    summarise(mean_value = round(weighted.mean(!!y, !!weight, na.rm = TRUE), 0))
}



###'######################################################################
###'
###' Calculate percentages based on groups
###'
###'

group_percent <- function(df, 
                          value = mean_value, 
                          ...){
  
  # Enquote variables
  value <- enquo(value)
  group <- quos(...)
  
  
  #' (1) Calculate the percentages
  #' (2) Format the labels and calculate their positions
  df %>%
    group_by(!!!group) %>%
    mutate(group_sum = sum(!!value, na.rm = TRUE), 
           percent = !!value/group_sum * 100, 
           # don't need to calculate the label positions from ggplot 2.1.0 
           # position = cumsum(amount) - 0.5 * amount,  
           label_text = paste0(sprintf("%.1f", percent), "%")) -> df
  return(df)
}



###'######################################################################
###'
###' is.numeric.elementwise(): 
###' Check whether each element is numeric
###'
###'

is.numeric.elementwise <- function(vector){
  
  lvector <- c()
  
  for (i in seq_along(vector)){
    
    element <- vector[i]
    
    elem_TF <- is.na(as.numeric(element))
    
    lvector <- c(lvector, elem_TF)
    
  }
}



###'######################################################################
###'
###' Get a dataframe of regression estimates 
###'
###'

get_lm_est_df <- function(lm_fit){
  
  summary <- summary(lm_fit)
  
  df <- data.frame(summary$coefficients)
  
  names(df) <- c("estimate", "std_error", "t_value", "p_value")
  
  df <- round(df, 3)
  
  df$variable <- rownames(df)
  rownames(df) <- NULL
  
  df <- df %>% 
    select(variable, everything())
  
  df
}






