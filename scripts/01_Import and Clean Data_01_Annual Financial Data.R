
###'######################################################################
###'
###' Import Annual Financial Data
###' 
###' Import Raw MS Access (.mdb format) files
###' 
###' (Converted & Downloaded as .txt file due to 64bit OS issue)
###' 
###' 20180702 JoonHo Lee
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



###'######################################################################
###'
###' Define helper functions to clean character vectors
###'
###'

### Convert empty strings to NA

empty_as_na <- function(x){
  
  if("factor" %in% class(x)) x <- as.character(x) 
  
  ## since ifelse won't work with factors
  
  ifelse(as.character(x) !="", x, NA)
}


### Remove and trim unnecessary white space 
### between, left, and rightside of words

rm_extra_ws <- function(x){
  
  if("factor" %in% class(x)) x <- as.character(x) 
  
  trimws(gsub("\\s+", " ", x))
}




###'######################################################################
###'
###' Import data from 2003 to 2016
###'
###'


### Prepare loop over years
years <- paste0(sprintf("%02d",seq(3, 16)), sprintf("%02d",seq(4, 17)))


for (i in seq_along(years)){
  
  
  ###'######################################################################
  ###'
  ###' Assign fiscal year
  ###'
  
  year <- years[i]
  
  
  ###'######################################################################
  ###'
  ###' Vectors for 1) Variable names, and 2) File names
  ###'
  ###'
  
  ### Import CSV file with only character format: Variable names
  
  setwd(data_dir)
  
  filepath <- paste0("Variable_Names/variable_names_", year, ".csv")
  
  var_names <- read.csv(file = filepath, colClasses = "character")
  
  
  ### File names
  
  files <- names(var_names)
  
  
  
  ###'######################################################################
  ###'
  ###' Set data containing working directory
  ###'
  ###'
  
  sacs_folder <- paste0(data_dir, "/sacs", year)
  
  setwd(sacs_folder)
  
  
  
  ###'######################################################################
  ###'
  ###' Import & Clean data
  ###'
  ###'
  
  for (j in seq_along(files)){
    
    ### Import text file as character strings
    df <- read.table(file = paste0(files[j], ".txt"), 
                     header = FALSE, sep = ",", 
                     colClasses = "character")
    
    ### Convert empty strings to NA
    df <- df %>% mutate_all(funs(empty_as_na)) 
    
    ### Remove extra white spaces between words
    df <- df %>% mutate_all(funs(rm_extra_ws))
    
    ### Attach variable names
    vec <- var_names[, j]
    vec <- vec[vec != ""]
    if (length(names(df)) == length(vec)) {
      names(df) <- vec
    } else {
      names(df) <- vec[1:length(names(df))]
    }
    
    
    ### Assign the name of imported data frame
    assign(files[j], df)
  }
  
  
  
  ###'######################################################################
  ###'
  ###' Merge Lookup Description Tables
  ###'
  ###'
  
  ### Collect data and variable names for looping
  data_list <- list(UserGL, UserGL_Totals)
  lookup_list <- list(Fund, Resource, Goal, Function, Object)
  
  data_name <- c("UserGL", "UserGL_Totals")
  lookup_name <- c("Fund", "Resource", "Goal", "Function", "Object")
  
  
  ### Merge lookup tables
  for (k in seq(length(data_name))){
    
    ### Merge LEAs first
    df_merged <- left_join(data_list[[k]], LEAs, by = c("Ccode", "Dcode"))
    
    ### Merge other 5 lookup tables
    for (l in seq(length(lookup_name))){
      
      names(lookup_list[[l]]) <- c(lookup_name[l], paste0(lookup_name[l], "_Desc"))
      
      df_merged <- left_join(df_merged, lookup_list[[l]], 
                             by = lookup_name[l])
    }
    
    ### Assign a name to merged dataset 
    assign(paste0(data_name[k], "_merged"), df_merged)
  }
  
  
  
  ###'######################################################################
  ###'
  ###' Order variables for merged datasets
  ###'
  ###'
  
  UserGL_merged <- UserGL_merged %>% 
    select(Fiscalyear, Ccode, Dcode, Dname, Dtype, SchoolCode, 
           Period:Account, starts_with("Fund"), starts_with("Resource"), Projectyear,
           starts_with("Goal"), starts_with("Function"), starts_with("Object"), 
           Value, everything())
  
  UserGL_Totals_merged <- UserGL_Totals_merged %>% 
    select(Fiscalyear, Ccode, Dcode, Dname, Dtype, 
           Period:Account, starts_with("Fund"), starts_with("Resource"), Projectyear,
           starts_with("Goal"), starts_with("Function"), starts_with("Object"), 
           Value, everything())
  
  
  
  ###'######################################################################
  ###'
  ###' Convert variable types to numeric
  ###'
  ###'
  
  ### UserGL
  
  to_numeric <- c("Fiscalyear", "Ccode", "Dcode", "SchoolCode", "Value", 
                  grep("ADA", names(UserGL_merged), value = TRUE))
  UserGL_merged[, to_numeric] <- sapply(UserGL_merged[, to_numeric], as.numeric)
  data.frame(sapply(UserGL_merged, class))
  
  
  ### UserGL_Totals
  
  to_numeric <- c("Fiscalyear", "Ccode", "Dcode", "Value", 
                  grep("ADA", names(UserGL_Totals_merged), value = TRUE))
  UserGL_Totals_merged[, to_numeric] <- sapply(UserGL_Totals_merged[, to_numeric], as.numeric)
  data.frame(sapply(UserGL_Totals_merged, class))
  
  
  
  ###'######################################################################
  ###' 
  ###' Save data objects
  ###' 
  ###' 
  
  ### Set data containing working directory
  setwd(sacs_folder)
  
  
  ### Save Cleaned UserGL and UserGL_Totals
  save(UserGL_merged, file = "UserGL_merged.rda")
  save(UserGL_Totals_merged, file = "UserGL_Totals_merged.rda")
  
  
  ### Save lookup tables altogether
  save(lookup_list, file = "Lookup_Description_Tables.RData")
  
  
  
  ###'######################################################################
  ###'
  ###' End of for loop
  ###'
  ###'

}



