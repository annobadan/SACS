
###'######################################################################
###'
###' Import and clean data files 
###' 
###' - Certificated Salaries & Benefits
###' 
###'   Salary and Benefits Schedule for the Certificated Bargaining Unit (Form J-90)
###'   
###' 
###' 20181027 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF/Financial/Certificated Salaries & Benefits")


### Call libraries
library(tidyverse)
library(readxl)
library(Hmisc)
library(ldat)
library(foreign)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Extract variable names from 0405 for 0001, 0102, 0203, 0304
###'
###'

### Set data containing working directory
folder <- paste0(data_dir, "/", "0405")
setwd(folder)


### Import data files
df1 <- read.csv(file = paste0("TS1_fixdinfo", ".txt"), header = TRUE)
df2 <- read.csv(file = paste0("TS2_colmhdgs", ".txt"), header = TRUE)
df3 <- read.csv(file = paste0("TS3_salxstep", ".txt"), header = TRUE)
df4 <- read.csv(file = paste0("TS4_beneinfo", ".txt"), header = TRUE)


### Extract variable names
names_df1 <- names(df1)
names_df2 <- names(df2)
names_df3 <- names(df3)
names_df4 <- names(df4)



###'######################################################################
###'
###' Loop over years: 9899-1617 
###' 
###' 

years <- c(sprintf("%02d", seq(0, 16))) 

for (i in seq_along(years)){
  
  
  ###'######################################################################
  ###'
  ###' Assign year number 
  ###' 
  ###' 
  
  year_num <- years[i]
  
  year_pasted <- paste0(year_num, sprintf("%02d", as.numeric(year_num) + 1)) 
  
  
  
  ###'######################################################################
  ###'
  ###' Set data containing working directory
  ###'
  ###'
  
  folder <- paste0(data_dir, "/", year_pasted)
  
  setwd(folder)
  
  
  
  ###'######################################################################
  ###'
  ###' (1) FIXDINFO: TS1
  ###'     
  ###' Contains information for sections I, III, and IV of the J-90.
  ###' 
  ###'         
  
  ### Define header logical values
  headerl <- if_else(as.numeric(year_num) <= 3, FALSE, TRUE)
  
  
  ### Import data files
  df1 <- read.csv(file = paste0("TS1_fixdinfo", ".txt"), 
                  header = headerl, as.is = TRUE)
  
  classmode(df1, everything())

  
  ### Assign variable names
  dim(df1)
  length(names_df1)
  names_df1

  if (year_num %in% sprintf("%02d", c(0:1))){
    
    varsl <- names_df1 %in% c("TS1_NOTE", "TS1_CONF", 
                              "TS1_MAXCAF", "TS1_MAXSIN", "TS1_MAXTWO", 
                              "TS1_MAXFAM", "INCLUDED") 
    
    names(df1) <- names_df1[!varsl]

  } else if (year_num %in% sprintf("%02d", c(2:3))){
    
    varsl <- names_df1 %in% c("TS1_NOTE", "TS1_CONF") 
    
    names(df1) <- names_df1[!varsl]
    
  }

  
  ### Convert empty strings to NA
  df1 <- df1 %>% mutate_all(funs(empty_as_na)) 
  
  
  ### Rename indicators
  if (year_num %in% sprintf("%02d", c(0:10))){
    
    names(df1)[names(df1) %in% c("COUNTY", "CCODE")] <- c("CountyCode")
    names(df1)[names(df1) %in% c("CDS", "DISTRICT")] <- c("DistrictCode")
    names(df1)[names(df1) %in% c("TS1_DNAME")] <- c("DistrictName")
    names(df1)[names(df1) %in% c("TS1_COUNTY")] <- c("CountyName")
  
  } else if (year_num %in% sprintf("%02d", c(11:16))){
    
    names(df1)[names(df1) %in% c("COUNTY", "CCODE")] <- c("CountyCode")
    names(df1)[names(df1) %in% c("DISTRICT")] <- c("DistrictCode")
    names(df1)[names(df1) %in% c("TS1_DNAME")] <- c("DistrictName")
    names(df1)[names(df1) %in% c("TS1_COUNTY")] <- c("CountyName")
    
  }

  
  ###'######################################################################
  ###'
  ###' (2) COLMHDGS:  TS2
  ###' 
  ###'     Contains the text headings entered for each of the columns 
  ###'     in the salary schedule, Section II.
  ###'
  ###'
  
  ### Import data files
  df2 <- read.csv(file = paste0("TS2_colmhdgs", ".txt"), 
                  header = headerl, as.is = TRUE)
  
  classmode(df2, everything())
  
  if (year_num %in% sprintf("%02d", c(0))){
    
    df2 <- df2[, 1:8]
    
  }
  
  
  ### Assign variable names
  dim(df2)
  length(names_df2)
  names_df2
  
  if (year_num %in% sprintf("%02d", c(0:3))){

    names(df2) <- names_df2
    
  }
  
  
  ### Convert empty strings to NA
  df2 <- df2 %>% mutate_all(funs(empty_as_na)) 
  
  
  ### Rename indicators
  if (year_num %in% sprintf("%02d", c(0:10))){
    
    names(df2)[names(df2) %in% c("COUNTY", "CCODE")] <- c("CountyCode")
    names(df2)[names(df2) %in% c("CDS")] <- c("DistrictCode")
    
  } else if (year_num %in% sprintf("%02d", c(11:16))){
    
    names(df2)[names(df2) %in% c("COUNTY", "CCODE")] <- c("CountyCode")
    names(df2)[names(df2) %in% c("DISTRICT")] <- c("DistrictCode")
    
  }
  
  
  
  ###'######################################################################
  ###'
  ###' (3) SALXSTEP: TS3  
  ###' 
  ###'     Contains the salary and FTEs for each column and salary 
  ###'     in Section II.
  ###'
  ###'
  
  ### Import data files
  df3 <- read.csv(file = paste0("TS3_salxstep", ".txt"), 
                  header = headerl, as.is = TRUE)
  
  classmode(df3, everything())
  
  
  ### Assign variable names
  dim(df3)
  length(names_df3)
  names_df3
  
  if (year_num %in% sprintf("%02d", c(0:3))){
    
    names(df3) <- names_df3
    
  }
  
  
  ### Convert empty strings to NA
  df3 <- df3 %>% mutate_all(funs(empty_as_na)) 
  
  
  ### Rename indicators
  if (year_num %in% sprintf("%02d", c(0:10))){
    
    names(df3)[names(df3) %in% c("COUNTY", "CCODE")] <- c("CountyCode")
    names(df3)[names(df3) %in% c("CDS")] <- c("DistrictCode")
    
  } else if (year_num %in% sprintf("%02d", c(11:16))){
    
    names(df3)[names(df3) %in% c("COUNTY", "CCODE")] <- c("CountyCode")
    names(df3)[names(df3) %in% c("DISTRICT")] <- c("DistrictCode")
    
  }
  
  
  ###'######################################################################
  ###'
  ###' (4) BENEINFO: TS4 
  ###' 
  ###'     Contains the benefit information in Section V.
  ###'
  ###'
  
  ### Import data files
  df4 <- read.csv(file = paste0("TS4_beneinfo", ".txt"), 
                  header = headerl, as.is = TRUE)
  
  classmode(df4, everything())
  
  
  ### Assign variable names
  dim(df4)
  length(names_df4)
  names_df4
  
  if (year_num %in% sprintf("%02d", c(0))){
    
    names(df4) <- c("COUNTY", "CDS", "TS4_BEN", "TS4_STEP", "TS4_COLUMN",
                    "TS4_DESC", "TS4_ANNUAL", "TS4_CONTR", "TS4_FTE")
    
  } else if (year_num %in% sprintf("%02d", c(1:3))){
    
    names(df4) <- names_df4
    
  }
  
  
  ### Convert empty strings to NA
  df4 <- df4 %>% mutate_all(funs(empty_as_na)) 
  
  
  ### Rename indicators
  if (year_num %in% sprintf("%02d", c(0:10))){
    
    names(df4)[names(df4) %in% c("COUNTY", "CCODE")] <- c("CountyCode")
    names(df4)[names(df4) %in% c("CDS")] <- c("DistrictCode")
    
  } else if (year_num %in% sprintf("%02d", c(11:16))){
    
    names(df4)[names(df4) %in% c("COUNTY", "CCODE")] <- c("CountyCode")
    names(df4)[names(df4) %in% c("DISTRICT")] <- c("DistrictCode")
    
  }
  
  
  
  ###'######################################################################
  ###'
  ###' (5) retbeninfo1: TS5    
  ###' 
  ###'     Contains the benefit information in Section VIII for retirees over 65.
  ###'
  ###'
  
  if (year_num %in% sprintf("%02d", c(11:16))){
    
    ### Import data file
    df5 <- read.csv(file = paste0("TS5_retbeninfo1", ".txt"), 
                    header = headerl, as.is = TRUE)
    
    classmode(df5, everything())
    
    
    ### Convert empty strings to NA
    df5 <- df5 %>% mutate_all(funs(empty_as_na)) 
    
    
    ### Rename indicators
    names(df5)[names(df5) %in% c("COUNTY", "CCODE")] <- c("CountyCode")
    names(df5)[names(df5) %in% c("DISTRICT")] <- c("DistrictCode")
    
  }
  
  
  
  ###'######################################################################
  ###'
  ###' (6) retbeninfo2: TS6    
  ###' 
  ###'     Contains the benefit information in Section VIII for retirees 65 and under.
  ###'
  ###'
  
  if (year_num %in% sprintf("%02d", c(11:16))){
    
    ### Import data file
    df6 <- read.csv(file = paste0("TS6_retbeninfo2", ".txt"), 
                    header = headerl, as.is = TRUE)
    
    classmode(df6, everything())
    
    
    ### Convert empty strings to NA
    df6 <- df6 %>% mutate_all(funs(empty_as_na)) 
    
    
    ### Rename indicators
    names(df6)[names(df6) %in% c("COUNTY", "CCODE")] <- c("CountyCode")
    names(df6)[names(df6) %in% c("DISTRICT")] <- c("DistrictCode")
    
  }
  
  
  
  ###'######################################################################
  ###' 
  ###' Save data objects
  ###' 
  ###' 
  
  ### Set data containing working directory
  setwd(folder)
  
  
  ### Save the resulting dataframes
  save(df1, file = paste0("TS1_fixdinfo", year_pasted, "_cleaned", ".rda"))
  save(df2, file = paste0("TS2_colmhdgs", year_pasted, "_cleaned", ".rda"))
  save(df3, file = paste0("TS3_salxstep", year_pasted, "_cleaned", ".rda"))
  save(df4, file = paste0("TS4_beneinfo", year_pasted, "_cleaned", ".rda"))
  
  write.dta(df1, file = paste0("TS1_fixdinfo", year_pasted, "_cleaned", ".dta"))
  write.dta(df2, file = paste0("TS2_colmhdgs", year_pasted, "_cleaned", ".dta"))
  write.dta(df3, file = paste0("TS3_salxstep", year_pasted, "_cleaned", ".dta"))
  write.dta(df4, file = paste0("TS4_beneinfo", year_pasted, "_cleaned", ".dta"))
  
  
  ### Save the additional dataframes: year_num >= 11
  if (year_num %in% sprintf("%02d", c(11:16))){
    
    save(df5, file = paste0("TS5_retbeninfo1", year_pasted, "_cleaned", ".rda"))
    save(df6, file = paste0("TS6_retbeninfo2", year_pasted, "_cleaned", ".rda"))
    
    write.dta(df5, file = paste0("TS5_retbeninfo1", year_pasted, "_cleaned", ".dta"))
    write.dta(df6, file = paste0("TS6_retbeninfo2", year_pasted, "_cleaned", ".dta"))
    
  }


  
  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("Year ", years[i], " completed", "\n"))
  
  
} # End of loop over years

