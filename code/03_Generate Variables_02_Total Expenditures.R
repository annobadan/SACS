
###'######################################################################
###'
###' (1) Defining Total Expenditures
###' 
###' => This forms the base for other calculations
###' 
###' 20180719 JoonHo Lee
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


### Import datasets
df_cur_exp <- read.csv(file = "table/current_expense_of_education_allyears.csv")



###'######################################################################
###'
###' Definition 1: All funds
###' 
###' Defines total expenditures as all SACS outgo (objects 1000-7999)
###' 
###' except:
###' 
###' - Tuition (objects 7100-7199)
###' - Transfers to Other Districts (objects 7211, 7221, 7281)
###' - Transfers to Charter in Lieu of Property Taxes (object 7280)
###' - Inter-fund Transfers (object7600-7629)
###' - Transfers to County Offices of Education (objects 7212, 7222, 7282)
###' 
###' These categories are primarily transfers that will be accounted for
###' elsewhere and thus should be taken out to avoid double-counting.
###'
###' 
###' Definition 2: Only the General Fund
###' 
###' Takes total expenditures under Definition 1 and
###' and make additional exclusion of all funds 
###' except the General Fund (funds 1, 3, and 6 in SACS)
###' 
###' => Most California Department of Education (CDE) finance calculations, 
###'    including official cost of education figures, include only spending 
###'    from the general fund
###'
###'        

### Prepare loop
years <- paste0(sprintf("%02d",seq(3, 16)), sprintf("%02d",seq(4, 17)))
definitions <- c("def1", "def2")

temp_list <- list()
df_exp_perADA_allyears <- data.frame()


for (i in seq_along(years)){  # Loop over years
  
  ### Import original dataset
  
  year_chr <- years[i]
  
  setwd(paste0(data_dir, "/sacs", year_chr))
  
  load(file = "UserGL_merged.rda")
  
  df <- UserGL_merged; rm(UserGL_merged) 
  
  
  for(j in seq_along(definitions)){  # Loop over two definitions  
    
    ### Filter all SACS outgo except a few categories  
    
    df_outgo_pre <- df %>%
      filter(Object >= 1000 & Object <= 7999) %>%     # filter only expenditures
      filter(!(Object >= 7100 & Object <= 7199)) %>%  # filter out tuition
      filter(!(Object >= 7600 & Object <= 7629)) %>%  # filter out interfund transfers
      filter(!Object %in% c(7211, 7221, 7281, 7280, 
                            7212, 7222, 7282))        # filter out other transfers
    
    if(j == 1){  # Definition 1
      df_outgo <- df_outgo_pre
    } else {     # Definition 2
      df_outgo <- df_outgo_pre %>%
        filter(Fund %in% c(1, 3, 6))
    }
    
    
    ### Calculate total expenditure
    
    df_exp <- df_outgo %>%
      group_by(Ccode, Dcode) %>%
      summarise(
        # Fiscal year
        Fiscalyear = first(Fiscalyear), 
        
        # District information
        Dname = first(Dname),
        Dtype = first(Dtype), 
        
        # Total Expenditure
        TotalExp = sum(Value, na.rm = TRUE), 
        TotalExp16 = sum(Value2016, na.rm = TRUE)
      ) %>%
      select(Fiscalyear, Ccode, Dcode, everything())
    
    
    ###' Average Daily Attendence (differ by years) 
    ###' FY0304 - FY0708: RegularADA, SpecialEdADA, ROCPADA, AdultEdADA
    ###' FY0809 - FY1213: RegularADA, SpecialEdADA
    ###' FY1314 - FY1617: K12ADA
    ###' => Need to generate K12ADA for FY0304 - FY1213
    
    ### Subset Current Expense of Education Data (CEE)
    df_cur_exp_ADA <- df_cur_exp %>%
      filter(FiscalYear == 2000 + as.numeric(substr(year_chr, 1, 2))) %>%
      select(Ccode, Dcode, contains("ADA"))
    
    ### Summarize the calculated total expenditure and merge with CEE
    if("K12ADA" %in% names(df_outgo)){
      df_ADA <- df %>%
        group_by(Ccode, Dcode) %>%
        summarise_at(vars(matches("ADA")), first) %>%
        left_join(df_cur_exp_ADA, by = c("Ccode", "Dcode"))
    } else {
      df_ADA <- df %>%
        group_by(Ccode, Dcode) %>%
        summarise_at(vars(matches("ADA")), first) %>%
        ungroup() %>%
        # Assume that K12ADA = RegularADA + SpecialEdADA
        # Excluding Regional Occupation Center/Program (ROCPADA) and Adult Education (AdultEdADA)
        # ROCPADA and AdultEdADA are available only for FY0304-0708
        mutate(K12ADA = rowSums(df_ADA[, c("RegularADA", "SpecialEdADA")])) %>%
        left_join(df_cur_exp_ADA, by = c("Ccode", "Dcode"))
    }
    
    
    ###' Generate total expenditure per ADA
    ###' We use the K12ADA measure from the Current Expense of Education Data
    ###' because it is assumed to be more accurate. 
    
    df_exp_perADA <- df_exp %>% 
      left_join(df_ADA, by = c("Ccode", "Dcode")) %>% 
      # Filter out COE and JPA
      filter(!Dtype %in% c("CO OFFICE", "JPA")) %>%
      # Calculate total expenditures per ADA
      mutate(
        TotalExp_PP = TotalExp/K12ADA_C, 
        TotalExp16_PP = TotalExp16/K12ADA_C
      ) 
    
    summary(df_exp_perADA)
    
    
    ###' Prorating COE office expenditure
    ###' 
    ###' - County Offices of Education (COEs) also provide administrative, 
    ###'   instructional and other services to all districts in a given county.
    ###'   
    ###' - It is not clear how best to attribute spending by COEs to their 
    ###'   affiliated districts.
    ###'   
    ###' - The strategy we employ is to exclude all transfers to COEs
    ###'   but then add back average per student spending by the affiliated COE. 
    ###' 
    
    df_County_ADA <- df_ADA %>%
      group_by(Ccode) %>%
      summarise(K12ADA_C_Avg = sum(K12ADA_C, na.rm = TRUE))
    
    df_COE_prorate <- df_exp %>%
      filter(Dtype == "CO OFFICE") %>%
      left_join(df_County_ADA, by = c("Ccode")) %>%
      mutate(COE_prorate = TotalExp/K12ADA_C_Avg, 
             COE_prorate16 = TotalExp16/K12ADA_C_Avg)
    
    df_exp_perADA <- df_exp_perADA %>%
      left_join(df_COE_prorate[, c("Ccode", "COE_prorate", "COE_prorate16")], 
                by = c("Ccode")) %>%
      mutate(TotalExp_PP_COE = TotalExp_PP + COE_prorate, 
             TotalExp16_PP_COE = TotalExp16_PP + COE_prorate16)
    
    
    ### Store in the `j`th list
    temp_list[[j]] <- df_exp_perADA
    
  } # End of loop over definitions (j)
  
  ### Merge data frames from the two definitions
  def1 <- temp_list[[1]]
  def2 <- temp_list[[2]]
  
  names(def2)[-(1:5)] <- paste0(names(def2)[-(1:5)], "_d2")

  temp_merged <- left_join(def1, select(def2, -Fiscalyear, -Dname, -Dtype, -contains("ADA")), 
                           by = c("Ccode", "Dcode"))
  
  ### Append to previous dataframe
  df_exp_perADA_allyears <- rbind.data.frame(df_exp_perADA_allyears, temp_merged)
    
} # End of loop over years (i)

