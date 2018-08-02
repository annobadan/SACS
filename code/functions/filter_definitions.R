
###'######################################################################
###'
###' Filtering / Factoring functions  
###' 
###' to define the (sub)categories of SACS expenditures and revenues
###' 
###' 
###' (1) gen_exp_vs_rev()
###' 
###' 
###'  
###' 20180801 JoonHo Lee
###' 
###' 

### Package dependency
library(tidyverse)



###'######################################################################
###'
###' (1) gen_exp_vs_rev(): 
###'    
###'     generate a factor variable contrasting expenditures vs. revenues
###'     while excluding some categories  
###'     
###' 
###' 1-1. << EXPENDITURES >>:
###' 
###' total expenditures as all SACS outgo (objects 1000-7999)
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
###' 1-2. << REVENUES >>:
###' 
###' total revenues as all SACS revenues (objects 8000-8999)
###' 
###' except: 
###' 
###' - Adult education (resource 3900-3999, 6015-6016, 6390-6391 OR object 8671)
###' - Head Start (resource 5210-5240)
###' - Deferred maintenance (resource 6205 OR object 8540)
###' - Capital school facilities projects - state (resource 7701-7799 OR object 8545)
###' - Community redevelopment and education (resource 6280 OR object 8047, 8625)
###' - Transfers from JPAs (object 8783, 8793)
###' - Other transfers in from all others (object 8799)
###' 
###' These categories are excluded to create a measure of revenues for 
###' K-12 operating expenditures. 
###' 
###' 

gen_exp_vs_rev <- function(df, 
                           Resource = Resource, 
                           Object = Object){
  # Enquote variables
  Resource <- enquo(Resource)
  Object <- enquo(Object)
  
  # Generate a factor variable spliting expeditures vs. revenues
  df %>% 
    mutate(exp_vs_rev = case_when(
      !!Object >= 1000 & !!Object <= 7999 ~ "expenditure",
      !!Object >= 8000 & !!Object <= 8999 ~ "revenue", 
      TRUE ~ NA_character_
    )) -> df

  # Exclude some expenditure categories
  idx_exp <- with(df, 
                  which(exp_vs_rev == "expenditure" & 
                          (!!Object >= 7100 & !!Object <= 7199 | 
                           !!Object >= 7600 & !!Object <= 7629 |
                           !!Object %in% c(7211, 7221, 7281, 7280, 7212, 7222, 7282))))
  df[idx_exp, "exp_vs_rev"] <- NA

  # Exclude some revenue categories
  idx_rev <- with(df, 
                  which(exp_vs_rev == "revenue" &
                          (!!Resource >= 3900 & !!Resource <= 3999 | 
                           !!Resource %in% c(6015, 6016, 6390, 6391)|!!Object == 8671 |
                           !!Resource >= 5210 & !!Resource <= 5240 | 
                           !!Resource == 6205|!!Object == 8540 |
                           !!Resource >= 7701 & !!Resource <= 7799|!!Object == 8545 | 
                           !!Resource == 6280 | !!Object %in% c(8047, 8625) |
                           !!Object %in% c(8783, 8793, 8799))))
  df[idx_rev, "exp_vs_rev"] <- NA
  
  # Convert to factor
  df$exp_vs_rev <- factor(df$exp_vs_rev)
  
  return(df)
} 

# ### Test the code
# df <- gen_exp_vs_rev(df)



###'######################################################################
###'
###' (2) tag_exp_definitions()
###'
###' => Tag entries for Definition 1 & 2 for total expenditures 
###'    level1: Definition 1 & 2
###'    level2: Only Definition 1
###'
###' 
###' Definition 1: All fund
###' 
###' => Same with "expenditure" from gen_exp_vs_rev() function
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

tag_exp_definitions <- function(df, 
                                exp_vs_rev = exp_vs_rev, 
                                Fund = Fund){
  # Enquote variables
  exp_vs_rev <- enquo(exp_vs_rev)
  Fund <- enquo(Fund)
  
  # Generate two tags
  df %>%
    mutate(definition1 = ifelse(!!exp_vs_rev == "expenditure", 1, NA), 
           definition2 = ifelse(!!exp_vs_rev == "expenditure" & !!Fund %in% c(1, 3, 6), 1, NA))
}

# ### Test the code
# df <- tag_exp_definitions(df)



###'######################################################################
###' 
###' (3) gen_std_vs_nonstd()
###' 
###'     Generate a factor variable separating total expenditures into 
###'     Student and Non-Student spending
###'
###' 3-1. STUDENT SPENDING
###' 
###'      parallels the CDE definition of Current Expense of Education
###'      All total expenditure except the non-student spending
###'      
###'      
###' 3-2. NON-STUDENT SPENDING
###' 
###' - Debt service (Object 7430-7439)
###' - Capital Outlay and Facilities (Function 8500  OR  Object 6000-6499)
###' - Non-Agency and Community Services (Goal 7100-7199, 8100  OR  Function 5000-5999)
###' - Spending on programs for infants, pre-K, and Adults (Goal 0001-0999, 4000-4749, 5710)
###' - Retiree benefits (Object 3701, 3702)
###' - PERS Reductions (Object 3800-3899)  
###'           
###'                     

gen_std_vs_nonstd <- function(df, 
                              exp_vs_rev, 
                              Goal = Goal, 
                              Function = Function, 
                              Object = Object){
  
  # Enquote variables
  exp_vs_rev <- enquo(exp_vs_rev)
  Goal <- enquo(Goal)
  Function <- enquo(Function)
  Object <- enquo(Object)
  
  #' Generate row indices for Non-expenditures (revenues and NAs), 
  #' Non-student spending, and Student spending 
  idx_NA <- which(with(df, !!exp_vs_rev == "expenditure") %in% c(FALSE, NA))
  idx_nonstd <- with(df, 
                     which(!!exp_vs_rev == "expenditure" & 
                             (!!Object >= 7430 & !!Object <= 7439 |
                              !!Function == 8500 | 
                              !!Object >= 6000 & !!Object <= 6499 |
                              !!Goal >= 7100 & !!Goal <= 7199 | !!Goal == 8100 |
                              !!Function >= 5000 & !!Function <= 5999 | 
                              !!Goal >= 0001 & !!Goal <= 0999 | 
                              !!Goal >= 4000 & !!Goal <= 4749 | !!Goal == 5710 |
                              !!Object %in% c(3701, 3702) | 
                              !!Object >= 3800 & !!Object <= 3899)))
  
  df$std_vs_nonstd <- "Student Spending"
  df[idx_NA, "std_vs_nonstd"] <- NA
  df[idx_nonstd, "std_vs_nonstd"] <- "Non-student Spending"
  
  # Convert to factor
  df$std_vs_nonstd <- factor(df$std_vs_nonstd)
  
  return(df)
}


 
###'######################################################################
###' 
###' (4) gen_nonstd_sub()
###' 
###'     Generate a factor variable for the subcategories of 
###'     Non-student spending
###'     
###' - Debt Service (Object 7430-7439)
###' - Capital Outlay and Facilities (Function 8500  OR  Object 6000-6499)
###' - Non-Agency and Community Services (Goal 7100-7199, 8100  OR  Function 5000-5999)
###' - Spending on programs for infants, pre-K, and Adults (Goal 0001-0999, 4000-4749, 5710)
###' - Retiree benefits (Object 3701, 3702)
###' - PERS Reductions (Object 3800-3899)  
###' 
###' 

gen_nonstd_sub <- function(){
  
  
  df$nonstd_sub <- 0
  
  # Cases to exclude (NA)
  idx_NA <- which(with(df, !!std_vs_nonstd == "Non-student Spending") %in% c(FALSE, NA))
  df[idx_NA, "nonstd_sub"] <- NA
  
  # Recode Non-student spending subcategories
  idx1 <- with(df, which(!!std_vs_nonstd == "Non-student Spending" & 
                           (!!Object >= 7430 & !!Object <= 7439)))
  df[idx1, "nonstd_sub"] <- "Debt Service"
  
  idx2 <- with(df, which(!!std_vs_nonstd == "Non-student Spending" & 
                           (!!Function == 8500 | !!Object >= 6000 & !!Object <= 6499)))
  df[idx2, "nonstd_sub"] <- "Capital Outlay and Facilities" 
  
  idx3 <- with(df, which(!!std_vs_nonstd == "Non-student Spending" & 
                           (!!Function == 8500 | !!Object >= 6000 & !!Object <= 6499)))
  df[idx2, "nonstd_sub"] <- "Capital Outlay and Facilities" 
  

  
  idx1 <- with(df, 
               which(!!std_vs_nonstd == "Non-student Spending" & 
                       (!!Object >= 7430 & !!Object <= 7439 |
                          !!Function == 8500 | 
                          !!Object >= 6000 & !!Object <= 6499 |
                          !!Goal >= 7100 & !!Goal <= 7199 | !!Goal == 8100 |
                          !!Function >= 5000 & !!Function <= 5999 | 
                          !!Goal >= 0001 & !!Goal <= 0999 | 
                          !!Goal >= 4000 & !!Goal <= 4749 | !!Goal == 5710 |
                          !!Object %in% c(3701, 3702) | 
                          !!Object >= 3800 & !!Object <= 3899)))
  


  
  
}



summary(df$std_vs_nonstd)











