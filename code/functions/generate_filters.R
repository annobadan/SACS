
###'######################################################################
###'
###' Generate filters 
###' 
###' to define the (sub)categories of SACS expenditures and revenues
###'  
###' 20180801 JoonHo Lee
###' 
###' 

### Package dependency
library(tidyverse)


# ### Import data for testing
# data_dir <- c("D:/Data/LCFF/Financial/Annual Financial Data")
# years <- paste0(sprintf("%02d",seq(3, 16)), sprintf("%02d",seq(4, 17)))
# i = 1
# year_chr <- years[i]
# setwd(paste0(data_dir, "/sacs", year_chr))
# load(file = "UserGL_merged.rda")
# df <- UserGL_merged


### Define the function
generate_filters <- function(df){
 
  ###'######################################################################
  ###'
  ###' (1) Generate a factor variable contrasting expenditures vs. revenues
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
  
  # Generate a factor variable spliting expeditures vs. revenues
  df %>% 
    mutate(exp_vs_rev = case_when(
      Object >= 1000 & Object <= 7999 ~ "expenditure",
      Object >= 8000 & Object <= 8999 ~ "revenue", 
      TRUE ~ NA_character_
    )) -> df
  
  # Exclude some expenditure categories
  idx_exp <- with(df, 
                  which(exp_vs_rev == "expenditure" & 
                          (Object >= 7100 & Object <= 7199 | 
                             Object >= 7600 & Object <= 7629 |
                             Object %in% c(7211, 7221, 7281, 7280, 7212, 7222, 7282))))
  df[idx_exp, "exp_vs_rev"] <- NA
  
  # Exclude some revenue categories
  idx_rev <- with(df, 
                  which(exp_vs_rev == "revenue" &
                          (Resource >= 3900 & Resource <= 3999 | 
                             Resource %in% c(6015, 6016, 6390, 6391)|Object == 8671 |
                             Resource >= 5210 & Resource <= 5240 | 
                             Resource == 6205|Object == 8540 |
                             Resource >= 7701 & Resource <= 7799|Object == 8545 | 
                             Resource == 6280 | Object %in% c(8047, 8625) |
                             Object %in% c(8783, 8793, 8799))))
  df[idx_rev, "exp_vs_rev"] <- NA
  
  # Convert to factor
  df$exp_vs_rev <- factor(df$exp_vs_rev)
  
  
  ###'######################################################################
  ###'
  ###' Tag entries for Definition 1 & 2 for total expenditures 
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
  
  df <- df %>%
    mutate(definition1 = ifelse(exp_vs_rev == "expenditure", 1, NA), 
           definition2 = ifelse(exp_vs_rev == "expenditure" & Fund %in% c(1, 3, 6), 1, NA))
  
  
  ###'######################################################################
  ###' 
  ###' Generate a factor variable separating total expenditures into 
  ###' Student and Non-Student spending
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
  
  #' Generate row indices for Non-expenditures (revenues and NAs), 
  #' Non-student spending, and Student spending 
  idx_NA <- which(with(df, exp_vs_rev == "expenditure") %in% c(FALSE, NA))
  idx_nonstd <- with(df, 
                     which(exp_vs_rev == "expenditure" & 
                             (Object >= 7430 & Object <= 7439 |
                                Function == 8500 | 
                                Object >= 6000 & Object <= 6499 |
                                Goal >= 7100 & Goal <= 7199 | Goal == 8100 |
                                Function >= 5000 & Function <= 5999 | 
                                Goal >= 0001 & Goal <= 0999 | 
                                Goal >= 4000 & Goal <= 4749 | Goal == 5710 |
                                Object %in% c(3701, 3702) | 
                                Object >= 3800 & Object <= 3899)))
  
  df$std_vs_nonstd <- "Student Spending"
  df[idx_NA, "std_vs_nonstd"] <- NA
  df[idx_nonstd, "std_vs_nonstd"] <- "Non-student Spending"
  
  # Convert to factor
  df$std_vs_nonstd <- factor(df$std_vs_nonstd)
  
  
  ###'######################################################################
  ###' 
  ###' Generate a factor variable for the subcategories of Non-student spending 
  ###'          
  ###' - Debt Service (Object 7430-7439)
  ###' - Capital Outlay and Facilities (Function 8500  OR  Object 6000-6499)
  ###' - Non-Agency and Community Services (Goal 7100-7199, 8100  OR  Function 5000-5999)
  ###' - Spending on Programs for Infants, Pre-K, and Adults (Goal 0001-0999, 4000-4749, 5710)
  ###' - Retiree Benefits (Object 3701, 3702)
  ###' - PERS Reductions (Object 3800-3899)  
  ###' 
  ###' 
  
  # Generate a uniform vector
  df$nonstd_sub <- "Other Non-Student Spending"
  
  # Exclude NA, revenues, student spending
  idx_NA <- which(with(df, std_vs_nonstd == "Non-student Spending") %in% c(FALSE, NA))
  df[idx_NA, "nonstd_sub"] <- NA
  
  # Recode Non-student spending subcategories
  nonstd_TRUE <- df$std_vs_nonstd == "Non-student Spending"
  
  idx_list <- list(
    with(df, which(nonstd_TRUE & (Object >= 7430 & Object <= 7439))), 
    with(df, which(nonstd_TRUE & (Function == 8500 | Object >= 6000 & Object <= 6499))),
    with(df, which(nonstd_TRUE & (Goal >= 7100 & Goal <= 7199 | Goal == 8100 | 
                                    Function >= 5000 & Function <= 5999))),
    with(df, which(nonstd_TRUE & (Goal >= 0001 & Goal <= 0999 | 
                                    Goal >= 4000 & Goal <= 4749 | Goal == 5710 ))), 
    with(df, which(nonstd_TRUE & (Object %in% c(3701, 3702)))), 
    with(df, which(nonstd_TRUE & (Object >= 3800 & Object <= 3899)))
    )
  
  name_vec <- c("Debt Service", 
                "Capital Outlay and Facilities", 
                "Non-Agency and Community Service", 
                "Spending on Programs for Infants, Pre-K, and Adults",
                "Retiree Benefits", 
                "PERS Reductions")
  
  for (i in seq_along(name_vec)){
    df[idx_list[[i]], "nonstd_sub"] <- name_vec[i]
  }
  
  # Convert to a factor
  df$nonstd_sub <- factor(df$nonstd_sub, levels = c(name_vec, "Other Non-Student Spending"))
  
  
  ###'######################################################################
  ###' 
  ###' Defining subcategories of student spending
  ###' 
  ###' (1) By Object codes
  ###' 
  ###' - Salaries (object 1000-2999)
  ###' - Employee Benefits (object 3000-3999)
  ###' - Books & Supplies (object 4000-4999)
  ###' - Services & Other Operating Expenditures (object 5000-5999)
  ###' - Equipment Replacement (object 6500)
  ###'  
  ###'   

  # Generate a uniform vector
  df$std_sub <- "Other Student Spending"
  
  # Exclude NA, revenues, non-student spending
  idx_NA <- which(with(df, std_vs_nonstd == "Student Spending") %in% c(FALSE, NA))
  df[idx_NA, "std_sub"] <- NA
  
  # Recode Non-student spending subcategories
  std_TRUE <- df$std_vs_nonstd == "Student Spending"
  
  idx_list <- list(
    with(df, which(std_TRUE & (Object >= 1000 & Object <= 2999))),
    with(df, which(std_TRUE & (Object >= 3000 & Object <= 3999))), 
    with(df, which(std_TRUE & (Object >= 4000 & Object <= 4999))),
    with(df, which(std_TRUE & (Object >= 5000 & Object <= 5999))),
    with(df, which(std_TRUE & (Object == 6500)))
  )
  
  name_vec <- c("Salaries", 
                "Employee Benefits", 
                "Books & Supplies", 
                "Services & Other Operating Expenditures", 
                "Equipment Replacement")
  
  for (i in seq_along(name_vec)){
    df[idx_list[[i]], "std_sub"] <- name_vec[i]
  }
  
  # Convert to a factor
  df$std_sub <- factor(df$std_sub, levels = name_vec)
  
  
  ###'######################################################################
  ###' 
  ###' Defining subcategories of Salaries
  ###' 
  ###' - Certificated Teachers (object 1100)
  ###' - Administrators and Supervisors (object 1300, 2300)
  ###' - Other Certificated (object 1000-1999, except for 1100, 1300)
  ###' - Other Classified (oject 2000-2999, except for 2300)
  ###'  
  ###'   
  
  # Generate a uniform vector
  df$salaries <- "Other Salaries"
  
  # Exclude non-salaries
  idx_NA <- which(with(df, std_sub == "Salaries") %in% c(FALSE, NA))
  df[idx_NA, "salaries"] <- NA
  
  # Recode salaries subcategories
  salaries_TRUE <- df$std_sub == "Salaries"
  
  idx_list <- list(
    with(df, which(salaries_TRUE & (Object == 1100))),
    with(df, which(salaries_TRUE & (Object %in% c(1300, 2300)))), 
    with(df, which(salaries_TRUE & (Object >= 1000 & Object <= 1999) & !(Object %in% c(1100, 1300)))),
    with(df, which(salaries_TRUE & (Object >= 2000 & Object <= 2999) & !(Object %in% c(2300))))
  )
  
  name_vec <- c("Certificated Teachers",
                "Administrators and Supervisors", 
                "Other Certificated", 
                "Other Classified")
  
  for (i in seq_along(name_vec)){
    df[idx_list[[i]], "salaries"] <- name_vec[i]
  }
  
  # Convert to a factor
  df$salaries <- factor(df$salaries, levels = c(name_vec, "Other Salaries"))
  
  
  ###'######################################################################
  ###' 
  ###' Defining subcategories of Employee Benefits
  ###' 
  ###' - Current Health and Welfare, Certificated (object 3401)
  ###' - Current Health and Welfare, Classified (object 3402)
  ###' - Retirement Benefits, Certificated (object 3101, 3201)
  ###' - Retirement Benefits, Classfied (object 3102, 3202)
  ###'  
  ###'   
  
  # Generate a uniform vector
  df$benefits <- "Other Benefits"
  
  # Exclude non-benefits
  idx_NA <- which(with(df, std_sub == "Employee Benefits") %in% c(FALSE, NA))
  df[idx_NA, "benefits"] <- NA
  
  # Recode benefits subcategories
  benefits_TRUE <- df$std_sub == "Employee Benefits"
  
  idx_list <- list(
    with(df, which(benefits_TRUE & (Object == 3401))),
    with(df, which(benefits_TRUE & (Object == 3402))), 
    with(df, which(benefits_TRUE & (Object %in% c(3101, 3201)))), 
    with(df, which(benefits_TRUE & (Object %in% c(3102, 3202))))
  )
  
  name_vec <- c("Current Health and Welfare, Certificated",
                "Current Health and Welfare, Classified", 
                "Retirement Benefits, Certificated", 
                "Retirement Benefits, Classfied")
  
  for (i in seq_along(name_vec)){
    df[idx_list[[i]], "benefits"] <- name_vec[i]
  }
  
  # Convert to a factor
  df$benefits <- factor(df$benefits, levels = c(name_vec, "Other Benefits"))
  
  return(df)
}