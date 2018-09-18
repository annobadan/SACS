
###'######################################################################
###'
###' Generate categories 
###' 
###' to define the (sub)categories of SACS expenditures and revenues
###'  
###' 20180801 JoonHo Lee
###' 
###' 

### Package dependency
library(tidyverse)



# ###'######################################################################
# ###'
# ###' Import data for testing
# ###'
# ###'
# 
# data_dir <- c("D:/Data/LCFF/Financial/Annual Financial Data")
# years <- paste0(sprintf("%02d",seq(3, 16)), sprintf("%02d",seq(4, 17)))
# i = 1
# year_chr <- years[i]
# setwd(paste0(data_dir, "/sacs", year_chr))
# load(file = "UserGL_merged.rda")
# df <- UserGL_merged



###'######################################################################
###'
###' Define a simple helper function to generate filter
###' 
###' 

gen_factor <- function(df, 
                       varname, 
                       remainders,     
                       precondition, 
                       condition_list, 
                       name_vec){
  
  # Generate a uniform vector
  df$newvar <- remainders
  
  # Include only cases satisfying the pre-condition
  idx_NA <- which(precondition %in% c(FALSE, NA))
  df[idx_NA, "newvar"] <- NA
  
  # Generate row indices out of condition vectors 
  index_list <- lapply(condition_list, function(x) which(precondition & x))
  
  # Recode categories
  for (i in seq_along(name_vec)){
    df[index_list[[i]], "newvar"] <- name_vec[i]
  }
  
  # Convert to a factor
  df$newvar <- factor(df$newvar, levels = c(name_vec, remainders))
  
  # Delete the column if its name match with varname
  df <- df[, !names(df) == varname]
  
  # Rename the resulting variable
  names(df)[names(df) == "newvar"] <- varname
  
  return(df)
}



###'######################################################################
###'
###' Define a function generating categories
###' 
###' 

generate_categories <- function(df){
  
 
  ###'######################################################################
  ###'
  ###' Expenditures vs. Revenues
  ###'     
  ###' Exclude some categories  
  ###'     
  ###' 
  ###' << EXPENDITURES >>:
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
  ###' << REVENUES >>:
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
  ###' Definition 1 & 2 for total expenditures 
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
  
  df <- df %>%
    mutate(definition1 = ifelse(exp_vs_rev == "expenditure", 1, NA), 
           definition2 = ifelse(exp_vs_rev == "expenditure" & Fund %in% c(1, 3, 6), 1, NA))
  
  
  
  ###'######################################################################
  ###' 
  ###' Student vs. Non-Student Spending
  ###'
  ###' STUDENT SPENDING
  ###' 
  ###'      parallels the CDE definition of Current Expense of Education
  ###'      All total expenditure except the non-student spending
  ###'      
  ###'      
  ###' NON-STUDENT SPENDING
  ###' 
  ###' - Debt service (Object 7430-7439)
  ###' - Capital Outlay and Facilities (Function 8500  OR  Object 6000-6499)
  ###' - Non-Agency and Community Services (Goal 7100-7199, 8100  OR  Function 5000-5999)
  ###' - Spending on programs for infants, pre-K, and Adults (Goal 0001-0999, 4000-4749, 5710)
  ###' - Retiree benefits (Object 3701, 3702)
  ###' - PERS Reductions (Object 3800-3899)  
  ###'           
  ###'  
  
  varname <- "std_vs_nonstd"
  
  remainders <- "Student Spending"
  
  precondition <- df$exp_vs_rev == "expenditure"
  
  condition_list <- list(
    with(df, Object >= 7430 & Object <= 7439 | 
           Function == 8500 | Object >= 6000 & Object <= 6499 | 
           Goal >= 7100 & Goal <= 7199 | Goal == 8100 | Function >= 5000 & Function <= 5999 | 
           Goal >= 0001 & Goal <= 0999 | Goal >= 4000 & Goal <= 4749 | Goal == 5710 | 
           Object %in% c(3701, 3702) | 
           Object >= 3800 & Object <= 3899)
    )
  
  name_vec <- c("Non-Student Spending")
  
  df <- gen_factor(df, varname, remainders, precondition, condition_list, name_vec)

  
  
  ###'######################################################################
  ###' 
  ###'  Subcategories of Non-student spending 
  ###'          
  ###' - Debt Service (Object 7430-7439)
  ###' - Capital Outlay and Facilities (Function 8500  OR  Object 6000-6499)
  ###' - Non-Agency and Community Services (Goal 7100-7199, 8100  OR  Function 5000-5999)
  ###' - Spending on Programs for Infants, Pre-K, and Adults (Goal 0001-0999, 4000-4749, 5710)
  ###' - Retiree Benefits (Object 3701, 3702)
  ###' - PERS Reductions (Object 3800-3899)  
  ###' 
  ###' 
  
  varname <- "nonstd_sub"
  
  remainders <- "Other Non-Student Spending"
  
  precondition <- df$std_vs_nonstd == "Non-Student Spending"
  
  condition_list <- list(
    with(df, Object >= 7430 & Object <= 7439), 
    with(df, Function == 8500 | Object >= 6000 & Object <= 6499),
    with(df, Goal >= 7100 & Goal <= 7199 | Goal == 8100 | Function >= 5000 & Function <= 5999),
    with(df, Goal >= 0001 & Goal <= 0999 | Goal >= 4000 & Goal <= 4749 | Goal == 5710), 
    with(df, Object %in% c(3701, 3702)), 
    with(df, Object >= 3800 & Object <= 3899)
  )
  
  name_vec <- c("Debt Service", 
                "Capital Outlay and Facilities", 
                "Non-Agency and Community Service", 
                "Spending on Programs for Infants, Pre-K, and Adults",
                "Retiree Benefits", 
                "PERS Reductions")
  
  df <- gen_factor(df, varname, remainders, precondition, condition_list, name_vec)

  
  
  ###'######################################################################
  ###' 
  ###' Subcategories of Student Spending 
  ###' 
  ###' (1) By object codes
  ###' 
  ###' - Salaries (object 1000-2999)
  ###' - Employee Benefits (object 3000-3999)
  ###' - Books & Supplies (object 4000-4999)
  ###' - Services & Other Operating Expenditures (object 5000-5999)
  ###' - Equipment Replacement (object 6500)
  ###'  
  ###'   

  varname <- "std_sub"
  
  remainders <- "Other Student Spending"
  
  precondition <- df$std_vs_nonstd == "Student Spending"
  
  condition_list <- list(
    with(df, Object >= 1000 & Object <= 2999),
    with(df, Object >= 3000 & Object <= 3999), 
    with(df, Object >= 4000 & Object <= 4999),
    with(df, Object >= 5000 & Object <= 5999),
    with(df, Object == 6500)
  )
  
  name_vec <- c("Salaries", 
                "Employee Benefits", 
                "Books & Supplies", 
                "Services & Other Operating Expenditures", 
                "Equipment Replacement")
  
  df <- gen_factor(df, varname, remainders, precondition, condition_list, name_vec)
  
  
  
  ###'######################################################################
  ###' 
  ###' Subcategories of Salaries
  ###' 
  ###' - Certificated Teachers (object 1100)
  ###' - Administrators and Supervisors (object 1300, 2300)
  ###' - Other Certificated (object 1000-1999, except for 1100, 1300)
  ###' - Other Classified (oject 2000-2999, except for 2300)
  ###'  
  ###'   
  
  varname <- "salaries"
  
  remainders <- "Other Salaries"
  
  precondition <- df$std_sub == "Salaries"
  
  condition_list <- list(
    with(df, Object == 1100),
    with(df, Object %in% c(1300, 2300)), 
    with(df, Object >= 1000 & Object <= 1999 & !(Object %in% c(1100, 1300))),
    with(df, Object >= 2000 & Object <= 2999 & !(Object %in% c(2300)))
  )
  
  name_vec <- c("Certificated Teachers",
                "Administrators and Supervisors", 
                "Other Certificated", 
                "Other Classified")
  
  df <- gen_factor(df, varname, remainders, precondition, condition_list, name_vec)
  
  
  
  ###'######################################################################
  ###' 
  ###' Subcategories of Employee Benefits
  ###' 
  ###' - Current Health and Welfare, Certificated (object 3401)
  ###' - Current Health and Welfare, Classified (object 3402)
  ###' - Retirement Benefits, Certificated (object 3101, 3201)
  ###' - Retirement Benefits, Classfied (object 3102, 3202)
  ###'  
  ###'   
  
  varname <- "benefits"
  
  remainders <- "Other Benefits"
  
  precondition <- df$std_sub == "Employee Benefits"
  
  condition_list <- list(
    with(df, Object == 3401),
    with(df, Object == 3402), 
    with(df, Object %in% c(3101, 3201)), 
    with(df, Object %in% c(3102, 3202))
  )
  
  name_vec <- c("Current Health and Welfare, Certificated",
                "Current Health and Welfare, Classified", 
                "Retirement Benefits, Certificated", 
                "Retirement Benefits, Classfied")
  
  df <- gen_factor(df, varname, remainders, precondition, condition_list, name_vec)
  

  
  ###'######################################################################
  ###' 
  ###' Subcategories of Student Spending (All Instructional Expenditures)
  ###' 
  ###' (2) By Goal codes
  ###' 
  ###' - General Education, K-12 (Goal 1000-3999)
  ###' - Supplemental Education, K-12 (Goal 4750-4999)
  ###' - Special Education  (Goal 5000-5999)
  ###' - ROC/P (Goal 6000-6999)
  ###' - Child Care & Development (Goal 8500-8599)
  ###' 
  ###' 
  
  varname <- "goals"
  
  remainders <- "Other Goals"
  
  precondition <- df$std_vs_nonstd == "Student Spending"
  
  condition_list <- list(
    with(df, Goal >= 1000 & Goal <= 3999),
    with(df, Goal >= 4750 & Goal <= 4999),
    with(df, Goal >= 5000 & Goal <= 5999),
    with(df, Goal >= 6000 & Goal <= 6999), 
    with(df, Goal >= 8500 & Goal <= 8599)
  )
  
  name_vec <- c("General Education, K-12", 
                "Supplemental Education, K-12", 
                "Special Education", 
                "ROC/P", 
                "Child Care & Development") 
  
  df <- gen_factor(df, varname, remainders, precondition, condition_list, name_vec)
  
  
  
  ###'######################################################################
  ###' 
  ###'  General Education, K-12 - Subcagetories
  ###'  
  ###' - Regular Education, K-12 (Goal 1100-1029)
  ###' - Other K-12 Schools (Goal 3100-3700)
  ###' - Vocational Education (Goal 3800)
  ###' 
  ###' 

  varname <- "general_Ed"
  
  precondition <- df$goals == "General Education, K-12"
  
  remainders <- "Other General Education, K-12"
  
  condition_list <- list(
    with(df, Goal >= 1100 & Goal <= 1129), 
    with(df, Goal >= 3100 & Goal <= 3700), 
    with(df, Goal == 3800)
  )
  
  name_vec <- c("Regular Education, K-12", 
                "Other K-12 Schools", 
                "Vocational Education")
  
  df <- gen_factor(df, varname, remainders, precondition, condition_list, name_vec)
  
  
  
  ###'######################################################################
  ###'
  ###'  Supplemental Education, K-12 - Subcategories
  ###'  
  ###' - Bilingual Education (Goal 4760)
  ###' - Migrant Education (Goal 4850)
  ###' - Other Supplemental Education (Goal 4900)
  ###'
  ###'
  
  varname <- "supp_Ed"
  
  precondition <- df$goals == "Supplemental Education, K-12"
  
  remainders <- NULL
  
  condition_list <- list(
    with(df, Goal == 4760), 
    with(df, Goal == 4850), 
    with(df, Goal == 4900)
  )
  
  name_vec <- c("Bilingual Education", 
                "Migrant Education", 
                "Other Supplemental Education")
  
  df <- gen_factor(df, varname, remainders, precondition, condition_list, name_vec)
  
  
  
  ###'######################################################################
  ###'
  ###'  Special Education - Subcategories
  ###'  
  ###' - Pre School (Goal 5730)
  ###' - Severely Disabled, Age 5-22 (Goal 5750)
  ###' - Nonseverely Disabled, Age 5-22 (Goal 5770)
  ###'
  ###'
  
  varname <- "SPED"
  
  precondition <- df$goals == "Special Education"
  
  remainders <- "Other SPED"
  
  condition_list <- list(
    with(df, Goal == 5730), 
    with(df, Goal == 5750), 
    with(df, Goal == 5770)
  )
  
  name_vec <- c("Pre School", 
                "Severely Disabled, Age 5-22", 
                "Nonseverely Disabled, Age 5-22")
  
  df <- gen_factor(df, varname, remainders, precondition, condition_list, name_vec)
  
  
  
  ###'######################################################################
  ###'
  ###' Subcategories of Student Spending
  ###' 
  ###' (2) By Function codes
  ###' 
  ###' - General Education Instruction (Function 1000-1999 EXCEPT 1100-1199)
  ###' - Special Education Instruction (Function 1100-1199)
  ###' - Instruction-related Services (Function 2000-2999)
  ###' - Pupil Services (Function 3000-3999)
  ###' - Ancillary Services (Function 4000-4999)
  ###' - Community Services (5000-5999) => Remove (no cases for student spending)
  ###' - Enterprise (6000-6999)
  ###' - General Administration (Function 7000-7999)
  ###' - Ongoing Plant Services (Function 8000-8999)
  ###'
  ###'
  
  varname <- "functions"
  
  precondition <- df$std_vs_nonstd == "Student Spending"
  
  remainders <- "Other Student Spending"
  
  condition_list <- list(
    with(df, (Function >= 1000 & Function <= 1999) & !(Function >= 1100 & Function <= 1199)), 
    with(df, Function >= 1100, Function <= 1199), 
    with(df, Function >= 2000 & Function <= 2999), 
    with(df, Function >= 3000 & Function <= 3999), 
    with(df, Function >= 4000 & Function <= 4999), 
    with(df, Function >= 6000 & Function <= 6999), 
    with(df, Function >= 7000 & Function <= 7999), 
    with(df, Function >= 8000 & Function <= 8999)
  )
  
  name_vec <- c("General Education Instruction", 
                "Special Education Instruction", 
                "Instruction-related Services", 
                "Pupil Services", 
                "Ancillary Services", 
                "Enterprise", 
                "General Administration", 
                "Ongoing Plant Services")
  
  df <- gen_factor(df, varname, remainders, precondition, condition_list, name_vec)
  
  
  
  ###'######################################################################
  ###'
  ###' Special Education Instruction: Subcategories
  ###' 
  ###' - Separate Classes (Function 1110)
  ###' - Resource Specialist Instruction (Function 1120)
  ###' - Aids in Regular Classrooms (Function 1130)
  ###' - Nonpublic Agencies/Schools (Function 1180) 
  ###' - Other Specialized Servicies (Function 1190)
  ###' 
  ###'

  varname <- "SPED_inst" 
  
  precondition <- df$functions == "Special Education Instruction"
  
  remainders <- "Other SPED instruction"
  
  condition_list <- list(
    with(df, Function == 1110),
    with(df, Function == 1120), 
    with(df, Function == 1130), 
    with(df, Function == 1180), 
    with(df, Function == 1190)
  )
  
  name_vec <- c("Separate Classes", 
                "Resource Specialist Instruction", 
                "Aids in Regular Classrooms", 
                "Nonpublic Agencies/Schools", 
                "Other Specialized Servicies")
  
  df <- gen_factor(df, varname, remainders, precondition, condition_list, name_vec)
  
  
  
  ###'######################################################################
  ###'
  ###' Pupil Services: Subcategories
  ###' 
  ###' - Guidance and Counseling Services (Function 3110)
  ###' - Psychological Services (Function 3120)
  ###' - Attendence and Social Work Servicies (Function 3130)
  ###' - Pupil Testing Services (Function 3160)
  ###' - Health Services (Function 3140)
  ###' - Food Services (Function 3700)
  ###' - Transportation Services (Function 3600)
  ###'
  ###'
  
  varname <- "pupil_services"
  
  precondition <- df$functions == "Pupil Services"
  
  remainders <- "Other Pupil Services"
  
  condition_list <- list(
    with(df, Function == 3110), 
    with(df, Function == 3120), 
    with(df, Function == 3130), 
    with(df, Function == 3160), 
    with(df, Function == 3140), 
    with(df, Function == 3700), 
    with(df, Function == 3600)
  )
  
  name_vec <- c("Guidance and Counseling", 
                "Psychological Services", 
                "Attendence & Social Work", 
                "Pupil Testing", 
                "Health", 
                "Food", 
                "Transportation")
  
  df <- gen_factor(df, varname, remainders, precondition, condition_list, name_vec)
  
  
  
  ###'######################################################################
  ###'
  ###' Unrestricted vs. Restricted Revenues
  ###'
  ###' - Unrestricted Resources (Resource 0000-1999)
  ###' - Restricted Resources (Resource 2000-9999)
  ###'
  ###'
  
  varname <- "unres_vs_res"
  
  precondition <- df$exp_vs_rev == "revenue"
  
  remainders <- NULL
  
  condition_list <- list(
    with(df, Resource >= 0 & Resource <= 1999), 
    with(df, Resource >= 2000 & Resource <= 9999)
  )

  name_vec <- c("Unrestricted Resources", 
                "Restricted Resources")
  
  df <- gen_factor(df, varname, remainders, precondition, condition_list, name_vec)
  
  
  
  ###'######################################################################
  ###'
  ###' Restricted Revenues: Subcategories
  ###' 
  ###' - Revenue Limit Resources (Resource 2000-2999)
  ###' - Federal Resources (Resource 3000-5999) 
  ###' - State Resources (Resource 6000-7999)
  ###' - Local Resources (Resource 8000-9999)
  ###' 
  ###' 

  varname <- "restricted_sub"
  
  precondition <- df$unres_vs_res == "Restricted Resources"
  
  remainders <- "Other Restricted Resources"
  
  condition_list <- list(
    with(df, Resource >= 2000 & Resource <= 2999), 
    with(df, Resource >= 3000 & Resource <= 5999), 
    with(df, Resource >= 6000 & Resource <= 7999), 
    with(df, Resource >= 8000 & Resource <= 9999)
  )
  
  name_vec <- c("Revenue Limit Resources", 
                "Federal Resources", 
                "State Resources", 
                "Local Resources")
  
  df <- gen_factor(df, varname, remainders, precondition, condition_list, name_vec)
  
  
  
  ###'######################################################################
  ###'  
  ###'  Revenue Subcategories by Object codes
  ###'  => For more detailed description of revenues
  ###'  => Regardless of Unrestricted / Restricted
  ###'  
  ###' - Revenue Limit (LCFF) Sources (Object 8000-8099)
  ###' - Federal Revenue Sources (Object 8100-8299)
  ###' - Other State Revenue (Object 8300-8599)
  ###' - Other Local Revenues, not Contributions (Object 8600-8799, Except 8699)
  ###' - Other Local Revenues, Contributions (Object 8699)
  ###'   -> Contributions from parents and foundations
  ###' - Other Financing Sources (Object 8900-8999)
  ###' 
  ###'
  
  varname <- "revenue_by_object"
  
  precondition <- df$exp_vs_rev == "revenue"
  
  remainders <- "Other Revenue"
  
  condition_list <- list(
    with(df, Object >= 8000 & Object <= 8099), 
    with(df, Object >= 8100 & Object <= 8299), 
    with(df, Object >= 8300 & Object <= 8599), 
    with(df, (Object >= 8600 & Object <= 8799) & !(Object == 8699)), 
    with(df, Object == 8699), 
    with(df, Object >= 8900 & Object <= 8999) 
  )
  
  name_vec <- c("Revenue Limit (LCFF from 2013)", 
                "Federal Revenue", 
                "Other State Revenue", 
                "Other Local Revenue, not Contributions", 
                "Other Local Revenue, Contributions", 
                "Other Financing Sources")
  
  df <- gen_factor(df, varname, remainders, precondition, condition_list, name_vec)
  
  
  
  ###'######################################################################
  ###' 
  ###' Additional Revenue Categories
  ###'  => To look at particular categories of interest
  ###'
  ###'  - County and District Taxes 
  ###'  - Lottery Revenues (State)
  ###'  - Class Size Reduction Revenues (State) 
  ###'  - NCLB Act Monies (Federal)  
  ###'  - Special Education Monies (Federal & State)
  ###'  - Vocational Education Monies (Federal)
  ###'  - Child Nutrition Funds (Federal)
  ###'  - Staff Development (State)
  ###'
  ###'              
  
  varname <- "revenue_interest"
  
  precondition <- df$exp_vs_rev == "revenue"
  
  remainders <- "Other Revenues"
  
  condition_list <- list(
    with(df, (Object >= 8040 & Object <= 8079) | (Object >= 8610 & Object <= 8629)),
    with(df, Resource %in% c(1100, 6300)), 
    with(df, Resource %in% c(1200, 1300, 6200)), 
    with(df, (Resource >= 3010 & Resource <= 3185) | 
           (Resource %in% c(3710, 3715, 3718)) | 
           (Resource >= 4035 & Resource <= 4204) | 
           (Resource %in% c(4610, 5510, 5630))), 
    with(df, (Resource >= 3310 & Resource <= 3405) | 
           (Resource >= 6500 & Resource <= 6540)), 
    with(df, Resource >= 3505 & Resource <= 3555), 
    with(df, Resource >= 5310 & Resource <= 5455), 
    with(df, (Resource >= 7280 & Resource <= 7352) | 
           (Resource %in% c(7120, 7271, 6260, 6261, 6262, 6263)))
  )
  
  name_vec <- c("County and District Taxes", 
                "Lottery Revenues (State)", 
                "Class Size Reduction Revenues (State)",  
                "NCLB Act Monies (Federal)",   
                "Special Education Monies (Federal & State)", 
                "Vocational Education Monies (Federal)", 
                "Child Nutrition Funds (Federal)", 
                "Staff Development (State)")
  
  df <- gen_factor(df, varname, remainders, precondition, condition_list, name_vec)
  
  
  return(df)
}