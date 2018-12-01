
###'######################################################################
###'
###' Model Estimation
###'
###' The Per-Cluster (PC) Estimator: Mean Effects and Quantile Effeects
###' 
###' 01_ALL SCHOOLS
###' 
###' 
###' (1) Mean Effects
###' 
###' Bates, M. D., Castellano, K. E., Rabe-Hesketh, S., & Skrondal, A. (2014). 
###' Handling correlations between covariates and random slopes in multilevel models. 
###' Journal of Educational and Behavioral Statistics, 39(6), 524-549.
###' 
###' - Before we go to the grouped IV quantile regression, 
###'   we first get the average effects using the PC estimator
###' 
###' 
###' (2) Quantile Effects
###' 
###' Chetverikov, D., Larsen, B., & Palmer, C. (2016). 
###' IV quantile regression for group‐level treatments,
###' with an application to the distributional effects of trade. 
###' Econometrica, 84(2), 809-833.
###' 
###' 
###' 20181101 JoonHo Lee 
###' 20181113 JoonHo Lee - Complete snippets
###' 20181118 JoonHo Lee - Add quantile estimation
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


### Call libraries
library(tidyverse)
library(broom)
library(collateral)
library(foreign)
library(quantreg)
library(multcomp)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import datasets
###'
###'

### Student compositions + Teacher compositions 2003-2017
setwd(work_dir)
load(file = "processed_data/df_Teacher_Student_K12_character.rda")
df <- df_to_save


###' District information on actual/predicted per-pupil expenditures
###' LCFF-induced exogenous increases in district per-pupil expenditures
load(file = "processed_data/LCFF_induced_exogenous_PPE_2003_2016.rda")
df_pred <- df_to_save



###'######################################################################
###'
###' Loop over outcome variables & quantile levels
###'
###'

### Outcome variables
y_names_vec <- c("PCT_Teacher", "PCT_Admin", "PCT_Pupil_Serv", 
                 "PCT_female", "PCT_White", "PCT_Latino", "PCT_Black", "PCT_Asian", 
                 "PCT_Master_Plus", "PCT_Bachelor_Minus", 
                 "mean_yrs_teach", "mean_yrs_in_dist", 
                 "PCT_New_teach", "PCT_New_in_dist",   
                 "PCT_Longterm_Sub",  "PCT_Probationary", 
                 "PCT_Tenured", "PCT_Other", 
                 "ST_Ratio", "SS_Ratio", 
                 "PCT_Reclass", "PCT_Dropout_Total", "PCT_UC_GRADS", 
                 "TA_Rate_Susp", "TA_Rate_Exps")

y_labels_vec <- c("Percentage of Teachers", 
                  "Percentage of Administrators", 
                  "Percentage of Pupil Services Staffs", 
                  "Percentage of Female Teachers", 
                  "Percentage of White Teachers", 
                  "Percentage of Hispanic or Latino Teachers", 
                  "Percentage of African American Teachers", 
                  "Percentage of Asian Teachers", 
                  "Percentage of Teachers with more than a Masters degree", 
                  "Percentage of Teachers with less than Bachelors degree", 
                  "Total years of teaching experience (school mean)", 
                  "Total years of service in the district (school mean)", 
                  "Percentage of novice teachers", 
                  "Percentage of teachers new in the district", 
                  "Percentage of longterm substitute or temporary employee", 
                  "Percentage of probationary teachers", 
                  "Percentage of tenured teachers", 
                  "Percentage of teachers with other employment status", 
                  "Student teacher ratio", 
                  "Student staff ratio", 
                  "EL Reclassification Rate", 
                  "Dropout Rate", 
                  "Percentage of graduated meeting A-G requirements", 
                  "Suspension Rate", 
                  "Expulsion Rate")

all.equal(length(y_names_vec), length(y_labels_vec))


### Quantile levels & Elements of estimates (Mean, tau20, tau50, etc)
quantile_vec <- c(0.2, 0.5, 0.8)
element_vec <- c(paste0("tau", quantile_vec*100), "mean")


for (i in seq_along(y_names_vec)){
  
  ###'######################################################################
  ###'
  ###' STEP 1. Calculate (un)conditional means and quantiles
  ###' 
  ###' (1) Data preparation
  ###'
  ###'
  
  ### Assign the name and label of outcome variable
  y_name <- y_names_vec[i]
  y_label <- y_labels_vec[i]
  
  
  ### Create folder to save results & set working directory
  folder_name <- paste0(sprintf("%02d", i), "_", y_label)
  
  folder_dir <- file.path(work_dir, "figures", 
                          "17_Mean and Quantile Effects of LCFF-induced Increases_01_All Schools", 
                          folder_name)
                          
  dir.create(folder_dir, showWarnings = FALSE)
  setwd(folder_dir)
  
  
  ### Rename the outcome variable in the copied dataset
  df_temp <- df
  names(df_temp)
  names(df_temp)[names(df_temp) %in% y_name] <- "yvar"
  
  
  ### Recode Inf/NaN (Infinite number or Not a number) into NA (Not available)
  df_temp$yvar[is.infinite(df_temp$yvar)] <- NA
  df_temp$yvar[is.nan(df_temp$yvar)] <- NA
  
  
  ### Nest dataset by District and Year
  df_nested <- df_temp %>%
    group_by(CountyCode, DistrictCode, AcademicYear) %>%
    nest()
  
  head(df_nested)
  
  
  ### Generate dataframe summarizing yvar's distribution for district-by-year cells
  df_Nschools <- df_temp %>%
    group_by(CountyCode, DistrictCode, AcademicYear) %>%
    summarise(DistrictName = first(DistrictName), 
              DOC = first(DOC), 
              N_schools = n_distinct(SchoolCode),
              N_ymiss = sum(!complete.cases(yvar)), 
              yvar_min = min(yvar, na.rm = TRUE), 
              yvar_mean = mean(yvar, na.rm = TRUE), 
              yvar_max = max(yvar, na.rm = TRUE), 
              cond_skip = N_schools == N_ymiss) 
  
  

  ###'######################################################################
  ###'
  ###' STEP 1. Calculate (un)conditional means and quantiles
  ###' 
  ###' (2) Calculate MEAN Effects: 
  ###' 
  ###'     => 2-1. Fit ordinary linear regression model
  ###'
  ###'
  
  ### Map ordinary linear regression model for each district
  
  safe_lm <- safely(lm)   # define a wrapped function for lm()
  
  model_mean <- df_nested %>%
    mutate(model = map(.x = df_nested$data, 
                       .f = ~safe_lm(formula = yvar ~ 1, 
                       data = .x)), 
           model_result = map(model, "result"), 
           model_error = map(model, "error"), 
           idx_error = map(model_error, is.null), 
           idx_error = !simplify(idx_error))
  
  head(model_mean)
  
  
  ###' Extract the coefficients and fit statistics of each model 
  ###' into nested dataframes
  df_result_mean <- model_mean %>%
    mutate(coef = map(model_result, ~ tidy(.x)), 
           fit = map(model_result, ~ glance(.x))) %>%
    unnest(coef, fit)

  tabdf(df_result_mean, idx_error)
  
  
  
  ###'######################################################################
  ###'
  ###' STEP 1. Calculate (un)conditional means and quantiles
  ###' 
  ###' (2) Calculate MEAN Effects: 
  ###' 
  ###'    => 2-2. Which district-by-year cell did return errors?
  ###'
  ###'
  
  ### Create folder to save error-related results
  folder_error <- file.path(folder_dir, "Error Cells") 
  dir.create(folder_error, showWarnings = FALSE)
  setwd(folder_error)
  
  
  ### Save the number/percent of cells returned error
  write.csv(tabdf(model_mean, idx_error), 
            paste0("Table_01_", "Error_Count_", "Mean_Model", ".csv"))
  
  
  ### Extract dataframe for error vs. results
  df_error <- model_mean %>%
    dplyr::select(ends_with("Code"), AcademicYear, idx_error) 
  
  
  ### Generate district by year summary and merge with df_error
  df_Nschools_error <- df_Nschools %>%
    left_join(df_error, by = c("CountyCode", "DistrictCode", "AcademicYear"))
  
  write.csv(df_Nschools_error, 
            file = paste0("Data_01_", 
            "Error for each district-by-year-cell_", 
            "Mean_Model", ".csv"))
  
  
  ### Generate tables tabulating Error vs. (N_schools, AcademicYear, DOC)
  tab_factor_vec <- c("N_schools", "AcademicYear", "DOC")
  
  for (tf in seq_along(tab_factor_vec)){
    
    # Assign tabulating factor
    temptf <- df_Nschools_error
    names(temptf)[names(temptf) == tab_factor_vec[tf]] <- c("tab_factor")
    
    # Generate and save a table
    tbl_temp <- with(temptf, table(tab_factor, idx_error)) %>%
      as.data.frame() %>%
      arrange(tab_factor, idx_error) %>%
      group_percent(value = Freq, tab_factor)
    
    write.csv(tbl_temp, file = paste0("Table_02_", "Mean_Model_", "Error vs. ", 
                                      tab_factor_vec[tf], ".csv"))
    
    # Generate and save a plot for AcademicYear and DOC
    if (tf %in% c(2, 3)){
      
      p <- ggplot(tbl_temp, 
                  aes(x = factor(tab_factor), y = Freq, fill = idx_error)) +
        geom_bar(position = position_stack(), stat = "identity", width = 0.7) +
        geom_text(aes(label = label_text), position = position_stack(vjust = 0.5), size = 2) 
      
      p <- p + theme_trend +
        labs(title = "Percent of Errors among District-by-Year Cells", 
             subtitle = paste0("By ", tab_factor_vec[tf], " for ", y_name), 
             caption = NULL, 
             y = "Frequency",  
             x = tab_factor_vec[tf]) +
        coord_flip()
      
      ggsave(paste0("Table_02_", "Error vs. ", tab_factor_vec[tf], 
                    "_Mean_Model", ".pdf"), 
             p, width = 7, height = 5)
      
    }
  }
  
  
  
  ###'######################################################################
  ###'
  ###' STEP 1. Calculate (un)conditional means and quantiles
  ###' 
  ###' (3) Calculate QUANTILE Effects: 
  ###' 
  ###'    => 3-1. Fit single-level linear quantile regression model
  ###'    => 3-2. Which district-by-year cell did return errors?
  ###'
  ###'
  
  list_result_quantile <- list()
  
  for (j in seq_along(quantile_vec)){
    
    ### Map linear quantile regression model for each district
    
    safe_rq <- safely(rq)   # define a wrapped function for lm()
    
    model_quantile <- df_nested %>%
      mutate(model = map(.x = df_nested$data, 
                         .f = ~safe_rq(formula = yvar ~ 1, 
                                       data = .x, 
                                       tau = quantile_vec[j])), 
             model_result = map(model, "result"), 
             model_error = map(model, "error"), 
             idx_error = map(model_error, is.null), 
             idx_error = !simplify(idx_error))
    
    
    ###' Extract the coefficients and fit statistics of each model 
    df_result_quantile <- model_quantile %>%
      mutate(coef = map(model_result, ~ tidy(.x)), 
             fit = map(model_result, ~ glance(.x))) %>%
      unnest(coef, fit)
    
    
    ### Collect in the prepared list
    list_result_quantile[[j]] <- df_result_quantile
    
    
    ### Save the number/percent of cells returned error
    setwd(folder_error)
    write.csv(tabdf(model_quantile, idx_error), 
              paste0("Table_01_", "Error_Count_", 
                     "Quantile_Model_", 100*quantile_vec[j], "_",  
                     ".csv"))
    
    
    ### Extract dataframe for error vs. results
    df_error <- model_quantile %>%
      dplyr::select(ends_with("Code"), AcademicYear, idx_error) 
    
    
    ### Generate district by year summary and merge with df_error
    df_Nschools_error <- df_Nschools %>%
      left_join(df_error, by = c("CountyCode", "DistrictCode", "AcademicYear"))
    
    setwd(folder_error)
    write.csv(df_Nschools_error, 
              file = paste0("Data_01_",  
                            "Error for each district-by-year-cell", "_",  
                            "Quantile_Model_", 100*quantile_vec[j], ".csv"))
    
    
    ### Generate tables tabulating Error vs. (N_schools, AcademicYear, DOC)
    tab_factor_vec <- c("N_schools", "AcademicYear", "DOC")
    
    for (tf in seq_along(tab_factor_vec)){
      
      # Assign tabulating factor
      temptf <- df_Nschools_error
      names(temptf)[names(temptf) == tab_factor_vec[tf]] <- c("tab_factor")
      
      # Generate and save a table
      tbl_temp <- with(temptf, table(tab_factor, idx_error)) %>%
        as.data.frame() %>%
        arrange(tab_factor, idx_error) %>%
        group_percent(value = Freq, tab_factor)
      
      write.csv(tbl_temp, file = paste0("Table_02_", 
                                        "Quantile_Model_", 100*quantile_vec[j], "_",
                                        "Error vs. ", tab_factor_vec[tf], ".csv"))
      
      # Generate and save a plot for AcademicYear and DOC
      if (tf %in% c(2, 3)){
        
        p <- ggplot(tbl_temp, 
                    aes(x = factor(tab_factor), y = Freq, fill = idx_error)) +
          geom_bar(position = position_stack(), stat = "identity", width = 0.7) +
          geom_text(aes(label = label_text), position = position_stack(vjust = 0.5), size = 2) 
        
        p <- p + theme_trend +
          labs(title = "Percent of Errors among District-by-Year Cells", 
               subtitle = paste0("By ", tab_factor_vec[tf], " for ", y_name), 
               caption = NULL, 
               y = "Frequency",  
               x = tab_factor_vec[tf]) +
          coord_flip()
        
        ggsave(paste0("Table_02_",
                      "Error vs. ", tab_factor_vec[tf], 
                      "_Quantile_Model_", 100*quantile_vec[j],
                      ".pdf"), 
               p, width = 7, height = 5)
        
      }
    }
  }
  
  ### Bind lists across quantile levels
  df_result_quantile <- bind_rows(list_result_quantile) %>%
    arrange(CountyCode, DistrictCode, AcademicYear, term, tau)
  
  
  
  ###'######################################################################
  ###'
  ###' STEP 2. Group-level 2SLS estimation
  ###' 
  ###' (0) Prepare loop over different reference years: 2012 vs. 2013
  ###'
  ###'
  
  ref_year_vec <- c(2012, 2013)
  
  for (yr in seq_along(ref_year_vec)){
    
    ### Assign reference year
    ref_year <- ref_year_vec[yr]
    
    
    ###'######################################################################
    ###'
    ###' STEP 2. Group-level 2SLS estimation
    ###' 
    ###' (1) Data preparation: Predictors
    ###'
    ###'
    
    ### Generate matching keys
    names(df_pred)
    df_pred <- df_pred %>% 
      mutate(DistrictCode = as.numeric(Dcode), 
             AcademicYear = as.numeric(as.character(Fiscalyear)) - 2000)
    
    
    ###' Choose factors used as a predictor (among 60 factors)
    ###' Start with a small number of predictors (9 categories)
    
    tabdf(df_pred, factor)
    
    pred_factor_vec <- c("Total Expenditures", 
                         "Student Spending", 
                         "Salaries", 
                         "Employee Benefits", 
                         "Services & Other Operating Expenditures", 
                         "General Education, K-12", 
                         "General Education Instruction", 
                         "Instruction-related Services", 
                         "Pupil Services")
    
    df_pred_sub <- df_pred %>%
      filter(factor %in% pred_factor_vec)
    
    
    ### Convert to log and $1000 scales: .fitted and pred_mod2
    df_pred_sub <- df_pred_sub %>%
      mutate(PPE_log = if_else(.fitted <= 1, 0, log(.fitted), missing = NA_real_),  
             trend_log = if_else(pred_mod2 <= 1, 0, log(pred_mod2), missing = NA_real_), 
             PPE_1000 = .fitted/1000, 
             trend_1000 = pred_mod2/1000)
    
    
    ### Generate the releveled "year" factor variables
    df_pred_sub <- df_pred_sub %>%
      mutate(year = relevel(Fiscalyear, as.character(ref_year)))
    
    
    
    ###'######################################################################
    ###'
    ###' STEP 2. Group-level 2SLS estimation
    ###' 
    ###' (2) Merge predictors with estimated means / quantiles
    ###' (3) Fit linear regression model: Add Sandwich estimator later
    ###' (4) Get event study estimates
    ###'     Get coefficients and their 95% confidence intervals of  
    ###'     the linear combinations of log_PPE and its interactions with year dummies
    ###'     => Nonparametric effect estimates of log_PPE for each year
    ###'
    ###'
    
    ### Prepare a list containing results from mean and each quantile effect
    element_vec <- c(paste0("tau", quantile_vec*100), "mean")
    list_result_quantile[[length(quantile_vec) + 1]] <- df_result_mean
    list_result <- list_result_quantile
    names(list_result) <- element_vec
    
    
    ### Prepare empty lists to collect the Step 2 estimates
    list_model_coef <- list()   # collect model estimates
    list_event_est <- list()    # collect event study estimates
    
    
    ### Start a loop
    for (k in seq_along(element_vec)){
      
      ###'########################################################
      ###'(2) Merge predictors with estimated means / quantiles
      ###'
      
      ### Extract dataframe
      df_yest <- list_result[[k]]
      
      
      ###' Join dataframe with predictors (df_pred_sub) with 
      ###' the dataframe with df_result_mean and df_result_quantile (df_yest)
      df_step2 <- df_pred_sub %>%
        left_join(df_yest, by = c("DistrictCode", "AcademicYear"))
      
      
      ### Nest dataframe by factor
      df_step2_nested <- df_step2 %>%
        group_by(factor) %>%
        nest()
      
      head(df_step2_nested, length(df_step2_nested$factor))
      
      
      ###'####################################
      ###' (3) Fit linear regression model
      ###'
      
      ### Map linear regression model for each factor
      fit_step2 <- df_step2_nested %>%
        mutate(model = map(.x = df_step2_nested$data, 
                           .f = ~lm(formula = estimate ~ PPE_log*year + trend_log, 
                                    data = .x)))
      
      
      ### Extract the coefficients of each model into nested dataframes
      model_coef <- fit_step2 %>%
        mutate(coef = map(model, ~tidy(.x))) %>%
        unnest(coef)
      
      # write.csv(model_coef, file = "tables/coef_example.csv")
      
      
      ### Collect the dataframe in the list
      model_coef <- model_coef %>%
        mutate(element = element_vec[k]) %>%
        dplyr::select(element, everything())
      
      list_model_coef[[k]] <- model_coef
      
      
      ###'###################################
      ###' (4) Get event study estimates
      ###'
      
      ### Check fitted models
      head(fit_step2)
      
      
      ### Prepare a specification of the linear hypotheses to be tested.
      coef_terms <- unique(model_coef$term)
      coef_terms_inter <- coef_terms[grepl(":", coef_terms)]
      hypotheses <- paste0("PPE_log + ", coef_terms_inter, " = 0")
      # dput(hypotheses, "hypotheses.txt")
      
      
      ### Get the estimates and their 95% confidence intervals
      library(multcomp)
      
      df_est <- fit_step2 %>%
        mutate(lincom = map(model, confint)) %>%
        mutate(lincom_test = map(model, function(x) glht(x, linfct = hypotheses))) %>%
        mutate(lincom_test_result = map(lincom_test, ~tidy(confint(.x)))) %>%
        unnest(lincom_test_result)
      
      detach("package:multcomp", unload=TRUE)
      
      
      ### Tidy up and save the resulting estimates
      names(df_est)
      df_est_tidy <- df_est %>%
        mutate(year = as.numeric(substr(lhs, nchar(lhs) - 4 + 1, nchar(lhs)))) %>%
        dplyr::select(-rhs, -lhs) %>%
        dplyr::select(factor, year, estimate, conf.low, conf.high)
      
      
      ### Bind reference year data
      year_vec <- rep(ref_year, length(pred_factor_vec))
      
      fill_zero <- matrix(data = 0, 
                          nrow = length(pred_factor_vec), 
                          ncol = length(c("estimate", "conf.low", "conf.high")))
      
      df_anchor <- cbind.data.frame(pred_factor_vec, year_vec, fill_zero)
      
      names(df_anchor) <- names(df_est_tidy)
      
      df_temp <- bind_rows(df_est_tidy, df_anchor) %>% 
        arrange(factor, year)
      
      
      ### Define the order of factor level
      idx <- levels(df_est_tidy$factor) %in% df_temp$factor
      level_vec <- levels(df_est_tidy$factor)[idx]
      
      df_event_est <- df_temp %>%
        mutate(factor = factor(factor, levels = level_vec), 
               element = element_vec[k]) %>%
        arrange(factor) %>%
        dplyr::select(factor, element, year, everything())
      
      
      ### Collect the dataframe in the list
      list_event_est[[k]] <- df_event_est
      
    } ### End of Loop over elements
    
    
    ### Bind rows: (1) model coeffients
    df_model_coef <- bind_rows(list_model_coef) %>%
      mutate(element = factor(element, levels = element_vec)) %>%
      arrange(factor, term, element)
    
    
    ### Bind rows: (2) the event study estimates
    df_event_est <- bind_rows(list_event_est) %>%
      mutate(element = factor(element, levels = element_vec)) %>%
      arrange(factor, element, year)
    
    
    ### Save the resulting estimates
    setwd(folder_dir)
    
    write.csv(df_model_coef, 
              file = paste0("01_Model coefficients_", 
                            sprintf("%02d", i), "_", y_name, 
                            "_", ref_year, ".csv"))
    
    write.csv(df_event_est, 
              file = paste0("02_Event study estimates_", 
                            sprintf("%02d", i), "_", y_name, 
                            "_", ref_year, ".csv"))
    
    
    
    ###'######################################################################
    ###'
    ###' STEP 2. Group-level 2SLS estimation
    ###' 
    ###' Plot event study estimates
    ###'
    ###'
    
    level_vec <- levels(df_event_est$factor)
    
    for (l in seq_along(level_vec)){
      
      ### Extract only the subset of data
      factor_name <- level_vec[l]
      
      df_sub <- df_event_est %>%
        filter(factor == factor_name)
      
      
      ### Plot!
      p <- ggplot(aes(x = year, y = estimate), 
                  data = df_sub) +
        
        # Add point, line, and text for coefficients
        geom_line(size = 0.5) + 
        geom_point(size = 1.5) + 
        geom_text(aes(label = round(estimate, 2), hjust = 0.5, vjust = 2.0), size = 4) +
        
        # Add confidence interval band
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +
        
        # Add horizontal line at zero
        geom_hline(aes(yintercept = 0), color = "black", linetype = "dashed") + 
        
        # Add vertical line at event
        geom_vline(aes(xintercept = ref_year), color = "black", linetype = "dashed") + 
        
        # Facet by element
        facet_wrap(~element) + 
        
        # Scales
        scale_x_continuous(breaks = seq(2003, 2016, by = 1)) + 
        scale_y_continuous(labels = comma) + 
        
        # Theme
        theme_bw() + 
        theme(panel.background = element_blank(),
              panel.grid = element_blank(), 
              legend.position = "bottom", 
              legend.direction = "horizontal", 
              legend.title = element_blank()) + 
        
        # Labels
        labs(title = paste0("Effects of LCFF-induced 1% Increase in ", factor_name), 
             subtitle = paste0("On ", y_label), 
             caption = NULL, 
             y = paste0("Change in ", y_label),  
             x = "Academic Year")
      
      # print(p)
      
      
      ### Save the resulting plot
      setwd(folder_dir)
      ggsave(paste("Figure",  
                   sprintf("%02d", i), y_name, 
                   sprintf("%02d", l), factor_name, 
                   ref_year, 
                   ".pdf", sep = "_"), 
             p, width = 11, height = 9)
      
      
    }  ### End of loop over different funding categories
    
  }  ### End of loop over different reference years
  
  
  
  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("Outcome Variable: ", 
             sprintf("%02d", i), "_", y_label, 
             " => completed", "\n"))
  
  
  
  ###'######################################################################
  ###' 
  ###' End for loop
  ###' 
  
}

