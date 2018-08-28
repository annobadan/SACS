
###'######################################################################
###'
###' Model Estimation
###' 
###' (1) Prediction models for the per-pupil expenditures/revenues
###' 
###'     => first-stage model of IV
###'     
###'     
###' Replicate/evaluate the Johnson & Tanner's research design
###' 
###' (1) Event study (interrupted time series)
###' 
###' 
###' (2) A simulated instrumental variable approach 
###'     using the LCFF funding formula
###' 
###' 
###' 20180813 JoonHo Lee
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
data_dir <- c("D:/Data/LCFF")


### Call libraries
library(tidyverse)
library(car)
library(plm)
library(lme4)
library(sjPlot)


### Call functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Import pre-cleaned datasets
###'
###'

### Per-pupil expenditures & revenues
load(file = "processed_data/list_expenditures_def_1_2.RData")
load(file = "processed_data/list_revenues.rda")


### LCFF funding snapshot data: FY13-14, 14-15, 15-16
load(file = "processed_data/funding_snapshot_3year.RData")


### The State of California's overall and local revenues/expenditures data
state_df <- read.csv(file = "processed_data/May_Revision_Budget_2000_to_2016.csv")



###'######################################################################
###'
###' Construct the simulated instrumental variable (Z_{d})
###' 
###' (1) Per-pupil supplemental grant: Supp_{d} = Base_{d} * 0.2 * UPP
###' 
###' (2) Per-pupil concentration grant: Conc_{d} = Base_{d} * max[UPP - 0.55, 0]
###' 
###' (3) Simulated instrumental variable: SimIV_{d} = Supp_{d} + Conc_{d} 
###' 
###' => The state's allocation of Supplemental and Concentration Grants is
###'    the focal point of our use of the funding formula 
###'    to isolate exogenous changes in district-level revenue 
###'    caused by the state policy change
###'
###' (4) Formula weight: 0.20 * UPP + max[UPP - 0.55, 0]
###'
###'
###' *** The meaning of the simulated IV
###' => The reform-induced changes in district spending
###' => Supplemental and concentration grant
###' 
###' Importantly, these reform-induced changes in district spending, 
###' which are credibly identified from the funding formula 
###' (and which serve as the instrumental variables), 
###' are unrelated to changes in child family and neighborhood characteristics 
###' conditional on the baseline level of disadvantage in each district.
###' (After controlling for UPP => Conditional exogeneity)
###'
###'

### Remove charter school entries
df <- fund1314 %>%
  filter(LEA_type == "School District")


### Generate simulated IV and formula weight
df <- df %>%
  mutate(formula_weight = 0.2 * Unduplicated + 
           ifelse(Unduplicated > 0.55, 0.5 * (Unduplicated - 0.55), 0), 
         SimSupp = Base * 0.2 * Unduplicated, 
         SimConc = ifelse(Unduplicated > 0.55, 0.5 * Base * (Unduplicated - 0.55), 0),
         SimIV = SimSupp + SimConc, 
         Supp_Conc = Supplemental + Concentration) %>%
  select(CountyCode, DistrictCode, LEA, Total_ADA, Unduplicated, formula_weight,  
         Base, Supplemental, SimSupp, Concentration, SimConc, Supp_Conc, SimIV, 
         everything())


### Round the simulated values for the visual comparison
cols_to_round <- which(names(df) %in% c("Base", "SimSupp", "SimConc", "SimIV"))

df <- df %>% 
  mutate_at(cols_to_round, round, 0)


### Are the simulated values consistent with the provided values?
df_nonzeroBase <- df %>% 
  filter(Base != 0)

all.equal(df_nonzeroBase$SimSupp, df_nonzeroBase$Supplemental)
all.equal(df_nonzeroBase$SimConc, df_nonzeroBase$Concentration)
all.equal(df_nonzeroBase$SimIV, df_nonzeroBase$Supp_Conc)


### Check the distribution of funding formula weights
ggplot(df, aes(formula_weight)) +
  geom_density()


### Generate decile dummies for simulated IV

df <- df %>%
  mutate(decile = ntile(SimIV, 10))

table(df$decile)



###'######################################################################
###'
###' Merge FY1314 funding snapshot to per-pupil expenditure panel data
###' 
###' 

### Extract dataframe
assign(names(list_expenditures_def_1)[[1]], list_expenditures_def_1[[1]])


### Rename CountyCode and DistrictCode and convert them to numeric
df <- df %>%
  mutate(Ccode = as.numeric(CountyCode), 
         Dcode = as.numeric(DistrictCode)) %>%
  select(Ccode, Dcode, everything()) %>%
  select(-(CountyCode:DistrictCode))


### Merge the two datasets
total_exp <- left_join(total_exp, df, by = c("Ccode", "Dcode"))


### Log transformation of outcome variables
total_exp <- total_exp %>%
  mutate(log_y = if_else(sum_value_PP_16 == 0, 0, log(sum_value_PP_16)))



###'######################################################################
###'
###' Fitting piecewise growth curve models:
###' 
###' => Provide visual evidence that the funding formula weight 
###' 
###'    does not predict changes in funding levels in the previous years
###' 
###'    leading up to the LCFF policy change in 2013
###'
###' Level 1: 
###' Y_{ij} = beta_{0j} + beta_{1j} time_1 + beta_{2j} time_2 + r_{ij}
###'
###' Level 2:
###' beta_{0j} = gamma_{00} + gamma_{01} formula_weight_{j} + U_{0j}
###' beta_{1j} = gamma_{10} + gamma_{11} formula_weight_{j} + U_{1j}
###' beta_{2j} = gamma_{20} + gamma_{21} formula_weight_{j} + U_{2j}
###'
###' Time coding scheme:
###' time_1: the pre-recession housing bubble (2003-2006) 
###' time_2: the housing crash and ensuing recession (2007-2009 + 2009-2012)
###' time_3: LCFF years (2013-2016)
###'
###' time_1: c(0, 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)
###' time_2: c(0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 6, 6, 6, 6)
###' time_3: c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4)
###'
###'

### Initial exploratory plot
df_plot <- total_exp %>%
  filter(Dtype %in% c("ELEMENTARY", "HIGH", "UNIFIED"))
  
p <- ggplot(aes(x = Fiscalyear, y = log_y), data = df_plot) +
  geom_line(aes(group = Dcode), color = "gray") + 
  geom_smooth(aes(group = 1), size = 3, color = "red", se = FALSE) +
  facet_grid(.~ Dtype) + 
  theme_trend + 
  scale_x_continuous(breaks = seq(min(df_plot$Fiscalyear), 
                                  max(df_plot$Fiscalyear), 
                                  by = 1))

ggsave("figures/total_exp_growth.pdf", p, width = 15, height = 8)


### Generate time variables for piecewise regression
Fiscalyear <- 2003:2016
time_1 <- c(0, 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)
time_2 <- c(0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 6, 6, 6, 6)
time_3 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4)
time_df <- data.frame(Fiscalyear, time_1, time_2, time_3)

total_exp <- left_join(total_exp, time_df, by = c("Fiscalyear")) 


### Fit the piecewise growth curve model

fit_piece <- lmer(log_y ~ (time_1 + time_2 + time_3) * formula_weight + 
                    (time_1 + time_2 + time_3 | Dcode), 
                  data = total_exp, REML = FALSE)

summary(fit_piece)


### Graph fitted curves

FittedFE <- function(x) model.matrix(x) %*% fixef(x)

df_plot_fit <- data.frame(fit_piece@frame, fitted = FittedFE(fit_piece))

df_plot_fit <- df_plot_fit %>% arrange(Dcode)

df_plot_fit <- left_join(df_plot_fit, time_df, by = c("time_1", "time_2", "time_3"))

p <- ggplot(df_plot_fit, aes(x = Fiscalyear, y = log_y)) + 
  # geom_point(shape = 19) +
  stat_summary(fun.y = mean, geom = "point", size = 5, shape = 1) + 
  geom_line(aes(y = fitted), lwd = 1.5)



###'######################################################################
###'
###' Per-pupil revenue prediction model
###'
###'

### Outcome variable: per-pupil revenue across years 2003-2016
total_rev <- list_revenues[["total_rev"]]


### Total state-level ADA across years 2003-2016
state_ADA <- total_exp %>%
  group_by(Fiscalyear) %>%
  summarise(stateADA = sum(K12ADA_C, na.rm = TRUE)) %>%
  filter(!is.na(Fiscalyear))
  

###' Generate the two state-level predictors
###' (1) CAExp: the total non K-12 state expenditures per pupil for birth cohort
###' (2) CALocalAssist: the state local assistance provided (excluding education)

state_operations_df <- state_df %>%
  filter(Category_1 == "State Operations") %>%
  filter(!(Category_2 %in% c("Education"))) %>%
  group_by(Fiscalyear) %>%
  summarise(CAExp = sum(value_16, na.rm = TRUE)) %>%
  left_join(state_ADA, by = c("Fiscalyear")) %>%
  mutate(CAExp_PP = CAExp/stateADA)

local_assistance_df <- state_df %>%
  filter(Category_1 == "Local Assistance") %>%
  filter(!(Category_2 %in% c("Public Schools - K-12"))) %>%
  group_by(Fiscalyear) %>%
  summarise(CALocalAssist = sum(value_16, na.rm = TRUE)) %>%
  left_join(state_ADA, by = c("Fiscalyear")) %>%
  mutate(CALocalAssist_PP = CALocalAssist/stateADA)

state_predictors <- left_join(state_operations_df, local_assistance_df, 
                              by = c("Fiscalyear"))


### Merge predictors to dataset with outcome variable
df_rev_pred <- total_rev %>%
  left_join(state_predictors, by = c("Fiscalyear"))

df_rev_pred <- df_rev_pred %>%
  left_join(df, by = c("Ccode", "Dcode")) %>%
  mutate(timetrend = Fiscalyear - 2003)


### Fit the regression model
















###'######################################################################
###'
###' The first stage model predicting average per-pupil spending
###' 
###' in district "d" in year "t"
###' 
###' - (1) event time dummies
###' - (2) formula weight dummies Q_{2013}  
###' 
###' => The coefficients for the two-way interactions map out
###'    the dynamic treatment effects of the LCFF reform on per-pupil spending
###'    for districts in spending quantile Q_{2013}
###' 
###' - (3) district fixed-effect
###'
###'

### Convert to factors and set reference category
df <- total_exp

df$Fiscalyear <- factor(df$Fiscalyear)
df$decile <- factor(df$decile)
df$Fiscalyear <- relevel(df$Fiscalyear, "2013")


### Tidy up analytical sample
df <- total_exp %>%
  filter(Dtype %in% c("ELEMENTARY", "HIGH", "UNIFIED")) %>%
  select(log_y, decile, Fiscalyear, Dcode) 


### Filter only complete cases
df <- df[complete.cases(df), ]


### Fixed effects using least squares dummy variable model

df$Fiscalyear <- factor(df$Fiscalyear)
df$decile <- factor(df$decile)
df$Fiscalyear <- relevel(df$Fiscalyear, "2013")

mm <- model.matrix(~ decile*Fiscalyear, df)
write.csv(mm, file = "tables/model_matrix.csv")

fit_lm <-lm(log_y ~ mm + factor(Dcode), data = df)
summary(fit_lm)


### Plot dynamic treatment effects

coef <- data.frame(fit_lm$coefficients)

coef$coef <- rownames(coef)

rownames(coef) <- NULL

coef_int <- coef[grepl(":", coef$coef), ]

coef_int <- coef_int %>%
  separate(coef, c("decile", "year"), ":") %>%
  select(decile, year, fit_lm.coefficients) %>%
  rename(coef = fit_lm.coefficients) %>% 
  arrange(decile, year)
  
coef_int$year <- as.numeric(substr(coef_int$year, 11, 14))


coef_int$decile <- factor(gsub('\\D+','', coef_int$decile), 
                          levels = rev(as.character(2:10)))
coef_int$coef <- round(coef_int$coef, 2)


coef_2013 <- data.frame(decile = as.character(2:10), 
                        year = rep(2013, 9), 
                        coef = rep(0, 9))

coef_int <- coef_int %>%
  bind_rows(coef_2013) %>%
  arrange(decile, year)


p1 <- plot_trend_grp(coef_int, year, coef, decile, yline = 2013, 
                     ylim = auto_ylim(coef_int$coef))

labels <- labs(title = "Effects of LCFF funding formula on per-pupil expenditure",
               subtitle = "Definition 1, Inflation-adjusted by CPI-U deflator", 
               x = "Fiscal Year",  
               y = "Difference in district per-pupil expenditures compared to 2013 & Decile 1") 

p1 <- p1 + labels

p1


















