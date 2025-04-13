

# ==== 1. PREPARE WORKSPACE ====
rm(list = ls())
options(scipen = 999)


# ==== 2. LOAD LIBRARIES ====
library(openxlsx)
library(dplyr)
library(tseries)
library(astsa)
library(zoo)
library(leaps)
library(forecast)
library(lmtest)
library(purrr) # for map_dfc used in getting the lags into one dataframe
library(stringr) # for str_remove_all used in removing duplicates
library(imputeTS)


# ==== 3. PREPARE WORKING DIRECTORY ====
overlay_loc <- "D:/John Adrian Files/Files/Projects/Final Compilation - NPL Ratio Forecasting Project"
setwd(overlay_loc)


# ==== 4. IMPORT INPUT FILES ====
setwd(paste0(overlay_loc, "\\OUTPUT"))

# Historical NPL Ratio and List of Macroeconomic Indicators 
variables_final <- read.csv("NPL Prediction - Variable List (Monthly).csv")


# ==== 5. INPUT PARAMETERS ====

# ---- Input number of observations in test data ----
no_of_test <- 24
no_of_train <- nrow(variables_final) - no_of_test


# ==== 6. APPLY DATA TRANSFORMATIONS + ADD DUMMY VARIABLE FOR OUTLIERS ====

# ---- CREATE CUSTOM FUNCTIONS ----

# LOG TRANSFORMATION; QUARTERLY/FIRST DIFFERENCING
log_diff <- function(ts_data, no_of_lags) {
  transformed_ts <- diff(log(ts_data), lag = no_of_lags)
  
  return(c(rep(NA, times = no_of_lags),
           transformed_ts))
}

# ASINH TRANSFORMATION; QUARTERLY/FIRST DIFFERENCING
asinh_diff <- function(ts_data, no_of_lags) {
  transformed_ts <- diff(asinh(ts_data), lag = no_of_lags)
  
  return(c(rep(NA, times = no_of_lags),
           transformed_ts))
}

# ---- Apply Log Transform & First Difference ----
log_diff_df <- variables_final %>%
  select(starts_with("CPI"), PPI.2018,
         Bank.Ave.Lend.Rate, Interest.Rate) %>%
  mutate_all(function(x) log_diff(x, 1))
colnames(log_diff_df) <- paste0(names(log_diff_df), ".ln.diff")

# ---- Apply Log Transform & Quarterly Seasonal Difference ----
log_diff_quarterly_df <- variables_final %>%
  select(NPL.Ratio, starts_with("GDP"), contains("Crude.Oil"), 
         PHP.USD.rate, Gross.Capital.Formation, 
         Government.Expenditure, Household.Consumption, 
         Unemployment, LaborForce.ParRate, starts_with("RPI"),
         starts_with("WPI")) %>%
  mutate_all(function(x) log_diff(x, 3))
colnames(log_diff_quarterly_df) <- paste0(names(log_diff_quarterly_df), ".ln.diff_3")

# ---- Apply Asinh Transform & Quarterly Seasonal Difference ----
asinh_diff_quarterly_df <- variables_final %>%
  select(starts_with("Inf"), CCI, BCI) %>%
  mutate_all(function(x) asinh_diff(x, 3))
colnames(asinh_diff_quarterly_df) <- paste0(names(asinh_diff_quarterly_df), ".asinh.diff_3")

# ---- Consolidate Data Transformed Files Together ----
data_transformations <- cbind(
  log_diff_df, log_diff_quarterly_df, asinh_diff_quarterly_df
)

rm(log_diff_df, log_diff_quarterly_df, asinh_diff_quarterly_df)


# ==== 7. DETERMINE AUTOCORRELATED LAGS FROM PREDICTORS ==== 

# ---- CREATE CUSTOM FUNCTIONS ----

# Custom function to determine significant lags
check_significant_lags <- function(df_file, var) {
  vars_to_check <- df_file %>%
    pull(var) %>%
    na.omit()
  
  check_pacf <- pacf(vars_to_check)
  autocorrelation <- check_pacf[['acf']][1:12]
  
  significant_lags <- index(autocorrelation)[which(abs(autocorrelation) > .3)]
  
  list_of_significant <- paste(significant_lags, collapse = ',')
  
  return(list_of_significant)
  
}

# Custom function to retrieve lagged variables
retrieve_lags <- function(var_name) {
  relevant_lags <- with_signifact_lags %>%
    filter(VARS == var_name) %>%
    pull(RELEVANT_LAGS)
  
  original_indices <- as.numeric(unlist(strsplit(relevant_lags, ",")))
  
  lagged_variants <- map_dfc(original_indices, function(lag_n) {
    lag(data_transformations[var_name], lag_n)
  })
  
  colnames(lagged_variants) <- paste0(var_name, ".lag", original_indices)
  
  return(lagged_variants)
}

# ---- Check significant/autocorrelated lags per variables ----
with_signifact_lags <- data.frame(VARS = names(data_transformations),
                                  RELEVANT_LAGS = sapply(names(data_transformations), function(x) {
                                    check_significant_lags(data_transformations, x)
                                  }))

# ---- Get all with lags ----
name_with_lags <- with_signifact_lags %>%
  filter(RELEVANT_LAGS != '' & !grepl("NPL.Ratio", VARS)) %>%
  pull(VARS); name_with_lags

# ---- Retrieve lags ----
all_lags <- map_dfc(name_with_lags, function(name_var) {
  retrieve_lags(name_var)
}) %>% as.data.frame() 

# ---- Combine predictors and lagged variables in one data frame ----
all_vars_and_lags <- data_transformations %>%
  cbind(all_lags) %>%
  # filter(!is.na(NPL.Ratio)) %>%
  slice(-c(1:12)) %>%
  na_locf() # Impute missing data via Last Observation Carried Forward method

rm(all_lags)

# ---- Split dataset into Train Data and Test Data ----
test_data <- all_vars_and_lags %>%
  slice((nrow(all_vars_and_lags) - no_of_test + 1):nrow(all_vars_and_lags))
train_data <- all_vars_and_lags %>%
  slice(1:(nrow(all_vars_and_lags) - no_of_test))


# ==== 8. CHECK NON STATIONARITY ====
stationary_tests <- data.frame(Vars = names(train_data),
                               ADF = sapply(train_data, function(x) adf.test(na.omit(x))$p.value),
                               KPSS = sapply(train_data, function(x) kpss.test(na.omit(x))$p.value))

# ---- Get non stationary variables to filter out later ----
non_stationary <- stationary_tests %>%
  filter(ADF > .10 & !grepl("NPL.Ratio", Vars)) %>%
  pull(Vars); non_stationary


# ==== 9. START VARIABLE SELECTION (BEST SUBSET SELECTION) ====

# ---- Choose filtering options based on chosen segment ----
final_data <- train_data %>%
  select(NPL.Ratio.ln.diff_3, everything()) %>%
  select(-any_of(non_stationary))

names(final_data)

# ---- Input number of predictors to use in the model ----
num_vars_taken <- 4

# ---- Start Best Subset Selection ----
best_subset <- regsubsets(NPL.Ratio.ln.diff_3 ~ ., data = final_data, method = 'exhaustive', 
                          nbest = 10000, nvmax = num_vars_taken, 
                          really.big = TRUE)
plot(best_subset, scale = 'adjr2')

# ---- Retrieve variable combinations ----
subsetting <- summary(best_subset)
get_combi <- subsetting$which %>% 
  as.data.frame() %>%
  filter(rowSums(.) == (num_vars_taken + 1))

# ---- Prepare to simulate the best candidate models ----
all_results <- data.frame()
all_models <- list()
all_predictions <- list()

# Initiate For Loop
i <- 1
for (i in 1:nrow(get_combi)) {
  
  # Retrieve set of predictors
  variables <- names(get_combi)[which(get_combi[i, ] == 'TRUE')]
  
  # Perform multiple linear regression
  all_forms <- paste0("NPL.Ratio.ln.diff_3 ~ ", paste(variables[-1], collapse = " + "))
  models <- lm(all_forms, data = train_data)
  all_models[[i]] <- models
  
  # Extract coefficient and p-value from model summary
  model_coefficient_value           = coef(summary(models))[, 1] %>% t() %>% as.data.frame()
  model_pval                        = coef(summary(models))[, 4] %>% t() %>% as.data.frame()
  
  
  if (length(model_coefficient_value) != (num_vars_taken + 1)) {
    no_to_add <- (num_vars_taken + 1) - length(t(coef(summary(models))[, 1]))
    col_names <- paste0('V_', 1:no_to_add)
    model_coefficient_value           = model_coefficient_value %>% 
      add_column(!!!setNames(rep(list(NA), no_to_add), 
                             paste0('V_', 1:no_to_add)))
    
    model_pval                        = model_pval %>% 
      add_column(!!!setNames(rep(list(NA), no_to_add), 
                             paste0('V_', 1:no_to_add)))
  }
  
  # Coefficients and P-Values
  model_coefficient_name            = names(model_coefficient_value) %>% t() %>% as.data.frame()
  colnames(model_coefficient_value) = c(paste0("Coefficient_", 1:(num_vars_taken + 1)))
  colnames(model_pval)              = c(paste0("p-value_", 1:(num_vars_taken + 1)))
  
  # Residual Standard Error
  model_resid                       = sigma(models)%>% as.data.frame()
  colnames(model_resid)             = c("Residual standard error")
  
  # Adjusted R-squared
  model_adjrsqrd                    = summary(models)$adj.r.squared %>% as.data.frame()
  colnames(model_adjrsqrd)          = c("Adjusted R-squared")
  
  # AIC
  model_AIC                         = AIC(models) %>% as.data.frame()
  colnames(model_AIC)               = c("AIC")
  
  # Train Data Model Metrics
  train_accuracy                    = accuracy(models) %>% as.data.frame()
  colnames(train_accuracy)          = paste0(names(train_accuracy), "_Train")
  rownames(train_accuracy)          = ''
  
  # Test Data Model Metrics
  prediction                        = predict(models, newdata = test_data[variables[-1]])
  all_predictions[[i]]              = prediction
  test_accuracy                     = accuracy(prediction, test_data$NPL.Ratio.ln.diff_3) %>% as.data.frame()
  colnames(test_accuracy)           = paste0(names(test_accuracy), "_Test")
  rownames(test_accuracy)           = ''
  
  # Variance Inflation Factor (VIF)
  val_vif               = tryCatch({
    t(car::vif(models))
  }, error = function(e) {
    matrix(NA, nrow = 1, ncol = num_vars_taken)
  }) %>% as.data.frame() 
  colnames(val_vif)     = c(paste0("VIF_", 1:(num_vars_taken)))
  
  #Durbin-Watson (autocorrelation)
  val_dw                = tryCatch({
    dwtest(models, order.by = NULL, alternative = "two.sided")$p.value %>% as.data.frame()
  }, error = function(e) {
    matrix(NA, nrow = 1, ncol = 1)
  })
  colnames(val_dw)      = c("DW")
  
  #Breusch-Godfrey(autocorrelation of errors)
  val_bg                = bgtest(models, 5)$p.value %>% as.data.frame()
  colnames(val_bg)      = c("BG")
  
  #Breusch-Pagan(constant variance)
  val_bp                = bptest(models)$p.value %>% as.data.frame()
  colnames(val_bp)      = c("BP")
  
  #Ljung-Box (absence of serial correlation; autocorrelation of residuals)
  val_Box               = Box.test(models$residuals, type = "Ljung-Box")$p.value %>% as.data.frame()
  colnames(val_Box)     = c("Box")
  
  #Shapiro-Wilk(normality)
  val_shapiro           = shapiro.test(models$residuals)$p.value %>% as.data.frame()
  colnames(val_shapiro) = c("Shapiro")
  
  # Engle-Granger (cointegration test)
  val_cointegrate       = adf.test(models$residuals)$p.value %>% as.data.frame()
  colnames(val_cointegrate) = c("Cointegration")
  
  #Multiplier per scenario
  coef_name_trim        = model_coefficient_name %>% select(-(V1))
  overlay_var_specific  = as.character(unlist(coef_name_trim))
  
  # Consolidate All Model Information and Metrics
  model_results = cbind(i,model_coefficient_name, model_coefficient_value, model_pval, model_resid, 
                        model_adjrsqrd, model_AIC, train_accuracy, test_accuracy, val_vif, val_dw, 
                        val_bg, val_bp, val_Box, val_shapiro, val_cointegrate)
  all_results   = rbind(all_results, model_results)
  
  print(paste0("Run 1: ", i, " of ", nrow(get_combi)))
}


# ==== 11. FINALIZE RESULTS ====

# ---- Determine if candidate models pass tests ----
comp_results = all_results
comp_results = comp_results %>% mutate(Pass.DW      = ifelse(DW      >= 0.05, "Pass", "Not"),
                                       Pass.BG      = ifelse(BG      >= 0.05, "Pass", "Not"),
                                       Pass.BP      = ifelse(BP      >= 0.05, "Pass", "Not"),
                                       Pass.Box     = ifelse(Box     >= 0.05, "Pass", "Not"),
                                       Pass.Shapiro = ifelse(Shapiro >= 0.05, "Pass", "Not"),
                                       Pass.Cointegration = ifelse(Cointegration <= 0.05, "Pass", "Not"))

comp_results_5  = comp_results

# ---- Export files with variables sorted according to AIC or Adjusted R-squared ----
num_models_5     = nrow(comp_results_5)
model_lowAIC_5   = comp_results_5[order(comp_results_5$AIC, decreasing = F)[1:num_models_5], ]
model_lowAIC_5   = comp_results_5 %>% mutate(V2_basename = str_remove_all(V2, "\\.lag\\d+"),
                                             V3_basename = str_remove_all(V3, "\\.lag\\d+"),
                                             V4_basename = str_remove_all(V4, "\\.lag\\d+"),
                                             V5_basename = str_remove_all(V5, "\\.lag\\d+"))

# ---- Check duplicates or combinations of original vars with lagged variants ----
cols_to_check <- paste0('V', 1:(num_vars_taken) + 1, '_basename')

with_duplicates = t(apply(model_lowAIC_5[cols_to_check], 1, function(x) {x[x==x[duplicated(x)]] = "Duplicate"; x})) %>% as.data.frame()
with_duplicates = with_duplicates %>% mutate(Duplicate = ifelse(rowSums(with_duplicates == "Duplicate") > 0 , 1, 0))

model_lowAIC_5   = model_lowAIC_5 %>% mutate(duplicates = with_duplicates$Duplicate)

# ---- Finalize final list of candidate models for SARIMAX model ----
model_lowAIC_5   = model_lowAIC_5 %>%
  filter_at(vars(c(paste0('VIF_', 1:num_vars_taken))), all_vars(. < 5)) %>%
  #filter_at(vars(c(paste0('p-value_', c(1:(num_vars_taken + 1))))), all_vars(. < .05)) %>%
  filter(duplicates == 0) %>%
  filter(Pass.Cointegration == "Pass" & 
           Pass.BP == "Pass")


# ==== 12. EXPORT RELEVANT DATA FINDINGS ====

# ---- Retrieve files for model validation ----
write.csv(model_lowAIC_5, "All Candidate NPL Ratio Models - 4 Variables.csv")
write.csv(final_data, "NPL Ratio and Macroeconomic Variables (transformed).csv")
write.csv(train_data, "NPL Ratio and Macroeconomic Variables - Train Data.csv")
write.csv(test_data, "NPL Ratio and Macroeconomic Variables - Test Data.csv")
save(all_models, get_combi, file = "List of MLR Models.RData")
