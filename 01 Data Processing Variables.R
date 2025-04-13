

# ==== 1. PREPARE WORKSPACE ====
rm(list = ls())
options(scipen = 999)

# ==== 2. LOAD LIBRARIES ====
library(openxlsx)
library(dplyr)
library(imputeTS)
library(astsa)
library(tseries)
library(stats)
library(zoo)


# ==== 3. PREPARE WORKING DIRECTORY ====
overlay_loc <- "D:/John Adrian Files/Files/Projects/Final Compilation - NPL Ratio Forecasting Project"
setwd(overlay_loc)


# ==== 4. IMPORT INPUT FILES ====
setwd(paste0(overlay_loc, "\\INPUT"))

variables <- read.xlsx("List of variables for NPL Ratio Prediction - final.xlsx",
                       sheet = "Monthly Data", startRow = 2,
                       detectDates = TRUE)

# for checking if interpolated monthly data mirrors quarterly data structure
quarterly_vars <- read.xlsx("List of variables for NPL Ratio Prediction - final.xlsx",
                            sheet = "Quarterly Data", startRow = 2)

# create a copy of variables_processed and quarterly_processed data frame
variables_processed <- variables
quarterly_processed <- quarterly_vars


# ==== 5. INTERPOLATE MISSING DATA + QUARTERLY INTO MONTHLY DATA ====

# ---- NPL RATIO ----
# Kalman Smoothing; Transform into decimals
tsplot(quarterly_processed$NPL.Ratio)

variables_processed <- variables_processed %>%
  mutate_at(vars("NPL.Ratio"), function(x) na_kalman(x) / 100)

tsplot(variables_processed$NPL.Ratio)

# ---- GDP.Constant, GDP.Current, GCF, GovExp, Household ----

# - if data is incomplete, use Kalman Smoothing -
quarterly_processed <- quarterly_processed %>%
  mutate(across(c("GDP.Constant", "GDP.Current", "Gross.Capital.Formation", 
                  "Government.Expenditure", "Household.Consumption"), na_kalman))
tsplot(quarterly_processed$GDP.Current)

# Linear Interpolation
variables_processed <- variables_processed %>%
  mutate(across(c("GDP.Constant", "GDP.Current", "Gross.Capital.Formation", 
                  "Government.Expenditure", "Household.Consumption"), na.approx))

tsplot(variables_processed$GDP.Current)

# ---- Unemploy and LFPR ----
# Kalman Smoothing; Transform into decimals
tsplot(variables_processed$Unemployment)
tsplot(variables_processed$LaborForce.ParRate)

variables_processed <- variables_processed %>%
  mutate_at(vars(c("Unemployment", "LaborForce.ParRate")), 
            function(x) na_kalman(x) / 100)

tsplot(variables_processed$Unemployment)
tsplot(variables_processed$LaborForce.ParRate)

# ---- Inf.All.Item, .Transport, .Housing, .Furnishing, .Goods ----
# Transform into decimals
variables_processed <- variables_processed %>%
  mutate_at(vars(c("Inf.All.Item", "Inf.Transport",
                   "Inf.Housing", "Inf.Furnishing",
                   "Inf.Goods")), function(x) x / 100)

# ---- Bank.Ave.Lend.Rate & Interest.Rate ----
# Transform into decimals
variables_processed <- variables_processed %>%
  mutate_at(vars(c("Bank.Ave.Lend.Rate", "Interest.Rate")), 
            function(x) x / 100)

# ---- PPI.2018 ----
# Kalman Smoothing
tsplot(variables_processed$PPI.2018)

variables_processed <- variables_processed %>%
  mutate_at(vars("PPI.2018"), na_kalman)

# ---- CCI and BCI ----
# Kalman Smoothing
tsplot(quarterly_processed$CCI)
tsplot(quarterly_processed$BCI)

variables_processed <- variables_processed %>%
  mutate_at(vars(c("CCI", "BCI")), na_kalman)

# ---- WPI.Goods and WPI.Machinery ----
# Kalman Smoothing
variables_processed <- variables_processed %>%
  mutate_at(vars(c("WPI.Goods", "WPI.Machinery")), na_kalman)

tsplot(variables_processed$WPI.Goods)
tsplot(variables_processed$WPI.Machinery)


# ==== 6. FINALIZE VARIABLES ====
final_var_data <- variables_processed %>%
  slice(-1)

# ---- VERIFY IF NO MISSING DATA ----
sapply(final_var_data, function(x) sum(is.na(x)))


# ==== 7. EXPORT FILE ====
setwd(paste0(overlay_loc, "\\OUTPUT"))
write.csv(final_var_data, "NPL Prediction - Variable List (Monthly).csv")
