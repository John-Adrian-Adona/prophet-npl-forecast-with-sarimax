

# ==== 1. PREPARE WORKSPACE ====
rm(list = ls())
options(scipen = 999)


# ==== 2. LOAD LIBRARIES ====
library(openxlsx)
library(dplyr)
library(tseries)
library(astsa)
library(zoo)


# ==== 3. PREPARE WORKING DIRECTORY ====
overlay_loc <- "D:/John Adrian Files/Files/Projects/Final Compilation - NPL Ratio Forecasting Project"
setwd(overlay_loc)


# ==== 4. IMPORT INPUT FILES ====
setwd(paste0(overlay_loc, "\\OUTPUT"))

variables <- read.csv("NPL Prediction - Variable List (Monthly).csv")


# ==== 5. INPUT NUMBER OF OBSERVATIONS FOR TEST DATA ====
no_of_test <- 24
no_of_train <- nrow(variables) - no_of_test

# ---- Create Train and Test Data ----
test_data <- variables %>%
  select(-c(1:5)) %>%
  slice((no_of_train + 1):nrow(variables))
train_data <- variables %>%
  select(-c(1:5)) %>%
  slice(1:no_of_train)

# ==== 6. CHECK NON STATIONARITY ====
stationary_tests <- data.frame(Vars = names(train_data),
                               ADF = sapply(train_data, function(x) adf.test(na.omit(x))$p.value),
                               KPSS = sapply(train_data, function(x) kpss.test(na.omit(x))$p.value))

# ==== 7. DATA TRANSFORMATIONS ====

# ---- NPL RATIO ----
# Transformation Used: LOG TRANSFORM, QUARTERLY DIFFERENCING
ts_npl <- ts(train_data$NPL.Ratio, frequency = 12)
tsplot(ts_npl)

experiment <- diff(log(ts_npl), 3)
tsplot(experiment)

adf.test(experiment)
kpss.test(experiment)


# ---- GDP.Constant, GDP.Current, GCF, Household ----
# Transformation Used: LOG TRANSFORM, QUARTERLY DIFFERENCING
ts_predictor <- ts(train_data$GDP.Constant, frequency = 12)
tsplot(ts_predictor)

experiment <- diff(log(ts_predictor), lag = 3)
tsplot(experiment)

adf.test(experiment)
kpss.test(experiment)


# ---- Unemployment Rate, Labor Force Participation Rate ---- 
# Transformation Used: LOG TRANSFORM, QUARTERLY DIFFERENCING
ts_predictor <- ts(train_data$LaborForce.ParRate, frequency = 12)
tsplot(ts_predictor)

experiment <- diff(log(ts_predictor), 3)
tsplot(experiment)

adf.test(experiment)
kpss.test(experiment)


# ---- CPI.AllItem, .Housing, .Furnishing, .Transport, .Goods ----
# Transformation Used: LOG TRANSFORM, FIRST DIFFERENCING
# NOTE: Despite not passing KPSS Test, it is good to use since it passed ADF test
ts_predictor <- ts(train_data$CPI.Transport, frequency = 12)
tsplot(ts_predictor)

experiment <- diff(log(ts_predictor))
tsplot(experiment)

adf.test(experiment)
kpss.test(experiment)


# ---- Inf.All.Item, .Transport, .Housing, .Furnishing, .Goods ----
# Transformation Used: ASINH TRANSFORM, QUARTERLY DIFFERENCING
ts_predictor <- ts(train_data$Inf.Goods, frequency = 12)
tsplot(ts_predictor)

experiment <- diff(asinh(ts_predictor), 3)
tsplot(experiment)

adf.test(experiment)
kpss.test(experiment)


# ---- PPI.2018 ----
# Transformation Used: LOG TRANSFORM, FIRST DIFFERENCING
ts_predictor <- ts(train_data$PPI.2018, frequency = 12)
tsplot(ts_predictor)

experiment <- diff(log(ts_predictor))
tsplot(experiment)

adf.test(experiment)
kpss.test(experiment)


# ---- PHP/USD Rate ----
# Transformation Used: LOG TRANSFORM, FIRST DIFFERENCING
# Second Attempt: LOG TRANSFORM, QUARTERLY DIFFERENCING
ts_predictor <- ts(train_data$PHP.USD.rate, frequency = 12)
tsplot(ts_predictor)

experiment <- diff(log(ts_predictor), 3)
tsplot(experiment)

adf.test(experiment)
kpss.test(experiment)


# ---- Brent Crude Oil, WTI Crude Oil ----
# Transformation Used: LOG TRANSFORM, FIRST DIFFERENCING
# Second Attempt: LOG TRANSFORM, QUARTERLY DIFFERENCING
ts_predictor <- ts(train_data$Brent.Crude.Oil.Spot, frequency = 12)
tsplot(ts_predictor)

experiment <- diff(log(ts_predictor), 3)
tsplot(experiment)

adf.test(experiment)
kpss.test(experiment)


# ---- CCI, BCI ----
# Transformation Used: ASINH TRANSFORM, QUARTERLY DIFFERENCING
ts_predictor <- ts(train_data$BCI, frequency = 12)
tsplot(ts_predictor)

experiment <- diff(asinh(ts_predictor), 3)
tsplot(experiment)

adf.test(experiment)
kpss.test(experiment)


# ---- RPI.Goods, RPI.Machinery ----
# Transformation Used: LOG TRANSFORM, QUARTERLY DIFFERENCING
ts_predictor <- ts(train_data$RPI.Machinery, frequency = 12)
tsplot(ts_predictor)

experiment <- diff(log(ts_predictor), 3)
tsplot(experiment)

adf.test(experiment)
kpss.test(experiment)


# ---- WPI.Goods, WPI.Machinery ----
# Transformation Used: LOG TRANSFORM, QUARTERLY DIFFERENCING
ts_predictor <- ts(train_data$WPI.Machinery, frequency = 12)
tsplot(ts_predictor)

experiment <- diff(log(ts_predictor), 3)
tsplot(experiment)

adf.test(experiment)
kpss.test(experiment)


# ---- Bank Ave. Lending Rate ----
# Transformation Used: LOG TRANSFORM, FIRST DIFFERENCING
ts_predictor <- ts(train_data$Bank.Ave.Lend.Rate, frequency = 12)
tsplot(ts_predictor)

experiment <- diff(log(ts_predictor))
tsplot(experiment)

adf.test(experiment)
kpss.test(experiment)


# ---- Interest Rate / Target RRP----
# Transformation Used: LOG TRANSFORM, FIRST DIFFERENCING
ts_predictor <- ts(train_data$Interest.Rate, frequency = 12)
tsplot(ts_predictor)

experiment <- diff(log(ts_predictor))
tsplot(experiment)

adf.test(experiment)
kpss.test(experiment)
