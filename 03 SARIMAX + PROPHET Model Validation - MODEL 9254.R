

# ==== 1. PREPARE WORKSPACE ====
rm(list = ls())
options(scipen = 999)


# ==== 2. LOAD LIBRARIES ====
library(tseries)
library(forecast)
library(dplyr)
library(ggplot2)
library(purrr)
library(lubridate)
library(prophet)
library(astsa)

# ==== 3. PREPARE WORKING DIRECTORY ====
overlay_loc <- "D:/John Adrian Files/Files/Projects/Final Compilation - NPL Ratio Forecasting Project"
setwd(overlay_loc)

# ==== 4. IMPORT INPUT FILES ====
setwd(paste0(overlay_loc, "\\OUTPUT"))

variables_final <- read.csv("NPL Prediction - Variable List (Monthly).csv")
final_data <- read.csv("NPL Ratio and Macroeconomic Variables (transformed).csv")
train_data <- read.csv("NPL Ratio and Macroeconomic Variables - Train Data.csv")
test_data <- read.csv("NPL Ratio and Macroeconomic Variables - Test Data.csv")
load("List of MLR Models.RData")


# ==== 5. START MODEL VALIDATION ====

# ---- Test model ----

# Input chosen model number here (based on NPL Prediction - Variable List)
n <- 9254
test_model <- all_models[[n]]
summary(test_model)

# Compare actual vs fitted results (transformed)
{
  # Predict Model using Test Data
  variables <- names(get_combi)[which(get_combi[n, ] == 'TRUE')]
  prediction <- predict(test_model, newdata = test_data[variables[-1]])
  
  # Consolidate actual and fitted values
  actual <- c(train_data$NPL.Ratio, test_data$NPL.Ratio)
  predicted <- c(test_model$fitted.values, prediction)
  
  # Revert them back to original scaling
  original_actual <- exp(lag(log(variables_final$NPL.Ratio[-c(1:12)]), 3) + c(train_data$NPL.Ratio, test_data$NPL.Ratio))
  original_predicted <- exp(lag(log(variables_final$NPL.Ratio[-c(1:12)]), 3) + c(test_model$fitted.values, prediction))
  
  plot(actual, type = "l", col = "blue")
  lines(predicted, col = "red")
}

# Compare actual vs fitted results (original scaling)
{

plot(original_actual, type = "l", col = "blue")
lines(original_predicted, col = "red")

}


# ==== 6. CREATEACTUAL VS FITTED GGPLOT GRAPH ====

# ---- Create custom theme for the ggplot ----
custom_theme <- theme(
  legend.position = "top",
  legend.text = element_text(size = 16, face = "italic"),
  legend.key = element_rect(fill = NA),
  legend.title = element_blank(),
  
  panel.grid.major = element_line(color = "#C0B8A9"),
  panel.grid.minor = element_line(color = "#E5DCC3"),
  
  axis.text = element_text(size = 16, face = "bold", color = "#1E1E1E"),
  axis.text.x = element_text(angle = 45, hjust = 1),
  axis.title = element_text(size = 16, face = "bold"),
  
  
  panel.background = element_rect(fill = "#FAF3E0", color = NA),
  panel.border = element_rect(linewidth = 2),
  plot.background = element_rect(fill = "white"),
  plot.title = element_text(size = 24, face = "bold", 
                            hjust = 0.5, margin = margin(b = 10)),
  plot.margin = margin(t = 30, r = 30, b = 30, l = 30),
  
  strip.text = element_text(size = 16, face = "bold")
)

# ---- Prepare data frame for the ggplot ----
original_state_results <- data.frame(
  date = rep(mdy(variables_final$Date[-c(1:12)]), 2),
  npl = c(original_actual, original_predicted),
  category = c(rep("Actual Data", 120), rep("Model Fit (Train Data)", 96), 
               rep("Model Prediction (Test Data)", 24))
) %>%
  mutate_at(vars("category"), function(x) {
    factor(x, levels = c("Actual Data", "Model Fit (Train Data)",
                         "Model Prediction (Test Data)"))
  })


# ---- Execute the ggplot ----
ggplot(original_state_results, aes(x = date, y = npl, 
                                   color = category, linetype = category)) +
  geom_line(data = subset(original_state_results, category == "Actual Data"), size = 3) + 
  geom_line(data = subset(original_state_results, category == "Model Fit (Train Data)"), size = 1.5) + 
  geom_line(data = subset(original_state_results, category == "Model Prediction (Test Data)"), size = 1.5) + 
  scale_color_manual(values = c("Actual Data" = "#003366", "Model Fit (Train Data)" = "red", 
                                "Model Prediction (Test Data)" = "orange")) +
  scale_linetype_manual(values = c("Actual Data" = "solid", "Model Fit (Train Data)" = "solid", 
                                   "Model Prediction (Test Data)" = "11")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Comparing Actual Data vs. Initial Model Predictions (M.L.R.)",
       x = "Date", y = "NPL Ratio", color = "category") + 
  theme_bw() +
  custom_theme


# ==== 7. FORECAST FUTURE VALUES (24 MONTHS) ====

# Check list of predictors in the model
variables

# ---- VAR 1 ----

# Check initial transformed data
ts_var_1 <- ts(final_data$CPI.All.Item.ln.diff, frequency = 12)
plot(ts_var_1)

adf.test(ts_var_1)
kpss.test(ts_var_1)

# Forecast using original data prior data transformations
ts_var_1 <- ts(variables_final$CPI.All.Item, frequency = 12)
plot(ts_var_1)

# Prepare STL + ETS MODEL
stl_var_1 <- stl(ts_var_1, s.window = "periodic")
plot(stl_var_1)

forecast_var_1 <- stlf(ts_var_1, etsmodel = "MAN", damped = FALSE, h = 24)
forecast_seasonal_1 <- forecast(stl_var_1$time.series[, "seasonal"], h = 24)

# Check forecasts and accuracy
plot(forecast_var_1)
plot(forecast_seasonal_1)

tsplot(c(ts_var_1, forecast_var_1$mean + forecast_seasonal_1$mean))
#accuracy(c(forecast_var_1$mean + forecast_seasonal_1$mean)[1:24],
#         variables_final$CPI.All.Item[109:132])

#fitted(stl_var_1)

# Revert data back to transformed scaling
var_1_result <- data.frame(var_1 = c(ts_var_1, 
                                     forecast_var_1$mean + forecast_seasonal_1$mean)) %>%
  mutate_all(function(x) c(NA, diff(log(x)))) %>%
  slice(-c(1:12))
tsplot(var_1_result$var_1)


# ---- VAR 2 ----

# Check initial transformed data
ts_var_2 <- ts(final_data$CCI.asinh.diff_3, frequency = 12)
plot(ts_var_2)

adf.test(ts_var_2)
kpss.test(ts_var_2)

# Forecast using original data prior data transformations
ts_var_2 <- ts(variables_final$CCI, frequency = 12)
plot(ts_var_2)

# Prepare STL + ETS MODEL
stl_var_2 <- stl(ts_var_2, s.window = "periodic")
plot(stl_var_2)

forecast_var_2 <- stlf(ts_var_2, damped = TRUE, h = 24)
forecast_seasonal_2 <- forecast(stl_var_2$time.series[, "seasonal"], h = 24)

# Check forecasts and accuracy
plot(forecast_var_2)
plot(forecast_seasonal_2)

tsplot(c(ts_var_2, forecast_var_2$mean + forecast_seasonal_2$mean))

# Revert data back to transformed scaling
var_2_result <- data.frame(var_2 = c(variables_final$CCI, 
                                     forecast_var_2$mean + forecast_seasonal_2$mean)) %>%
  mutate_all(function(x) c(rep(NA, 3), diff(asinh(x), 3))) %>%
  slice(-c(1:12))

tsplot(var_2_result$var_2)


# ---- VAR 3 ----

# Check initial transformed data
ts_var_3 <- ts(final_data$GDP.Current.ln.diff_3.lag2, frequency = 12)
plot(ts_var_3)

adf.test(ts_var_3)
kpss.test(ts_var_3)

# Forecast using original data prior data transformations
ts_var_3 <- ts(variables_final$GDP.Current, frequency = 12)
plot(ts_var_3)

# Prepare STL + ETS MODEL
stl_var_3 <- stl(ts_var_3, s.window = "periodic")
plot(stl_var_3)

forecast_var_3 <- stlf(ts_var_3, h = 24)
forecast_seasonal_3 <- forecast(stl_var_3$time.series[, "seasonal"], h = 24)

# Check forecasts and accuracy
plot(forecast_var_3)
plot(forecast_seasonal_3)

tsplot(c(ts_var_3, forecast_var_3$mean + forecast_seasonal_3$mean))

# Revert data back to transformed scaling
var_3_result <- data.frame(var_3 = c(ts_var_3, 
                                     forecast_var_3$mean + forecast_seasonal_3$mean)) %>%
  mutate_all(function(x)lag(c(rep(NA, 3), diff(log(x), 3)), 2)) %>%
  slice(-c(1:12))

tsplot(var_3_result$var_3)


# ---- VAR 4 ----

# Check initial transformed data
ts_var_4 <- ts(final_data$Gross.Capital.Formation.ln.diff_3.lag5, frequency = 12)
plot(ts_var_4)

adf.test(ts_var_4)
kpss.test(ts_var_4)

# Forecast using original data prior data transformations
ts_var_4 <- ts(variables_final$Gross.Capital.Formation[81:132], frequency = 12)
plot(ts_var_4)

# Prepare STL + ETS MODEL
stl_var_4 <- stl(ts_var_4, s.window = "periodic")
plot(stl_var_4)

forecast_var_4 <- stlf(ts_var_4, etsmodel = "MAN", h = 24)
forecast_seasonal_4 <- forecast(stl_var_4$time.series[, "seasonal"], h = 24)

# Check forecasts and accuracy
plot(forecast_var_4)
plot(forecast_seasonal_4)

tsplot(c(ts_var_4, forecast_var_4$mean))

# Revert data back to transformed scaling
var_4_result <- data.frame(var_4 = c(variables_final$Gross.Capital.Formation, 
                                     forecast_var_4$mean)) %>%
  mutate_all(function(x)lag(c(rep(NA, 3), diff(log(x), 3)), 5)) %>%
  slice(-c(1:12))

tsplot(var_4_result$var_4)


# ---- VAR 5 (NPL RATIO) ----

# Check NPL Ratio data
ts_var_5 <- ts(variables_final$NPL.Ratio, frequency = 12)
plot(ts_var_5)

pacf(ts_var_5)
acf(ts_var_5)

# -- PROPHET MODEL ATTEMPT --

# CREATE DATAFRAME FOR PROPHET
df_for_prophet <- data.frame(
  ds = mdy(variables_final$Date)[1:129],
  y = (ts_var_5)[1:129]
) 

# INITIATE PROPHET MODEL
{
  model <- prophet(changepoint.prior.scale = .15,
                   n.changepoints = 10,
                   seasonality.prior.scale = 5,
                   yearly.seasonality = FALSE)
  model <- add_seasonality(model, name = 'yearly',
                         period = 60, fourier.order = 2)
  model <- fit.prophet(model, df_for_prophet)

  future <- make_future_dataframe(model,
                                periods = 27, freq = "month")
  
  forecast <- predict(model, future)
  plot(model, forecast)
}

# CHECK POST-MODEL CROSS VALIDATION (VIA ROLLING WINDOW)
validate_model <- cross_validation(model, 
                                   initial = 2555,
                                   period = 180, 
                                   horizon = 730, units = "days")

# CHECK PERFORMANCE METRICS 
performance_metrics(validate_model)

# PLOT CROSS VALIDATION RESULTS 
plot_cross_validation_metric(validate_model, metric = 'mae')
plot_cross_validation_metric(validate_model, metric = 'rmse')
plot_cross_validation_metric(validate_model, metric = 'mape')

# PLOT PROPHET COMPONENTS
prophet_plot_components(model, forecast)


# ==== 8. SAVE FUTURE FORECASTS AS DATA FRAME ====
future_values <- data.frame(V1 = var_1_result$var_1[121:144],
                            V2 = var_2_result$var_2[121:144],
                            V3 = var_3_result$var_3[121:144],
                            V4 = var_4_result$var_4[121:144]) %>%
  set_names(variables[-1])


# ==== 9. TEST OUT SARIMAX MODEL ====

# Check NPL Ratio data again
ts_npl <- ts(train_data$NPL.Ratio, 
             frequency = 12)
plot(ts_npl)

adf.test(ts_npl)
kpss.test(ts_npl)

# ---- Move to SARIMAX model using Arima() ----

# Check PACF and ACF plots
pacf(ts_npl)
acf(ts_npl)

# Initiate SARIMAX Model with chosen terms (via trial and error)
sarimax <- Arima(ts_npl, order = c(1, 0, 3), seasonal = c(2, 1, 2), include.mean = FALSE,
                 biasadj = TRUE, xreg = as.matrix(train_data[variables[-1]]), method = "ML")
forecast_sarimax <- forecast(sarimax, h = 48,
                             xreg = as.matrix(rbind(test_data[variables[-1]], future_values)))

# Check forecasts and accuracy results
plot(forecast_sarimax)
tsplot(c(ts_npl, test_data$NPL.Ratio.ln.diff_3))

accuracy(sarimax)

# Compare Fitted VS Actual Plots
tsplot(fitted(sarimax))
tsplot(ts_npl)

# Check residuals 
pacf(residuals(sarimax))
acf(residuals(sarimax))

# Check Ljung-Box test
checkresiduals(sarimax)


# ==== 10. PREPARE GRAPH RESULTS ====

# ---- Next 24 months forecasts ----

# Prepare data frame for the ggplot (PART 1)
visual <- data.frame(date = seq(from = as.Date("2015-01-01"), length.out = 144, by = "months"),
                     Lag3_original = c(
                       rep(NA, 3),
                       log(variables_final$NPL.Ratio[-c(1:12)]),
                       log(forecast$yhat[133:153])
                     ),
                     Lag3_original_lower = c(
                       rep(NA, 3),
                       log(variables_final$NPL.Ratio[-c(1:12)]),
                       log(forecast$yhat_lower[133:153])
                     ),
                     Lag3_original_upper = c(
                       rep(NA, 3),
                       log(variables_final$NPL.Ratio[-c(1:12)]),
                       log(forecast$yhat_upper[133:153])
                     ),
                     From_model = c(sarimax$fitted, 
                                    forecast_sarimax$mean),
                     From_model_lower = c(sarimax$fitted, 
                                          forecast_sarimax$lower[, "80%"]),
                     From_model_upper = c(sarimax$fitted, 
                                          forecast_sarimax$upper[, "80%"]),
                     Original = c(variables_final$NPL.Ratio[-c(1:12)],
                                  rep(NA, times = 24))) %>%
  mutate(Final_forecasts = exp(Lag3_original + From_model),
         Final_forecasts_lower = exp(Lag3_original_lower + From_model_lower),
         Final_forecasts_upper = exp(Lag3_original_upper + From_model_upper),
         Category = c(rep("Train", 96), rep("Test", 24), 
                      rep("Forecasts", 24))) 

# Prepare data frame for the ggplot (PART 2)
for_plotting <- data.frame(
  date = rep(visual$date, 6),
  values = c(
    c(visual$Original[visual$Category == "Train"], rep(NA, 48)), 
    c(rep(NA, 96), visual$Original[visual$Category == "Test"], rep(NA, 24)),
    c(rep(NA, 96), visual$Final_forecasts[visual$Category == "Test"], rep(NA, 24)),
    c(rep(NA, 120), visual$Final_forecasts[visual$Category == "Forecasts"]),
    c(rep(NA, 96), visual$Final_forecasts_lower[visual$Category %in% c("Test", "Forecasts")]),
    c(rep(NA, 96), visual$Final_forecasts_upper[visual$Category %in% c("Test", "Forecasts")])
  ),
  category = c(rep("Train Data", 144), 
               rep("Test Data", 144), 
               rep("Model Prediction", 144),
               rep("Future Forecasts", 144),
               rep("80% Lower Forecast Interval", 144),
               rep("80% Upper Forecast Interval", 144))
) %>%
  mutate_at(vars("category"), function(x) {
    factor(x, levels = c("Train Data", "Test Data",
                         "Model Prediction", "Future Forecasts",
                         "80% Lower Forecast Interval", "80% Upper Forecast Interval"))
  })

# Execute the ggplot
ggplot(for_plotting, aes(x = date, y = values, 
                         color = category, linetype = category)) +
  geom_line(data = subset(for_plotting, category == "Train Data"), size = 2) + 
  geom_line(data = subset(for_plotting, category == "Test Data"), size = 2) + 
  geom_line(data = subset(for_plotting, category == "Model Prediction"), size = 2) + 
  geom_line(data = subset(for_plotting, category == "Future Forecasts"), size = 2) + 
  scale_color_manual(values = c("Train Data" = "#003366", "Test Data" = "green", 
                                "Model Prediction" = "red", "Future Forecasts" = "orange")) +
  scale_linetype_manual(values = c("Train Data" = "solid", "Test Data" = "solid", 
                                   "Model Prediction" = "11", "Future Forecasts" = "11")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "NPL Ratio Forecasts for the Next 24 Months",
       x = "Date", y = "NPL Ratio", color = "category") + 
  theme_bw() +
  custom_theme

# ---- Actual VS Fitted: SARIMAX Forecast Accuracy ----
sarimax_accuracy <- data.frame(
  date = rep(visual$date, 2),
  npl = c(ts_var_5[13:132], forecast$yhat[133:156], 
          visual$Final_forecasts),
  category = c(rep("Actual Data", 120), rep("Baseline Forecasts (Prophet)", 24),
               rep("Model Fit (Train Data)", 96), 
               rep("Model Prediction (Test Data)", 48))
) %>%
  mutate_at(vars("category"), function(x) {
    factor(x, levels = c("Actual Data", "Baseline Forecasts (Prophet)", 
                         "Model Fit (Train Data)",
                         "Model Prediction (Test Data)"))
  })


# ---- Execute the ggplot ----
ggplot(sarimax_accuracy, aes(x = date, y = npl, 
                             color = category, linetype = category)) +
  geom_line(data = subset(sarimax_accuracy, category == "Actual Data"), size = 3) + 
  geom_line(data = subset(sarimax_accuracy, category == "Baseline Forecasts (Prophet)"), size = 2) + 
  geom_line(data = subset(sarimax_accuracy, category == "Model Fit (Train Data)"), size = 1.5) + 
  geom_line(data = subset(sarimax_accuracy, category == "Model Prediction (Test Data)"), size = 1.5) + 
  scale_color_manual(values = c("Actual Data" = "#003366", 
                                "Baseline Forecasts (Prophet)" = "skyblue",
                                "Model Fit (Train Data)" = "red", 
                                "Model Prediction (Test Data)" = "orange")) +
  scale_linetype_manual(values = c("Actual Data" = "solid", 
                                   "Baseline Forecasts (Prophet)" = "solid",
                                   "Model Fit (Train Data)" = "solid", 
                                   "Model Prediction (Test Data)" = "11")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Comparing Baseline (Prophet) vs. Macroeconomic Adjustments (SARIMAX)",
       x = "Date", y = "NPL Ratio", color = "category") + 
  theme_bw() +
  custom_theme


# ---- Prediction interval comparison ----

# Prepare data frame for the ggplot
ci_comparision <- data.frame(
  date = rep(seq(from = as.Date("2023-01-01"), length.out = 48, by = "months"), 4),
  values = c(
    c(variables_final$NPL.Ratio[109:132], rep(NA, 24)),
    c(rep(NA, 24), forecast$yhat[133:156]),
    c(variables_final$NPL.Ratio[109:132], rep(NA, 24)),
    c(rep(NA, 24), visual$Final_forecasts[visual$Category == "Forecasts"])
  ),
  category = rep(c(rep("Actual Data", 48),
                   rep("Future Forecasts", 48)), 2),
  model = c(rep("Baseline Forecasts", 96), 
            rep("with Macroeconomic Adjustments", 96))
) %>%
  mutate(lower = c(c(rep(NA, 72), forecast$yhat_lower[133:156]),
                   c(rep(NA, 72), visual$Final_forecasts_lower[visual$Category == "Forecasts"])),
         upper = c(c(rep(NA, 72), forecast$yhat_upper[133:156]),
                   c(rep(NA, 72), visual$Final_forecasts_upper[visual$Category == "Forecasts"])))

# Execute the ggplot
ggplot(ci_comparision, aes(x = date, y = values, 
                           color = category, linetype = category)) +
  facet_wrap(~model) +
  geom_ribbon(aes(ymin = lower,
                  ymax = upper), fill = "grey",
              linetype = "22", alpha = .5, size = 1) +
  geom_line(size = 2) +
  scale_color_manual(values = c("Actual Data" = "green", "Future Forecasts" = "orange")) +
  scale_linetype_manual(values = c("Actual Data" = "solid", "Future Forecasts" = "solid")) + 
  labs(title = "NPL Ratio Forecasts: Comparing Two Time Series Approaches",
       x = "Date", y = "NPL Ratio", color = "category") +
  theme_bw() + 
  custom_theme


# ==== 11. CHECK FORECAST VS. ACTUAL SCATTER PLOT ====

# Prepare data frame for scatter plot
scatter_plot_df <- data.frame(
  Actual = c(ts_var_5[109:132], forecast$yhat[133:156]),
  Forecast = visual$Final_forecasts[visual$Category %in% c("Test", "Forecasts")]
  )

# Execute the scatter plot
ggplot(scatter_plot_df, aes(x = Actual, y = Forecast)) +
  geom_point(color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Actual vs Forecast Scatter Plot") +
  theme_bw() + 
  custom_theme

# ==== 12. CHECK MODEL METRICS ====

# ---- Train Results Validation ----
accuracy(ts(visual$Final_forecasts[visual$Category == "Train"]),
         ts(ts_var_5[13:108]))

# ---- Test Results Validation ----
accuracy(ts(visual$Final_forecasts[visual$Category %in% c("Test", "Forecasts")]),
         ts(c(ts_var_5[109:132], forecast$yhat[133:156])))


# ==== 13. SAVE PROGRESS ====
save.image("NPL Prediction - Model Validation.RData")
load("NPL Prediction - Model Validation.RData")
