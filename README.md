# Adjusting Prophet Model Forecasts with Macroeconomic Indicators: A SARIMAX & MLR Approach

*A Forecast Adjustment Framework Incorporating Macroeconomic Trends.*

___

### OVERVIEW

Accurate Non-Performing Loan (NPL) Ratio forecasts are crucial for financial institutions to assess credit risk and ensure financial stability. The NPL Ratio represents the percentage of loans that may not be repaid, making reliable forecasting essential for proactive risk management.

Prophet models can predict future NPL trends by detecting non-linear patterns and seasonality with minimal adjustments. Additionally, macroeconomic indicators can be incorporated as external regressors to refine future forecasts. However, naively including these regressors presents challenges:

*	**Selecting the right economic indicators** – With many macroeconomic factors to choose from and limited data availability, a limited set of macroeconomic combination is recommended. However, Prophet does not automatically determine the most relevant predictors, often requiring subjective selection.

*	**Avoiding redundant or correlated variables** – Without careful selection, multiple economic indicators may introduce high correlation between each other or multicollinearity, risking model reliability and interpretability.

To address these issues, this project employs Multiple Linear Regression (MLR) with Best Subset Selection to systematically identify the most relevant set of macroeconomic factors before incorporating them into a SARIMAX model. This approach enhances Prophet forecasts by integrating key economic indicators, resulting in more interpretable projections and deeper economic insights.


### INSTALLATION

##### Prerequisites

To run this projecet, ensure you have the following installed before proceeding with this project:

* R
* RStudio
* Git

##### Required Packages

Install the necessary packages for the project using the line of R script below:

```r
install.packages(c(
  "openxlsx", "dplyr", "imputeTS", "astsa", "tseries", "zoo",
  "leaps", "forecast", "lmtest", "purrr", "stringr",
  "lubridate", "prophet", "ggplot2"
))
```

##### Running the Project

1. Clone or download this repository
2. Run the scripts in the following order:
* `01 Data Processing Variables.R`
* `02 ARIMA Model - Data Processing.R`
* `03 SARIMAX + PROPHET Model Validation - MODEL 9254.R`

##### NOTE: 
- The R script file, `01.5 Multiple Linear Regression - Checking.R`, is unnecessary to run the whole project. It serves as proof of experimentation for the data transformations and stationarity.
- This project does not include an `.Rproj` file. You can run the R scripts manually or create one by opening the folder in RStudio and selecting `New Project > Existing Directory`.

### DATA COLLECTION

For detailed variable descriptions, please see this project's [Data Dictionary]().

### MODEL SELECTION AND TRAINING

1.	Due to the limited size of the monthly dataset and the large number of potential macroeconomic indicators, a limit is imposed on the number of regressors or exogenous variables. To ensure non-arbitrary selection of these indicators, we use a Multiple Linear Regression (MLR) framework in combination with the Best Subset Selection approach.
2.	Best Subset Selection evaluates all possible combinations of regressors, within the imposed limit, and selects the optimal model based on statistical criteria and other considerations.
3.	After performing Best Subset Selection, the following criteria were applied to determine the final list of candidate models:
   1.	**Multicollinearity** – Models with regressors exhibiting high multicollinearity (VIF > 5) or excessive correlation between macroeconomic indicators were avoided to ensure stable estimates.
   2.	**Homoscedasticity** – Models with non-constant variance in error terms were excluded, verified through the Breusch-Pagan test (p-value > 0.05)
   3.	**Cointegration Test** – Models with non-stationary error terms, indicating unstable long-term relationships between regressors and the target variable (NPL ratios), were rejected, verified through the Engle-Granger test (p-value > 0.05)
   4.	**Non-Duplication** – Models containing redundant regressors (e.g., the original regressor and its lag variant) were excluded to avoid information duplication
4.	Regression models that handle time series information also must account for autocorrelation or serial correlations within the regressors and target variable, depicting correlation with their past values. To account for these autocorrelations, migrating the chosen model to SARIMAX is necessary
5.	Time series regression models must account for autocorrelation or serial correlation, where regressors and the target variable correlate with past values. To address these autocorrelations, the selected model is transitioned to SARIMAX, which explicitly handles these dependencies.

### FORECASTING AND POST-PROCESSING

1.	This project will forecast NPL ratios and the other macroeconomic indicators for the next 24 months after December 2024, giving the monthly predictions for the years 2025 and 2026.
2.	As endorsed in the project title, the future forecasts of the historical NPL ratios are done using Prophet with an additional cosine Fourier term to help improve the baseline forecasts.
3.	On the other hand, the future forecasts of macroeconomic indicators from the chosen model use ETS models on their original scaling before consolidating their future forecasts with the historical data and applying the same data transformation done in the MLR-Best Subset Selection framework. 
4.	The SARIMAX outputs of the chosen model will be reverted back to its original scaling with the help of Prophet’s baseline forecasts of future NPL ratios, using them to revert the differencing and log transformations done prior the model selection.

### RESULTS AND INSIGHTS

