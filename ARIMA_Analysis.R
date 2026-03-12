# Installing the packages to be used 

# install.packages(c("forecast", "tidyverse","kableExtra", "TSA", "tseries", "TSstudio",
#                    "prophet", "gtsummary", "tsibble", "fable", "readxl", "lattice",
#                    "latticeExtra", "lubricate","Kendall", "MASS","webshot2","ggThemeAssist",
#                    "gptstudio","extraDistr","plotly"))
# 
# install.packages("trend")

# Loading the required packages for the analysis.

library(fabletools)
library(forecast)
library(tidyverse)
library(TSA)
library(tseries)
library(TSstudio)
library(gtsummary)
library(tsibble)
library(fable)
library(readxl)
library(lattice)
library(latticeExtra)
library(lubridate)
library(Kendall)
library(webshot)
library(webshot2)
library(MASS)
library(nortest)
library(DHARMa)
library(moments)
library(trend)
library(seastests)

Original_data_project <- read_excel(choose.files())
Original_data_project

# Cleaning the time series data

Scabies.ts <- Original_data_project[ , c(1,4)]
Scabies.ts


# Converting the datasets into a time series data

Scabies_ts <- ts(Scabies.ts$SCABIES, 
                 start = c(2016,01),
                 end = c(2023,05),
                 frequency = 12)
Scabies_ts
class(Scabies_ts)
str(Scabies_ts)

# Descriptive statistics of the time series data.

summary(Scabies_ts)
sd(Scabies_ts)
kurtosis(Scabies_ts)
skewness(Scabies_ts)


# Testing for the trend in the Scabies time series data
# Ho: There is no monotonic trend in the Scabies data
# Ha: There is a monotonic trend in the Scabies data

trend_test <- MannKendall(Scabies_ts)
trend_test

# Since the p-value is less than alpha value, we reject Ho and 
# conclude that there is a monotonic trend present.


# Seasonal Testing of the time series data

kw(Scabies_ts)
isSeasonal(Scabies_ts)

# Decomposing the time series data

decomposition <- decompose(Scabies_ts)
decomposition

# Visualizing the decomposed time series data.

autoplot(decomposition, 
         main = "Decomposition of the Scabies Time Series Datasets.",  series = "seasonal", series.col = "red") + 
  autolayer(decomposition$seasonal, series = "trend", series.col = "blue") +
 theme(panel.grid.major = element_line(linetype = "dashed"),
    panel.grid.minor = element_line(linetype = "dashed"),
    axis.title = element_text(family = "serif",
        size = 20, face = "bold.italic"),
    plot.title = element_text(family = "serif",
        size = 25, face = "bold.italic")) +labs(x = "Monthly", y = NULL) + 
 theme(plot.title = element_text(hjust = 0.5))


# Visualizing the time series data sets

autoplot(Scabies_ts,
         main = "A Time Series Plot of A Monthly Scabies Recordings (2016 - 2023).",
         col = "green",
         linewidth = 0.8) + theme(axis.line = element_line(linetype = "solid"),
    panel.grid.major = element_line(linetype = "blank"),
    panel.grid.minor = element_line(linetype = "blank"),
    axis.title = element_text(family = "serif",
        size = 20, face = "bold.italic"),
    axis.text.y = element_text(size = 1),
    plot.title = element_text(family = "serif",
        size = 25, face = "bold.italic",
        hjust = 0.5), panel.background = element_rect(fill = NA)) +labs(x = "Monthly", y = "Number of cases ") +
 theme(axis.line = element_line(linetype = "blank"),
    axis.text = element_text(family = "serif",
        face = "bold.italic"), axis.text.x = element_text(family = "serif",
        size = 12), axis.text.y = element_text(family = "serif",
        size = 12)) + theme(plot.title = element_text(size = 22)) + theme(axis.title = element_text(size = 23),
    plot.title = element_text(size = 25)) + theme(axis.text = element_text(size = 13),
    axis.text.x = element_text(size = 17))

# ts_plot(Scabies_ts,
#         title = "A Time Series Plot of A Monthly Scabies Recordings (2016 - 2023).",
#         color = "green",
#         Xtitle = "Monthly",
#         Ytitle = "Number of cases")


### Checking for stationarity of the time series data

# Using the ADF test
# Ho: The Scabies time series data is not stationary.
# Ha: The Scabies time series data is stationary.

adf.test(Scabies_ts)

# Conclusion: Since the p-value (0.06664) is greater than alpha value(0.05),
# we fail to reject the Ho and conclude that the Scabies time series data is 
# not stationary, hence we to difference it.

# Using the PP test.
# Ho: The Scabies time series data is not stationary.
# Ha: The Scabies time series data is stationary.

pp.test(Scabies_ts)

# Conclusion: With PP test, the p-value is 0.01 indicating that the Scabies 
# time series data is stationary. This is due to the fact that the p-value is 
# lesser than alpha value 0.05, leading to the rejection of the null hypothesis
# Ho.


# Using the KPSS test
# Ho: The Scabies time series data is stationary.
# Ha: The Scabies time series data is not stationary.

kpss.test(Scabies_ts)

# Conclusion: Since the p-value is smaller than the alpha value 0.05, we
# reject Ho, and conclude that the Scabies time series data is not stationary.


# Differencing the time series datasets

Scabies_ts_diff1 <- diff(Scabies_ts, 
                         differences = 1)
Scabies_ts_diff1

# Visualizing the First differenced Scabies time series datasets.

ts_plot(Scabies_ts_diff1,
        title = "A First Differenced of the Scabies Time Series datasets.")


# Testing for stationarity of the first differenced time series data.

# Using the ADF test.
# Ho: The first differenced of Scabies time series data is not 
# stationary.
# Ha: The first differenced of the Scabies time series data is stationary.

adf.test(Scabies_ts_diff1)

# Conclusion: With a p-value of 0.01, we can now confirm the stationarity of 
# the Scabies time series data.


# Using the KPSS test.
# Ho: The first differenced of the Scabies time series data is stationary.
# Ha: The first differenced of the Scabies time series data is not 
# stationary.

kpss.test(Scabies_ts_diff1)

# Conclusion: With a p-value of 0.1, greater than 0.05, we fail to reject Ho
# and conclude that the Scabies time series data is now stationary.


# Plotting of the ACF(MA) and PACF(AR) for the model selection.

par(mfrow = c(1,2))
acf(Scabies_ts_diff1, lag.max = 20,
    main = " An ACF Plot of The First Differenced of the Scabies Time Series datasets. ")

pacf(Scabies_ts_diff1, lag.max = 20,
     main = "A PACF Plot of The First Differenced of the Scabies Time Series datasets.")

# From the ACF and PACF plots the model that can be derived from it is an
# ARIMA(1,1,1), ARIMA(2,1,2) and ARIMA(1,1,2).
# We then build competing models with the models obtain and select the model with least 
# information criteria.

Model_1 <- Arima(Scabies_ts, order = c(1,1,1))
Model_1  # AIC=979.68   AICc=979.97   BIC=987.12

Model_2 <- Arima(Scabies_ts, order = c(0,1,3))
Model_2  # AIC=975.89   AICc=976.37   BIC=985.8

Model_3 <- Arima(Scabies_ts, order = c(2,1,2))
Model_3  # AIC=978.34   AICc=979.08   BIC=990.73

Model_4 <- Arima(Scabies_ts, order = c(1,1,3))
Model_4  # AIC=977.87   AICc=978.6   BIC=990.26

Model_5 <- Arima(Scabies_ts, order = c(1,1,2))
Model_5  # AIC=976.69   AICc=977.18   BIC=986.6

Model_6 <- Arima(Scabies_ts, order = c(0,1,3), seasonal = c(0,0,1))
Model_6  # AIC=977.06   AICc=977.79   BIC=989.44

Model_7 <- Arima(Scabies_ts, order = c(1,1,2), seasonal = c(1,0,0))
Model_7  # AIC=977.84   AICc=978.58   BIC=990.23

Model_8 <- Arima(Scabies_ts, order = c(1,1,1), seasonal = c(0,0,1))
Model_8  # AIC=981.02   AICc=981.5   BIC=990.92

Model_9 <- Arima(Scabies_ts, order = c(1,1,3), seasonal = c(0,0,1))
Model_9  # AIC=979   AICc=980.04   BIC=993.87

Model_10 <- Arima(Scabies_ts, order = c(2,1,2), seasonal = c(1,0,0))
Model_10  # AIC=979.48   AICc=980.52   BIC=994.35

Model_11 <- Arima(Scabies_ts, order = c(0,1,3), seasonal = c(0,1,1))
Model_11  # AIC=867.23   AICc=868.08   BIC=878.88

Model_12 <- Arima(Scabies_ts, order = c(0,1,3), seasonal = c(0,1,0))
Model_12  # AIC=884.39   AICc=884.95   BIC=893.71

Model_13 <- Arima(Scabies_ts, order = c(0,1,3), seasonal = c(1,1,0))
Model_13  # AIC=872.96   AICc=873.82   BIC=884.62

Model_14 <- Arima(Scabies_ts, order = c(0,1,3), seasonal = c(1,1,1))
Model_14  # AIC=869.2   AICc=870.42   BIC=883.19

Model_15 <- Arima(Scabies_ts, order = c(1,1,2), seasonal = c(0,1,1))
Model_15  # AIC=866.79   AICc=867.65   BIC=878.44



# From the competing models, the optimal model is Model_15 which is 
# ARIMA(1,1,2)(0,1,1)[12] with an AIC=866.79, AICc=867.65, and a  BIC=878.44 which 
# is the model with the least information criteria.

# We then check for the appropriateness of the model, normality and randomness 
# of the residuals.

# Checking the normality of the residuals (Lilliefors_Test)

# Ho: The residuals of ARIMA(1,1,2)(0,1,1)[12] is normally distributed.
# Ha: The residuals of ARIMA(1,1,2)(0,1,1)[12] is not normally distributed.


lilliefors_normality_test <- lillie.test(residuals(Model_15))
print(lilliefors_normality_test)

# Conclusion: Since the p-value, 0.05373 is greater than alpha value, 0.05
# we fail to reject Ho and conclude that the residuals of the model ARIMA(1,1,2)(0,1,1)[12]
# is normally distributed.

# Checking for the appropriateness of the model.

# Ho: The model ARIMA(1,1,2)(0,1,1)[12] is appropriate.
# Ha: The model ARIMA(1,1,2)(0,1,1)[12] is not appropriate.

checkresiduals(Model_15)
ggAcf(residuals(Model_15)) + theme(axis.title = element_text(family = "serif",
    size = 20, face = "bold.italic"), axis.text = element_text(size = 15),
    plot.title = element_text(family = "serif",
        size = 22, face = "bold.italic"),
    panel.background = element_rect(fill = NA)) +labs(title = "An ACF plot of the residuals of ARIMA(1,1,2)(0,1,1)[12]")

# Conclusion: Since the p-value(0.9113) is greater than the alpha value(0.05)
# we fail to reject Ho and conclude that the model ARIMA(1,1,2)(0,1,1)[12] is appropriate.


# Since all the assumption are met, we then go ahead and forecast

# Forecasted Values for the next 12 months of the Scabies datasets

Forecasted_values <- forecast(Model_15, h = 12)
Forecasted_values


autoplot(Forecasted_values, flwd = 0.9,
         fcol = "green") + theme(axis.title = element_text(family = "serif",
                                                           size = 20, face = "bold.italic"), plot.title = element_text(family = "serif",
                                                                                                                       size = 25, face = "bold.italic", hjust = 0.5)) +labs(title = "Forecast of the SARIMA(1,1,2)(0,1,1)[12]  model of the Scabies TS data.",
                                                                                                                                                                            x = "Monthly", y = "Number of cases") + theme(panel.background = element_rect(fill = NA))
# Visualizing the forecasted values for the next 12 months.

# plot_forecast(Forecasted_values,
#               title = "Forecast of the SARIMA(1,1,2)(0,1,1)[12] model of the Scabies TS data.",
#               Xtitle = "Months",
#               Ytitle = "Number of cases",
#               color = "green")
# 

# autoplot(Forecasted_values) + autolayer(test_scabies_1)


# TESTING THE ACCURACY OF THE SCABIES TIME SERIES DATASETS.

# Splitting the data into train and test datasets (75% and 25%).

train_scabies_1 <- window(ts(Scabies.ts$SCABIES, start = c(2016,01), end = c(2021,08), frequency = 12))
test_scabies_1 <- window(ts(Scabies.ts$SCABIES, start = c(2021, 09), end = c(2023, 05), frequency = 12))                        
length(train_scabies_1)
length(test_scabies_1)

model_train_scabies_1 <- Arima(train_scabies_1, order = c(1,1,2), seasonal = c(0,1,1))
model_train_scabies_1

predictions_1 <- forecast(model_train_scabies_1, h = length(test_scabies_1))
predictions_1

accuracy_1 <- accuracy(predictions_1, test_scabies_1)
accuracy_1


# Splitting the data into train and test datasets (90% and 10%).

train_scabies_2 <- window(ts(Scabies.ts$SCABIES, start = c(2016,01), end = c(2022,08), frequency = 12))
test_scabies_2 <- window(ts(Scabies.ts$SCABIES, start = c(2022, 09), end = c(2023, 05), frequency = 12))                        
length(train_scabies_2)
length(test_scabies_2)

model_train_scabies_2 <- Arima(train_scabies_2, order = c(1,1,2), seasonal = c(0,1,1))
model_train_scabies_2

predictions_2 <- forecast(model_train_scabies_2, h = length(test_scabies_2))
predictions_2

accuracy_2 <- accuracy(predictions_2, test_scabies_2)
accuracy_2



# Splitting the data into 95% and 5%

train_scabies_3 <- window(ts(Scabies.ts$SCABIES, start = c(2016,01), end = c(2023,01), frequency = 12))
test_scabies_3 <- window(ts(Scabies.ts$SCABIES, start = c(2023,02), end = c(2023,05), frequency = 12))                        
length(train_scabies_3)
length(test_scabies_3)

model_train_scabies_3 <- Arima(train_scabies_3, order = c(1,1,2), seasonal = c(0,1,1))
model_train_scabies_3

predictions_3 <- forecast(model_train_scabies_3, h = length(test_scabies_3))
predictions_3

accuracy_3 <- accuracy(predictions_3, test_scabies_3)
accuracy_3



train_scabies_31 <- window(ts(Scabies.ts$SCABIES, start = c(2016,01), end = c(2023,01), frequency = 12))
test_scabies_31 <- window(ts(Scabies.ts$SCABIES, start = c(2023,02), end = c(2023,05), frequency = 12))                        
length(train_scabies_31)
length(test_scabies_31)

model_train_scabies_31 <- Arima(train_scabies_31, order = c(0,1,3), seasonal = c(0,1,1))
model_train_scabies_31

predictions_31 <- forecast(model_train_scabies_31, h = length(test_scabies_31))
predictions_31

accuracy_31 <- accuracy(predictions_31, test_scabies_31)
accuracy_31



