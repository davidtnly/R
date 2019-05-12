# Start new
rm(list = ls())

# Load external scripts
source("theme_custom.R")

# Load dataset
data <- AirPassengers
class(data) # Make sure data is a "ts"

# Check ds
start(data)
end(data)
frequency(data)
summary(data)
dim(data)
# colSums(sapply(data,is.na))

# EDA
plot(data)
abline(reg = lm(data ~ time(data)))

# Other operations to help check inferences to be made
cycle(data) # Print cycles across years
plot(aggregate(data, FUN = mean)) # Aggregate the cycles per year
boxplot(data ~ cycle(data)) # plot shows an increase in months 6-8
# Strong seasonal effect with a cycle of 12 months of less (small variance as well)

## Intro to ARMA Time Series Modeling (auto-regression moving average)
# data needs to be staionary for ARMA to work
# Once we have a stationary time series, is it an AR or MA process?
# Correlation for a MA process should always be zero

# Use ACF (auto-correlation function)
# Plot of total correlation between different lag functions
# Find PACF

## Framework and Application of ARIMA Time Series Modeling
# 1: Visualize the time series
# 2: Stationarize the series
# 3: Plot ACF/PACF charts and find optimal parameters (p,d,q) = (AR,I,MA)
# 4: Build ARIMA model
# 5: Make predictions

# Start
plot(data)

# Address issues before checking for staionary
# 1) Remove unequal variances - log the series
# Address the trend component
# 2) Take difference of the series
library(tseries)
adf.test(diff(log(data)), alternative = "stationary", k = 0) # check if stationary enough

# Find right parameters to be used in the ARIMA model
acf(log(data))
# Analysis of this chart: decay is very slow which means that the population
#   is not stationary. intend to regress on the difference of logs rather than log directly.

# 1) Regress the difference
acf(diff(log(data)))
# Analysis: ACF plot cuts off after the first lag. Understand that value of
#   p should be 0 as the ACF is the curve getting a cut off. While value
#   of q should be 1 or 2. Value with least AIC/BIC = (0,1,1)

# Fit an ARIMA model and fit a seasonal component
fit <- arima(log(data)
             , c(0, 1, 1)
             , seasonal = list(order = c(0, 1, 1)
             , period = 12))

# Predict next 10 years
pred <- predict(fit, n.ahead = 10*12)
ts.plot(data, 2.718^pred$pred, log = "y", lty = c(1,3))

# Predict next 60 years
pred60 <- predict(fit, n.ahead = 60*12)
options(scipen = 99999)
ts.plot(data, 2.718^pred60$pred, log = "y", lty = c(1,3))


##################################################################################


