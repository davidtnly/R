# Start new
rm(list = ls())

# Load external scripts
source("theme_custom.R")

# Load libraries
if (require(readr) == FALSE)     {install.packages("readr")}; library(readr)         # Load our data
if (require(dplyr) == FALSE)     {install.packages("dplyr")}; library(dplyr)         # Data Wrangling
if (require(ggplot2) == FALSE)   {install.packages("ggplot2")}; library(ggplot2)     # Visualization
if (require(ggfortify) == FALSE) {install.packages("ggfortify")}; library(ggfortify) # Visualization
if (require(tseries) == FALSE)   {install.packages("tseries")}; library(tseries)     # Statistical Tests for Time Series data
if (require(forecast) == FALSE)  {install.packages("forecast")}; library(forecast)   # Time Series Forecasting
if (require(DT) == FALSE)        {install.packages("DT")}; library(DT)               # View our Raw Data

# Load Data
data <- read.csv("data.csv",
               stringsAsFactors = FALSE)
# Check data
head(data)
dim(data)
summary(data)
frequency(data)
start(data)
end(data)

## Preprocessing
data1 <- data %>% 
  select(year, month, in., out) 
head(data, n = 10)

# Convert data
data1$in. <- as.numeric(data1$in., na.rm = FALSE)
data1$out <- as.numeric(data1$out, na.rm = FALSE)
data1$year <- as.factor(data1$year)
data1$month <- as.factor(data1$month)

# Update column names
colnames(data1) <- c("Year", "Month", "Entry" , "Exit", "Total") 

# Remove dupes
toBeRemoved <- which(data1$Month == "Total")
x <- data1[-toBeRemoved,] # remove duplicate columns
mactan <- x[complete.cases(x), ]  # remove NA's
mactan$Month <- factor(mactan$Month,
                       labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                  "Oct", "Nov", "Dec")) # arrange our month levels from proper Jan to Dec 

# EDA (Plots) Check for outliers
ggplot(mactan, aes(x = Year, y = Entry)) + 
  geom_boxplot(outlier.colour = "red")

# Look at outlier (1999)
ind <- mactan[which(mactan$Year == '1999' & mactan$Entry > 500),]
ind

# Change outlier and regraph plot
macftan[103, 3] <- 150 # changed outlier to median
ggplot(mactan, aes(x = Year, y = Entry)) + 
  geom_boxplot(outlier.colour = "red")

## Create time series
## Steps
## Framework and Application of ARIMA Time Series Modeling
# 1: Visualize the time series
# 2: Stationarize the series
# 3: Plot ACF/PACF charts and find optimal parameters (p,d,q) = (AR,I,MA)
# 4: Build ARIMA model
# 5: Make predictions

# Aggregate the data
cb_fl <- aggregate(Entry ~ Month + Year, mactan, sum)
summary(cb_fl)

# Convert aggregate data in time series using ts()
cebu_ts <- ts(cb_fl$Entry,
              c(1991, 1), # Start?
              c(2017, 4), # End?
              12)         # Monthly?

# Use autoplot() to make ts plot
autoplot(cebu_ts) + 
  xlab(label = "Year") +
  ylab(label = "# of Flights") +
  ggtitle("MCIAA Entry Flights") +
  theme_bw()
ggplotly()

# Load new library and highlight specific year
suppressMessages(library(PerformanceAnalytics))

# Create period to hold the 3 months of 2000
period <- c("2001-01/2001-03")

# Highlight the first three months of 2015 in light grey
chart.TimeSeries(cebu_ts
                 ,period.areas = period
                 ,period.color = "yellow")

# Decompose time series (make stationary)
cebu_ts_dec <- decompose(cebu_ts, "multiplicative")
plot(cebu_ts_dec)
autoplot(cebu_ts_dec) # try after decomposing

# Look at data using autocorrelation 
# Plot each ob against another ob at the same time
# This is a lag plot because you are plotting ts against itself
# Correlatins associated with the lag plots form (ACF) - ggAcf()
# Create a lag plot of the data
gglagplot(cebu_ts)

# Create an acf plot of the data
ggAcf(cebu_ts)

# Save the lag corresponding to the max autocorrelation
maxlag_cebu_ts <- 1

# Naive Forecasting Method 
#  - Simplest forecasting method is to use the most recent ob (Naive)
cb_naive <- naive(cebu_ts, h = 30)

# Plot and summarise the forecasts
autoplot(cb_naive) +
  labs(x = "Year",
       y = "# of Flight Entries")

summary(cb_naive)

# Check time series residuals
cebu_ts %>%
  naive() %>% 
  checkresiduals()

########################################################
# Fitting an ARIMA model to the data
#   Exponential smoothing methods are useful for forecats and make no assumptions
#   about the correlations between successive values of the time series.

# Requires that the forecast errors are uncorrelated and are normally distributed
#   with a mean of zero and constant variance.

# Plot the sample P/ACF pair of the differenced data 
acf2(diff(cebu_ts))

# Fit an ARIMA(1,1,1) model to our data
sarima(cebu_ts, p = 1, d = 1, q = 1)

# Fit an ARIMA(0,1,2) model to globtemp. Which model is better?
sarima(cebu_ts, p = 0, d = 1, q = 2)

# Based on our 2 models, we find that ARIMA(1,1,1) 
#   is the better model when considering BIC and AIC.

# Forecast with the better ARIMA model (40 Months)
sarima.for(cebu_ts
           ,n.ahead = 40
           ,p = 1
           ,d = 1
           ,q = 1) 
# The advantage of ARIMA forecasting is it’s readability

## Auto ARIMA Modeling
# auto.arima() usually if data is not stationary

# Since data is stationary; no need for auto.arima()
ndiffs(cebu_ts)
# Data only needs lag = 1 differencing in order for it to become stationary.
cebu_ts_diff <- diff(cebu_ts)

# Plot differenced data
autoplot(cebu_ts_diff)

# Check stationarity of differenced data
adf.test(cebu_ts_diff)
ts_arima <- auto.arima(cebu_ts)
ts_arima

# Make forecast for 2017
ts_forecast <- forecast(ts_arima,12)

# Plot forecast
autoplot(ts_forecast)+
  ylab(label = "No of Entry Flights")


