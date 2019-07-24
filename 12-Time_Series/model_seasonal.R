# https://petolau.github.io/Regression-trees-for-forecasting-time-series-in-R/

# Start new
rm(list = ls())

# Load external themes
source("theme_custom.R")

# Steps
# decompose double-seasonal time series
# detrend time series
# model and forecast double-seasonal time series with trend
# use two types of simple regression trees
# set important hyperparameters related to regression tree

# Load libraries
if (require(readr) == FALSE)      {install.packages("readr")}; library(readr)         # Load our data
if (require(dplyr) == FALSE)      {install.packages("dplyr")}; library(dplyr)         # Data Wrangling
if (require(ggplot2) == FALSE)    {install.packages("ggplot2")}; library(ggplot2)     # Visualization
if (require(ggfortify) == FALSE)  {install.packages("ggfortify")}; library(ggfortify) # Visualization
if (require(tseries) == FALSE)    {install.packages("tseries")}; library(tseries)     # Statistical Tests for Time Series data
if (require(forecast) == FALSE)   {install.packages("forecast")}; library(forecast)   # Time Series Forecasting
if (require(data.table) == FALSE) {install.packages("data.table")}; library(data.table) 
if (require(rpart) == FALSE)      {install.packages("rpart")}; library(rpart) 
if (require(rpart.plot) == FALSE) {install.packages("rpart.plot")}; library(rpart.plot) 
if (require(party) == FALSE)      {install.packages("party")}; library(party) 
if (require(ggforce) == FALSE)    {install.packages("ggforce")}; library(ggforce) 
if (require(plotly) == FALSE)     {install.packages("plotly")}; library(plotly) 
if (require(grid) == FALSE)       {install.packages("grid")}; library(grid) 
if (require(animation) == FALSE)  {install.packages("animation")}; library(animation) 

# Load data
data <- read.csv("data.csv")

# Time series: 48
n_date <- unique(as.Date(data$date, format = "%m/%d/%Y")) # use format or error
period <- 48

# Update factor to date
data$date <- as.Date(data$date, format = "%m/%d/%Y")
class(data$date)

# Train and test
train <- data[data$date %in% n_date[43:64],] # as.data.frame(n_date[43:63], col.names = c("date"))
test <- data[data$date %in% n_date[65],]
colnames(train) <- c("value","date_time","date","week_num","weekday","temp","humidity","pressure")
colnames(test) <- c("value","date_time","date","week_num","weekday","temp","humidity","pressure")

# data %>%
#   filter(as.Date(date, format = "%m/%d/%Y") == c("2016-06-13"))
  # filter(date == c("2016-06-13"))

# Plot electricity load
ggplot(data = train, aes(x = date, y = value)) +
  geom_line() +
  theme_ts()

# Using stl() for visualization. STL decomp is based on "loess" and decomposes ts on: seasonal, trend, and remainder
data_ts <- ts(train$value, freq = period * 7)
decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)$time.series

decomp_stl <- data.table(Load = c(train$value, as.numeric(decomp_ts)),
                         Date = rep(train[,train$date_time], ncol(decomp_ts)+1),
                         Type = factor(rep(c("original data", colnames(decomp_ts)),
                                           each = nrow(decomp_ts)),
                                       levels = c("original data", colnames(decomp_ts))))




