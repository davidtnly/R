# Start new
rm(list = ls())

# Load external scripts


# Load libraries
if (require(tidyverse) == FALSE)  {install.packages("tidyverse")}; library(tidyverse)
if (require(sweep) == FALSE)      {install.packages("sweep")}; library(sweep)   
if (require(timetk) == FALSE)     {install.packages("timetk")}; library(timetk)   
if (require(forecast) == FALSE)   {install.packages("forecast")}; library(forecast)  
if (require(data.table) == FALSE) {install.packages("data.table")}; library(data.table)  

# Load theme
theme_ts <- theme(panel.border = element_rect(fill = NA, colour = "grey10"),
                  panel.background = element_blank(),
                  panel.grid.minor = element_line(colour = "grey85"),
                  panel.grid.major = element_line(colour = "grey85"),
                  panel.grid.major.x = element_line(colour = "grey85"),
                  axis.text = element_text(size = 13, face = "bold"),
                  axis.title = element_text(size = 15, face = "bold"),
                  plot.title = element_text(size = 16, face = "bold"),
                  strip.text = element_text(size = 16, face = "bold"),
                  strip.background = element_rect(colour = "black"),
                  legend.text = element_text(size = 15),
                  legend.title = element_text(size = 16, face = "bold")
                  # legend.background = element_rect(fill = "white"),
                  # legend.key = element_rect(fill = "white")
)

# Load Data
data <- read.csv("Flights/2005_2006_flights.csv", stringsAsFactors = FALSE)
data_2007 <- read.csv("Flights/2007_flights.csv", stringsAsFactors = FALSE)

# Prep
class(data)
dim(data)
names(data)
glimpse(data)
head(data)
head(data_2007)

# Convert year_month to date format
data$date <- as.Date(str_c(data$year_month, "-1"), format = "%Y-%m-%d")
data_2007$date <- as.Date(str_c(data_2007$year_month, "-1"), format = "%Y-%m-%d")

# Verify
class(data$date)
head(data$date)
head(data_2007$date)

# Split training and testing data
train <- data
test <- data_2007

# Convert to ts
train_ts <- ts(train$count, freq = 12, start = c(2005,1))
test_ts <- ts(test$count, freq = 12, start = c(2007,1))

# Plot and Decompose to view TS
checkresiduals(train_ts) # Acf plots and normality looks good
# dc_train <- stl(train_ts, s.window = "periodic")
dc_train <- decompose(train_ts)
autoplot(dc_train) # Additive model

# Szn plots - any outliers?
ggseasonplot(train_ts, polar = TRUE)

# ARIMA Model
fit_arima <- auto.arima(train_ts)
fit_arima

# Forecast
fc_arima <- forecast(fit_arima, h = 12)
checkresiduals(fc_arima) # Looks good (0,0,0)

# Plot model against actual - Model does not capture any seasonality (straight line - underfitting model)
autoplot(train_ts, series = "Actual") +
  autolayer(test_ts, series = "Actual") +
  autolayer(fitted(fit_arima), series = "Fitted") +
  autolayer(fc_arima$mean, series = "Forecast")

# Try a ARIMA() model and add in seasonality
fit_arima2 <- Arima(train_ts, order = c(1,1,0), seasonal = list(order = c(1,0,0), period = 12))
fit_arima2
tsdisplay(residuals(fit_arima2))

# Forecast 2
fc_arima2 <- forecast(fit_arima2, h = 12)
checkresiduals(fc_arima2)
tsdisplay(residuals(fc_arima2))

# Plot new model
autoplot(train_ts, series = "Actual") +
  autolayer(test_ts, series = "Actual") +
  autolayer(fitted(fit_arima2), series = "Fitted") +
  autolayer(fc_arima2$mean, series = "Forecast")

### Looks like seasonality, is captured with the new parameters but seriously overfits
## Compare the 2 models with their AIC where fit_arima2 is lower

# How will it look with a 2 year forecast?
fc_arima3 <- forecast(fit_arima2, h = 24)
autoplot(train_ts, series = "Actual") +
  autolayer(test_ts, series = "Actual") +
  autolayer(fitted(fit_arima2), series = "Fitted") +
  autolayer(fc_arima3$mean, series = "Forecast")

### Results
## ARIMA model with an AR(1), MA(0), and differencing of 1 and seasonality of (1,0,0) produces an accurate 12mo forecast
## Since it's AR(1) (first-order) - it means that the coefficient returns to its mean fairly quickly 