# Data
# https://archive.ics.uci.edu/ml/datasets/Combined+Cycle+Power+Plant#

# Remove all data
rm(list = ls())

# Load libraries
library(readxl)
library(ggplot2)
library(corrplot)
library(tidyverse)
library(caret)
library(xgboost)
library(gridExtra)

# Load data
data <- read_excel("data.xlsx")
data <- data.frame(data)

# Structure of data
class(data)
dim(data)
str(data)

# Check for NAs
colSums(sapply(data, is.na))
head(data)
map(data, class)

## Variable Names
# AT = Atmospheric Temperature in C
# V = Exhaust Vacuum Speed
# AP = Atmospheric Pressure
# RH = Relative Humidity
# PE = Power Output

# Visualize
ggplot(data, aes(x = AT, y = PE)) + 
  geom_point(alpha = 0.2, color = "blue") +
  labs(title = "Temp vs. Power Output",
       x = "Temperature",
       y = "Power Output"
       ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold", lineheight = 0.5),
        plot.subtitle = element_text(hjust = 0.5, lineheight = 0.5, face = "italic"),
        plot.background = element_rect(colour = "gray", fill = "gray"),
        axis.title = element_text(face = "bold"),
        axis.text.y = element_text(colour = "white", face = "bold", size = 12),
        axis.text.x = element_text(colour = "white", face = "bold", size = 12),
        panel.background = element_rect(colour = "gray", fill = "gray"),
        panel.grid.major.x = element_line(colour = "white"),
        panel.grid.major.y = element_line(colour = "white")
        )

# Exhaust Vacuum Speed vs. Power Output
ggplot(data, aes(x = V, y = PE)) + 
  geom_point(alpha = 0.3, color = "blue") +
  labs(title = "Exhaust Vacuum Speed vs. Power Output",
       x = "Exhaust Vacuum Speed",
       y = "Power Output"
  ) +
  geom_smooth(method = "lm", colour = "red", linetype = 2, size = 1, level = 0.90) +
  # geom_smooth(method = "loess", se = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold", lineheight = 0.5),
        plot.subtitle = element_text(hjust = 0.5, lineheight = 0.5, face = "italic"),
        plot.background = element_rect(colour = "darkgray", fill = "darkgray"),
        axis.title = element_text(face = "bold"),
        axis.text.y = element_text(colour = "white", face = "bold", size = 12),
        axis.text.x = element_text(colour = "white", face = "bold", size = 12),
        panel.background = element_rect(colour = "darkgray", fill = "darkgray"),
        panel.grid.major.x = element_line(colour = "white"),
        panel.grid.major.y = element_line(colour = "white")
  ) +
  scale_y_continuous(breaks = seq(410,520,20), limits = c(410,520)) +
  scale_x_continuous(breaks = seq(20,100,10), limits = c(20,90))

# Atmospheric Pressure vs. Power Output
ggplot(data, aes(x = AP, y = PE)) + 
  geom_point(alpha = 0.3, color = "blue") +
  labs(title = "Atmospheric Pressure vs. Power Output",
       x = "Atmospheric Pressure",
       y = "Power Output"
  ) +
  geom_smooth(method = "lm", colour = "red", linetype = 2, size = 1, level = 0.90) +
  # geom_smooth(method = "loess", se = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold", lineheight = 0.5),
        plot.subtitle = element_text(hjust = 0.5, lineheight = 0.5, face = "italic"),
        plot.background = element_rect(colour = "darkgray", fill = "darkgray"),
        axis.title = element_text(face = "bold"),
        axis.text.y = element_text(colour = "white", face = "bold", size = 12),
        axis.text.x = element_text(colour = "white", face = "bold", size = 12),
        panel.background = element_rect(colour = "darkgray", fill = "darkgray"),
        panel.grid.major.x = element_line(colour = "white"),
        panel.grid.major.y = element_line(colour = "white")
  )      

# Humidity vs. Power Output
ggplot(data, aes(x = RH, y = PE)) + 
  geom_point(alpha = 0.3, color = "blue") +
  labs(title = "Humidity vs. Power Output",
       x = "Humidity",
       y = "Power Output"
  ) +
  geom_smooth(method = "lm", colour = "red", linetype = 2, size = 1, level = 0.90) +
  # geom_smooth(method = "loess", se = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold", lineheight = 0.5),
        plot.subtitle = element_text(hjust = 0.5, lineheight = 0.5, face = "italic"),
        plot.background = element_rect(colour = "darkgray", fill = "darkgray"),
        axis.title = element_text(face = "bold"),
        axis.text.y = element_text(colour = "white", face = "bold", size = 12),
        axis.text.x = element_text(colour = "white", face = "bold", size = 12),
        panel.background = element_rect(colour = "darkgray", fill = "darkgray"),
        panel.grid.major.x = element_line(colour = "white"),
        panel.grid.major.y = element_line(colour = "white")
  )      

# Correlations
correlations <- cor(data, use = "complete.obs")
corrplot(correlations, method = "color")
corrplot.mixed(correlations,
         order = "hclust",
         sig.level = 0.05,
         tl.cex = 0.75,
         tl.col = "black",
         tl.srt = 45,
         upper = "color",
         lower = "number"
)
corrplot(correlations,
         mar = c(0,0,1,0),
         method = "color",
         order = "hclust", # Hierarchical clustering
         # p.mat = p_mat,    # Marks insignificant cols
         insig = "blank",  # Mark type
         addgrid = TRUE,
         # addCoef.col = "black", # Add coefficient of correlation
         sig.level = 0.05,
         type = "lower", 
         tl.cex = 0.75,    # Text Size
         tl.col = "black", # Text color
         tl.srt = 45       # Text angle
)

# Correlation pearson test
cor.test(data$RH, data$PE, method = "pearson") # low
# with(data, cor.test(RH, PE), method = "pearson")
cor.test(data$AP, data$PE, method = "pearson") # moderate
cor.test(data$AT, data$PE, method = "pearson") # high
cor.test(data$V, data$PE, method = "pearson") # high

# Data modeling
# Create training set indices with 80% of data
set.seed(100)  # For reproducibility
# Create index for testing and training data
inTrain <- createDataPartition(y = data$PE, 
                               p = 0.6, list = FALSE)

train <- data[inTrain,]
test <- data[-inTrain,]

inTest <- createDataPartition(y = test$PE, 
                               p = 0.5, list = FALSE)
test <- test[inTest,]
validation <- test[-inTest,]

# Size ratio of training and test dataset
message("As shown below, the training set is about 80%  and the test set is about 20% of the original data")

rbind("Training set" = nrow(train)/nrow(data),
      "Testing set" = nrow(test)/nrow(data)) %>% 
  round(2)*100

## LM model
# Fit linear regression model
# put the predictors on the same scale: mean of zero and unit variance
lm_model = train(train[,1:4], train[,5],
              method = "lm",
              preProc = c("center", "scale")
)
message("Linear Regression: Model performance on \n the training set")
lm_model$results[c("RMSE","Rsquared")] %>%
  round(2)
summary(lm_model)

# Predict
pred = predict(lm_model, test[, 1:4])
SSE_lm = sum((test[,5] - pred)^2)    # sum of squared errors
SST_lm = sum((test[,5] - mean(train[,5]))^2) # total sum of squares, remember to use training data here
R_square = 1 - (SSE_lm/SST_lm)
message('R_squared on the test data:')
round(R_square, 2)
RMSE_lm = sqrt(SSE_lm/length(pred))
message("Root mean square error on the test data: ")
round(RMSE_lm, 2)

# Output
output_data = as.data.frame(cbind(predicted = pred,
                              observed = test$PE))

# Plot predictions vs test data
p1 <- ggplot(output_data, aes(predicted, observed)) +
  geom_point(color = "darkred", alpha = 0.25) + 
  geom_smooth(method = "lm") + 
  geom_smooth(method = "loess", color = "green", linetype = 2) +
  ggtitle('Linear Regression ') +
  ggtitle("Linear Regression: Prediction vs Test Data") +
  xlab("Predicted Power Output ") +
  ylab("Observed Power Output") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold", lineheight = 0.5),
        plot.subtitle = element_text(hjust = 0.5, lineheight = 0.5, face = "italic"),
        plot.background = element_rect(colour = "darkgray", fill = "darkgray"),
        axis.title = element_text(face = "bold"),
        axis.text.y = element_text(colour = "white", face = "bold", size = 12),
        axis.text.x = element_text(colour = "white", face = "bold", size = 12),
        panel.background = element_rect(colour = "darkgray", fill = "darkgray"),
        panel.grid.major.x = element_line(colour = "white"),
        panel.grid.major.y = element_line(colour = "white")
  )   
p1

## xgb model

# Change data to a mtrix
x_train <- xgb.DMatrix(as.matrix(train[,-5]))
y_train <- train[,5]
x_test <- xgb.DMatrix(as.matrix(test[,-5]))
y_test <- test[,5]

# Create grid and control parameters
xgb_trcontrol <- trainControl(
  method = "repeatedcv",
  repeats = 3,
  number = 5,
  allowParallel = TRUE,
  returnData = TRUE,
  verboseIter = TRUE
)

ctrl_binary <- trainControl(
  method = "repeatedcv",
  repeats = 3,
  number = 5,
  allowParallel = TRUE,
  returnData = TRUE,
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,
  classProbs = TRUE
)

xgb_Grid <- expand.grid(nrounds = 500,
                        max_depth = 6, # seq(4,6,1),
                        colsample_bytree = 0.8, # seq(0.8, 0.9, length.out = 5),
                        eta = 0.2, # c(0,1,0.2,0.3),
                        gamma = 0,
                        min_child_weight = 1,
                        subsample = 0.9
                        )

set.seed(11)
xgb_model <- train(
  x = x_train,
  y = y_train,
  trControl = xgb_trcontrol,
  tuneGrid = xgb_Grid,
  method = "xgbTree"
)

# Find best parameter
xgb_model$bestTune
xgb_model$finalModel

# Model evaluation
predicted <- predict(xgb_model, x_test)
residuals <- y_test - predicted # (x- x(pred)) - Difference
RMSE = sqrt(mean(residuals^2))
cat("The root mean square error of the test data is ", round(RMSE,3),"\n")

# Calculate total sum of squares
y_test_mean = mean(y_test)
tss <- sum((y_test - y_test_mean)^2)

# Calculate residual sum of squares
rss <- sum(residuals^2)

# Calculate r-squared
rsq <- 1 - (rss/tss)
cat("The r-square of the test data is ", round(rsq,3), "\n")

# Plot actual vs. predicted
new_data <- as.data.frame(cbind(predicted = predicted,
                                observed = y_test))

p2 <- ggplot(new_data, aes(x = predicted, y = observed)) +
  geom_point(color = "darkred", alpha = 0.25) +
  geom_smooth(method = "lm") +
  ggtitle("XGB: Predicted vs. Test Data") +
  xlab("Predicted Power Output ") +
  ylab("Observed Power Output") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold", lineheight = 0.5),
        plot.subtitle = element_text(hjust = 0.5, lineheight = 0.5, face = "italic"),
        plot.background = element_rect(colour = "darkgray", fill = "darkgray"),
        axis.title = element_text(face = "bold"),
        axis.text.y = element_text(colour = "white", face = "bold", size = 12),
        axis.text.x = element_text(colour = "white", face = "bold", size = 12),
        panel.background = element_rect(colour = "darkgray", fill = "darkgray"),
        panel.grid.major.x = element_line(colour = "white"),
        panel.grid.major.y = element_line(colour = "white")
  )   
p2

# Random forest model
rfcontrol <- trainControl(
  method = "repeatedcv",
  number = 3, # 10
  repeats = 3,
  savePredictions = TRUE
)
rfgrid <- expand.grid(.mtry = 2) #c(1:5))
rf_model <- train(PE ~ .,
                  data = train,
                  method = "rf",
                  metric = "RMSE",
                  trControl = rfcontrol,
                  tuneGrid = rfgrid,
                  ntree = 20, # 500
                  importance = TRUE
                  )

print(rf_model)
# plot(rf_model)

# Predict values
prediction_rf <- predict(rf_model, newdata = test)

# Manually calculate eval metrics
# Residual
resid_rf <- test$PE - prediction_rf # Difference

# Root mean squared error
rmse_rf <- sqrt(mean(resid_rf^2))

# R-squared
mean_test_y <- mean(test$PE) # mean of indepdendent var (y)
tss_rf <- sum((test$PE - mean_test_y)^2)
rss_rf <- sum(resid_rf^2)
rsq_rf <- 1 - (rss_rf/tss_rf)


# Lm model
cat("LM: The root mean square error of the test data is ", round(RMSE_lm,3),"\n")
cat("LM: The r-square of the test data is ", round(R_square,3), "\n")

# Xgb model
cat("XGB: The root mean square error of the test data is ", round(RMSE,3),"\n")
cat("XGB: The r-square of the test data is ", round(rsq,3), "\n")

# # RF model
# cat("XGB: The root mean square error of the test data is ", round(rmse_rf,3),"\n")
# cat("XGB: The r-square of the test data is ", round(rsq_rf,3), "\n")

# grid.arrange(p1,p2,p3,ncol = 1)

# RMSE: indicates the absolute fit of the model to the data–how close 
#         the observed data points are to the model’s predicted values.
#  It is the most important criterion for fit if the main purpose of the model is prediction
#  RMSE (less straightforward) has the benefit of penalizing large errors vs. MAE

# More models
seed <- 7
metric <- "Accuracy"
preProcess = c("center", "scale")
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, returnData = TRUE, allowParallel = TRUE, verboseIter = TRUE)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, summaryFunction = twoClassSummary, classProbs = TRUE)
svm_grid <- expand.grid(C = c(0.25, 0.50, 0.75, 1, 1.25),
                        scale = c(.001, 0.01, 0.1))

# Logistic Regression
set.seed(seed)
fit.glm <- train(PE ~ ., data = train, method = "glm", trControl = control)
# GLMNET (elastic net)
set.seed(seed)
fit.glmnet <- train(PE ~ ., data = train, method = "glmnet", preProc = c("center", "scale"), trControl = control)
# SVM Radial
set.seed(seed)
fit.svmRadial <- train(PE ~ ., data = train, method = "svmRadial", preProc = c("center", "scale"), trControl = control, fit = FALSE)
# SVM Linear
set.seed(seed)
fit.svmLinear <- train(PE ~ ., data = train, method = "svmLinear", preProc = c("center", "scale"), trControl = control, fit = FALSE)
# kNN
set.seed(seed)
fit.knn <- train(PE ~ ., data = train, method = "knn", preProc = c("center", "scale"), trControl = control)
# CART
set.seed(seed)
fit.cart <- train(PE ~ ., data = train, method = "rpart", trControl = control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(PE ~ ., data = train, method = "gbm", trControl = control, verbose = FALSE)
# Random Forest 2
set.seed(seed)
fit.rf <- train(PE ~ ., data = train, method = "rf", trControl = control, verbose = FALSE)
# Lasso
set.seed(seed)
fit.lasso <- train(PE ~ ., data = train, method = "lasso", preProc = c("center", "scale"), trControl = control)
# Ridge
set.seed(seed)
fit.ridge <- train(PE ~ ., data = train, method = "ridge", preProc = c("center", "scale"), trControl = control)
#############################################################################################################################
# Bagged MARS
set.seed(seed)
fit.bagEarth <- train(PE ~ ., data = train, method = "bagEarth", preProc = c("center", "scale"),  trControl = control) # y = y_train, x = as.matrix(train)
# Boosted Generalized Linear Model
set.seed(seed)
fit.glmboost <- train(PE ~ ., data = train, method = "glmboost", preProc = c("center", "scale"), trControl = control) # y = y_train, x = as.matrix(train)
# eXtreme Gradient Boosting
set.seed(seed)
fit.xgbDart <- train(PE ~ ., data = train, method = "xgbDART", preProc = c("center", "scale"), trControl = control) # y = y_train, x = as.matrix(train)
# Least Angle Regression
set.seed(seed)
fit.lars <- train(PE ~ ., data = train, method = "lars", preProc = c("center", "scale"), trControl = control) # y = y_train, x = as.matrix(train)

# Save
save(xgb_model,file = "xgb_model.rda")
save(lm_model,file = "lm_model.rda")
save(fit.glm,file = "fit.glm.rda")
save(fit.glmnet,file = "fit.glmnet.rda")
save(fit.svmRadial,file = "fit.svmRadial.rda")
save(fit.svmLinear,file = "fit.svmLinear.rda")
save(fit.knn,file = "fit.knn.rda")
save(fit.cart,file = "fit.cart.rda")
save(fit.gbm,file = "fit.gbm.rda")
save(fit.rf,file = "fit.rf.rda")
save(rf_model,file = "rf_model.rda")
save(fit.lasso,file = "fit.lasso.rda")
save(fit.ridge,file = "fit.ridge.rda")
save(fit.bagEarth,file = "fit.bagEarth.rda")
save(fit.glmboost,file = "fit.glmboost.rda")
save(fit.xgbDart,file = "fit.xgbDart.rda")
save(fit.lars,file = "fit.lars.rda")

# MSE Function
mse <- function(x) 
  mean(x$residuals^2)

## Ensemble
# Predict
prediction_svmrad <- predict(fit.svmRadial, test)
prediction_gbm <- predict(fit.gbm, test)
prediction_rf2 <- predict(fit.rf, test)
prediction_lasso <- predict(fit.lasso, test)
prediction_ridge <- predict(fit.ridge, test)
prediction_glmnet <- predict(fit.glmnet, test)
prediction_bagEarth <- predict(fit.bagEarth, test)
prediction_glmboost <- predict(fit.glmboost, test)
prediction_xgbDart <- predict(fit.xgbDart, test)
prediction_lars <- predict(fit.lars, test)

# Evaluate
resid_lm <- test$PE - pred
resid_svmrad <- test$PE - prediction_svmrad
resid_gbm <- test$PE - prediction_gbm
resid_rf2 <- test$PE - prediction_rf2
resid_lasso <- test$PE - prediction_lasso
resid_ridge <- test$PE - prediction_ridge
resid_glmnet <- test$PE - prediction_glmnet
resid_bagEarth <- test$PE - prediction_bagEarth
resid_glmboost <- test$PE - prediction_glmboost
resid_xgbDart <- test$PE - prediction_xgbDart
resid_lars <- test$PE - prediction_lars

# RMSE
rmse_svmrad <- sqrt(mean(resid_svmrad^2))
rmse_gbm <- sqrt(mean(resid_gbm^2))
rmse_rf2 <- sqrt(mean(resid_rf2^2))
rmse_lasso <- sqrt(mean(resid_lasso^2))
rmse_ridge <- sqrt(mean(resid_ridge^2))
rmse_glmnet <- sqrt(mean(resid_glmnet^2))
rmse_bagEarth <- sqrt(mean(resid_bagEarth^2))
rmse_glmboost <- sqrt(mean(resid_glmboost^2))
rmse_xgbDart <- sqrt(mean(resid_xgbDart^2))
rmse_lars <- sqrt(mean(resid_lars^2))

# TSS
mean_test_y <- mean(test$PE)
tss_svmrad <- sum((test$PE - mean_test_y)^2)
tss_gbm <- sum((test$PE - mean_test_y)^2)
tss_rf2 <- sum((test$PE - mean_test_y)^2)
tss_lasso <-sum((test$PE - mean_test_y)^2)
tss_ridge <-sum((test$PE - mean_test_y)^2)
tss_glmnet <-sum((test$PE - mean_test_y)^2)
tss_bagEarth <-sum((test$PE - mean_test_y)^2)
tss_glmboost <-sum((test$PE - mean_test_y)^2)
tss_xgbDart <-sum((test$PE - mean_test_y)^2)
tss_lars <-sum((test$PE - mean_test_y)^2)

# RSS
rss_svmrad <- sum(resid_svmrad^2)
rss_gbm <- sum(resid_gbm^2)
rss_rf2 <- sum(resid_rf2^2)
rss_lasso <-sum(resid_lasso^2)
rss_ridge <-sum(resid_ridge^2)
rss_glmnet <-sum(resid_glmnet^2)
rss_bagEarth <-sum(resid_bagEarth^2)
rss_glmboost <-sum(resid_glmboost^2)
rss_xgbDart <-sum(resid_xgbDart^2)
rss_lars <-sum(resid_lars^2)

# Rsquared
rsq_svmrad <- 1 - (rss_svmrad/tss_svmrad)
rsq_gbm <- 1 - (rss_gbm/tss_gbm)
rsq_rf2 <- 1 - (rss_rf2/tss_rf2)
rsq_lasso <- 1- (rss_lasso/tss_lasso)
rsq_ridge <- 1- (rss_ridge/tss_ridge)
rsq_glmnet <- 1- (rss_glmnet/tss_glmnet)
rsq_bagEarth <- 1- (rss_bagEarth/tss_bagEarth)
rsq_glmboost <- 1- (rss_glmboost/tss_glmboost)
rsq_xgbDart <- 1- (rss_xgbDart/tss_xgbDart)
rsq_lars <- 1- (rss_lars/tss_lars)

# Avg
combined_data <- as.data.frame(cbind(new_data,prediction_rf2))
combined_data$avg_pred <- (combined_data$predicted+combined_data$prediction_rf2)/2
resid_combed <- combined_data$observed - combined_data$avg_pred
rmse_combed <- sqrt(mean(resid_combed^2))
tss_combed <- sum((combined_data$observed - mean_test_y)^2)
rss_combed <- sum(resid_combed^2)
rsq_combined <- 1- (rss_combed/tss_combed)

# MSE
mse_lm <- mean((resid_lm)^2) # mean(x$residuals^2)
mse_svmrad <- mean((resid_svmrad)^2)
mse_gbm <- mean((resid_gbm)^2)
mse_rf2 <- mean((resid_rf2)^2)
mse_rf <- mean((resid_rf)^2)
mse_lasso <- mean((resid_lasso)^2)
mse_ridge <- mean((resid_ridge)^2)
mse_xgb <- mean((residuals)^2)
mse_ensemble <- mean((resid_combed)^2)
mse_glmnet <- mean((resid_glmnet)^2)
mse_bagEarth <- mean((resid_bagEarth)^2)
mse_glmboost <- mean((resid_glmboost)^2)
mse_xgbDart <- mean((resid_xgbDart)^2)
mse_lars <- mean((resid_lars)^2)

# Results in a table
results <- resamples(list(logistic = fit.glm, glmnet = fit.glmnet,
                          svm_rad = fit.svmRadial, svm_lin = fit.svmLinear, cart = fit.cart, 
                          gbm = fit.gbm, rf_mod = fit.rf, lasso = fit.lasso, ridge = fit.ridge, elasticnet = fit.glmnet,
                          bagEarth = fit.bagEarth, glmboost = fit.glmboost, xgbDart = fit.xgbDart, lars = fit.lars))
# Table comparison
summary(results)
# boxplot comparison df[, c("name1", "name2", "name3")]
bwplot(results)
# Dot-plot comparison
dotplot(results)

# correlation between results
modelCor(results)
splom(results)

# Lm model
cat("LM: The root mean square error of the test data is ", round(RMSE_lm,3),"\n")
cat("LM: The r-square of the test data is ", round(R_square,3), "\n")

# Xgb model
cat("XGB: The root mean square error of the test data is ", round(RMSE,3),"\n")
cat("XGB: The r-square of the test data is ", round(rsq,3), "\n")

# RF model
cat("RF: The root mean square error of the test data is ", round(rmse_rf,3),"\n")
cat("RF: The r-square of the test data is ", round(rsq_rf,3), "\n")

# Svmrad model
cat("SVMRad: The root mean square error of the test data is ", round(rmse_svmrad,3),"\n")
cat("SVMRad: The r-square of the test data is ", round(rsq_svmrad,3), "\n")

# GBM model
cat("GBM: The root mean square error of the test data is ", round(rmse_gbm,3),"\n")
cat("GBM: The r-square of the test data is ", round(rsq_gbm,3), "\n")

# RF2 model
cat("RF2: The root mean square error of the test data is ", round(rmse_rf2,3),"\n")
cat("RF2: The r-square of the test data is ", round(rsq_rf2,3), "\n")

# Lasso model
cat("Lasso: The root mean square error of the test data is ", round(rmse_lasso,3),"\n")
cat("Lasso: The r-square of the test data is ", round(rsq_lasso,3), "\n")

# ridge model
cat("ridge: The root mean square error of the test data is ", round(rmse_ridge,3),"\n")
cat("ridge: The r-square of the test data is ", round(rsq_ridge,3), "\n")

# avg ensemble model
cat("ensemble: The root mean square error of the test data is ", round(rmse_combed,3),"\n")
cat("ensemble: The r-square of the test data is ", round(rsq_combined,3), "\n")

# elastic net model
cat("elastic net: The root mean square error of the test data is ", round(rmse_glmnet,3),"\n")
cat("elastic net: The r-square of the test data is ", round(rsq_glmnet,3), "\n")

# bagEarth model
cat("bagEarth: The root mean square error of the test data is ", round(rmse_bagEarth,3),"\n")
cat("bagEarth: The r-square of the test data is ", round(rsq_bagEarth,3), "\n")

# glmBoost model
cat("glmBoost: The root mean square error of the test data is ", round(rmse_glmboost,3),"\n")
cat("glmBoost: The r-square of the test data is ", round(rsq_glmboost,3), "\n")

# xGB Dart model
cat("xGB Dart: The root mean square error of the test data is ", round(rmse_xgbDart,3),"\n")
cat("xGB Dart: The r-square of the test data is ", round(rsq_xgbDart,3), "\n")

# Lars model
cat("Lars: The root mean square error of the test data is ", round(rmse_lars,3),"\n")
cat("Lars: The r-square of the test data is ", round(rsq_lars,3), "\n")

# Create table of values
rmse_list <- c(RMSE_lm, RMSE, rmse_rf, rmse_svmrad, rmse_gbm, rmse_rf2, rmse_lasso, rmse_ridge, rmse_combed, rmse_glmnet, rmse_bagEarth, rmse_glmboost, rmse_xgbDart, rmse_lars )
rsq_list <- c(R_square, rsq, rsq_rf, rsq_svmrad, rsq_gbm, rsq_rf2, rsq_lasso, rsq_ridge, rsq_combined, rsq_glmnet, rsq_bagEarth, rsq_glmboost, rsq_xgbDart, rsq_lars)
mse_list <- c(mse_lm, mse_xgb, mse_rf, mse_svmrad, mse_gbm, mse_rf2, mse_lasso, mse_ridge, mse_ensemble, mse_glmnet, mse_bagEarth, mse_glmboost, mse_xgbDart, mse_lars)

result_df <- as.data.frame(cbind(rmse_list, rsq_list, mse_list))
colnames(result_df) <- c("RMSE", "Rsq", "MSE")
rownames(result_df) <- c("Linear Regression", "XGradient Boost", "Random Forest", "SVM Radial", "Gradient Boost", "Random Forest 2", "LASSO", "Ridge", "Ensemble", "Elastic Net"
                         ,"bagEarth", "glmboost", "xgb Dart", "LARS")

# Reorder by lowest error
result_df[order(result_df[,1] ),]

# Plot actual vs. predicted
svmrad_data <- as.data.frame(cbind(predicted = prediction_svmrad,
                               observed = y_test))
gbm_data <- as.data.frame(cbind(predicted = prediction_gbm,
                               observed = y_test))
rf_data <- as.data.frame(cbind(predicted = prediction_rf2,
                                observed = y_test))
ridge_data <- as.data.frame(cbind(predicted = prediction_ridge,
                               observed = y_test))
ensemble_data <- as.data.frame(cbind(predicted = combined_data$avg_pred,
                                  observed = y_test))

p4 <- ggplot(svmrad_data, aes(x = predicted, y = observed)) +
  geom_point(color = "darkred", alpha = 0.25) +
  geom_smooth(method = "lm") +
  ggtitle("SVM Linear: Predicted vs. Test Data") +
  xlab("Predicted Power Output ") +
  ylab("Observed Power Output") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold", lineheight = 0.5),
        plot.subtitle = element_text(hjust = 0.5, lineheight = 0.5, face = "italic"),
        plot.background = element_rect(colour = "darkgray", fill = "darkgray"),
        axis.title = element_text(face = "bold"),
        axis.text.y = element_text(colour = "white", face = "bold", size = 12),
        axis.text.x = element_text(colour = "white", face = "bold", size = 12),
        panel.background = element_rect(colour = "darkgray", fill = "darkgray"),
        panel.grid.major.x = element_line(colour = "white"),
        panel.grid.major.y = element_line(colour = "white")
  )   

p5 <- ggplot(gbm_data, aes(x = predicted, y = observed)) +
  geom_point(color = "darkred", alpha = 0.25) +
  geom_smooth(method = "lm") +
  ggtitle("Gradient Boosting: Predicted vs. Test Data") +
  xlab("Predicted Power Output ") +
  ylab("Observed Power Output") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold", lineheight = 0.5),
        plot.subtitle = element_text(hjust = 0.5, lineheight = 0.5, face = "italic"),
        plot.background = element_rect(colour = "darkgray", fill = "darkgray"),
        axis.title = element_text(face = "bold"),
        axis.text.y = element_text(colour = "white", face = "bold", size = 12),
        axis.text.x = element_text(colour = "white", face = "bold", size = 12),
        panel.background = element_rect(colour = "darkgray", fill = "darkgray"),
        panel.grid.major.x = element_line(colour = "white"),
        panel.grid.major.y = element_line(colour = "white")
  )   

p6 <- ggplot(rf_data, aes(x = predicted, y = observed)) +
  geom_point(color = "darkred", alpha = 0.25) +
  geom_smooth(method = "lm") +
  ggtitle("RF2: Predicted vs. Test Data") +
  xlab("Predicted Power Output ") +
  ylab("Observed Power Output") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold", lineheight = 0.5),
        plot.subtitle = element_text(hjust = 0.5, lineheight = 0.5, face = "italic"),
        plot.background = element_rect(colour = "darkgray", fill = "darkgray"),
        axis.title = element_text(face = "bold"),
        axis.text.y = element_text(colour = "white", face = "bold", size = 12),
        axis.text.x = element_text(colour = "white", face = "bold", size = 12),
        panel.background = element_rect(colour = "darkgray", fill = "darkgray"),
        panel.grid.major.x = element_line(colour = "white"),
        panel.grid.major.y = element_line(colour = "white")
  )  

p7 <- ggplot(ridge_data, aes(x = predicted, y = observed)) +
  geom_point(color = "darkred", alpha = 0.25) +
  geom_smooth(method = "lm") +
  ggtitle("Ridge Regression: Predicted vs. Test Data") +
  xlab("Predicted Power Output ") +
  ylab("Observed Power Output") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold", lineheight = 0.5),
        plot.subtitle = element_text(hjust = 0.5, lineheight = 0.5, face = "italic"),
        plot.background = element_rect(colour = "darkgray", fill = "darkgray"),
        axis.title = element_text(face = "bold"),
        axis.text.y = element_text(colour = "white", face = "bold", size = 12),
        axis.text.x = element_text(colour = "white", face = "bold", size = 12),
        panel.background = element_rect(colour = "darkgray", fill = "darkgray"),
        panel.grid.major.x = element_line(colour = "white"),
        panel.grid.major.y = element_line(colour = "white")
  )  

p8 <- ggplot(ensemble_data, aes(x = predicted, y = observed)) +
  geom_point(color = "darkred", alpha = 0.25) +
  geom_smooth(method = "lm") +
  ggtitle("Ensemble: Predicted vs. Test Data") +
  xlab("Predicted Power Output ") +
  ylab("Observed Power Output") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold", lineheight = 0.5),
        plot.subtitle = element_text(hjust = 0.5, lineheight = 0.5, face = "italic"),
        plot.background = element_rect(colour = "darkgray", fill = "darkgray"),
        axis.title = element_text(face = "bold"),
        axis.text.y = element_text(colour = "white", face = "bold", size = 12),
        axis.text.x = element_text(colour = "white", face = "bold", size = 12),
        panel.background = element_rect(colour = "darkgray", fill = "darkgray"),
        panel.grid.major.x = element_line(colour = "white"),
        panel.grid.major.y = element_line(colour = "white")
  )  

grid.arrange(p1,p2,p4,p5,p6,p8,ncol = 2)

# Reorder by loweest error
result_df[order(result_df[,1] ),] 


