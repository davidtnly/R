## Generate level-one dataset for training the ensemble metalearner
library(h2o)
library(readxl)
library(ggplot2)
library(corrplot)
library(tidyverse)
library(caret)
library(xgboost)
library(gridExtra)

# # Load data
# data <- read_excel("data.xlsx")
# data <- data.frame(data)
# 
# 
# # Create training set indices with 80% of data
# set.seed(100)  # For reproducibility
# inTrain <- createDataPartition(y = data$PE, 
#                                p = 0.6, list = FALSE)
# 
# train <- data[inTrain,]
# test <- data[-inTrain,]
# inTest <- createDataPartition(y = test$PE, 
#                               p = 0.5, list = FALSE)
# test <- test[inTest,]
# validation <- test[-inTest,]

# # Change data to a mtrix
# x_train <- xgb.DMatrix(as.matrix(train[,-5]))
# y_train <- train[,5]
# x_test <- xgb.DMatrix(as.matrix(test[,-5]))
# y_test <- test[,5]
# 
# # Create grid and control parameters
# xgb_trcontrol <- trainControl(
#   method = "repeatedcv",
#   repeats = 3,
#   number = 5,
#   allowParallel = TRUE,
#   returnData = TRUE,
#   verboseIter = TRUE
# )
# 
# ctrl_binary <- trainControl(
#   method = "repeatedcv",
#   repeats = 3,
#   number = 5,
#   allowParallel = TRUE,
#   returnData = TRUE,
#   verboseIter = TRUE,
#   summaryFunction = twoClassSummary,
#   classProbs = TRUE
# )
# 
# xgb_Grid <- expand.grid(nrounds = 500,
#                         max_depth = seq(4,6,1),
#                         colsample_bytree = seq(0.8, 0.9, length.out = 5),
#                         eta = c(0,1,0.2,0.3),
#                         gamma = 0,
#                         min_child_weight = 1,
#                         subsample = 0.9
# )
# 
# set.seed(11)
# fit_xgb <- train(
#   x = x_train,
#   y = y_train,
#   trControl = xgb_trcontrol,
#   tuneGrid = xgb_Grid,
#   method = "xgbTree"
# )
# 
# # Find best parameter
# fit_xgb$bestTune
# fit_xgb$finalModel

h2o.init(nthreads = -1)  # Start an H2O cluster with nthreads = num cores on your machine
h2o.removeAll() # (Optional) Remove all objects in H2O cluster
set.seed(100)
train <- h2o::h2o.importFile("https://s3.amazonaws.com/erin-data/higgs/higgs_train_5k.csv")
test <- h2o::h2o.importFile("https://s3.amazonaws.com/erin-data/higgs/higgs_test_5k.csv")
y <- "response"
x <- setdiff(names(train), y)
family <- "binomial"

train[,y] <- as.factor(train[,y])  
test[,y] <- as.factor(test[,y])
 
# Specify Base Learners & Metalearner
learner <- c("h2o.glm.wrapper", "h2o.randomForest.wrapper", 
             "h2o.gbm.wrapper", "h2o.deeplearning.wrapper")

metalearner <- "h2o.glm.wrapper"

# Train an ensemble
fit_ensemble <- h2o.stackedEnsemble(
                    x = x,
                    y = y,
                    training_frame = train,
                    family = family,
                    learner = learner,
                    metalearner = "h2o.glm.wrapper",
                    cvControl = list(V = 5)
                    )

perf <- h2o.ensemble_performance(fit, newdata = test)
perf
print(perf, metric = "MSE")
pred <- predict(fit, newdata = test)
predictions <- as.data.frame(pred$pred)[,3]  #third column is P(Y==1)
labels <- as.data.frame(test[,y])[,1]

h2o.shutdown()







# predXGB <- predict(xgb_model, newdata = test)
# predrf <- predict(fit.rf, newdata = test)
