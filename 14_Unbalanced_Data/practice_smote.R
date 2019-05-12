library(dplyr)
library(caret) 
library(DMwR)
library(purrr) 
library(pROC) 


# Create simulations (data) with a training and test set
# Include 20 variables and 10 noise variables
# Intercept arugmnet controls the overall level of class imbalance

set.seed(100)
train <- twoClassSim(1000,
                     intercept = -25,
                     linearVars = 20,
                     noiseVars = 10)

test  <- twoClassSim(1000,
                     intercept = -25,
                     linearVars = 20,
                     noiseVars = 10)

prop.table(table(train$Class))

##
# Class1 Class2 
# 0.9776 0.0224 
##

# Model using GBM (can handle potential non-linearities)
# cross-validation
ctrl <- trainControl(method = "repeatedcv",
                     number = 3,
                     repeats = 3,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE
                     )

# Build a standard classifier using GBM
set.seed(100)
gbmtune_model <- train(Class ~ .,
                       data = train,
                       method = "gbm",
                       verbose = FALSE,
                       metric = "ROC",
                       trControl = ctrl
                       )

# AUC
aucroc <- function(mod, data) {
  roc(data$Class,
      predict(model, data, type = "prob")[, "Class2"])
}

gbmtune_model %>% 
  aucroc(data = test) %>% 
  auc()

## Handling class imbalanced with weighted or sampling methods to improve AUC score
# Create model weights
model_weights <- ifelse(train$Class == "Class1",
                        (1/table(train$Class)[1]) * 0.5, # 0.001022495
                        (1/table(train$Class)[2]) * 0.5) # 0.04545455 

# Get seed
ctrl$seeds <- glmtune_model$control$seeds

# Build weighted model
gbm_weighted_model <- train(Class ~ .,
                            data = train,
                            method = "gbm",
                            verbose = FALSE,
                            weights = model_weights,
                            metric = "ROC",
                            trControl = ctrl
                            )

# Build down-sampled model
ctrl$sampling <- "down"
down_fit <- train(Class ~ .,
                  data = train,
                  method = "gbm",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl
                  )

# Build up-sampled model
ctrl$sampling <- "up"
up_fit <- train(Class ~ .,
                data = train,
                method = "gbm",
                verbose = FALSE,
                metric = "ROC",
                trControl = ctrl
                )

# Build smote model
ctrl$sampling <- "smote"
smote_fit <- train(Class ~ .,
                   data = train,
                   method = "gbm",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = ctrl
                   )


# Create list of models
model_list <- list(original = gbmtune_model,
                 weighted = gbm_weighted_model,
                 down = down_fit,
                 up = up_fit,
                 SMOTE = smote_fit)

# ROC
model_list_roc <- model_list %>% 
  map(test_roc, data = test)

model_list_roc %>% 
  map(auc)

# Examine results and ploc ROC curve
results_roc <- list(NA)
num <- 1

for(roc in model_list_roc) {
  results_roc[[num]] <-
    data.frame(tpr = roc$sensitivities,
               fpr = 1 - roc$sensitivities,
               model = names(model_list)[num])
  num <- num + 1
}

results_df <- bind_rows(results_roc)
m_col <- c("blue", "red", "green", "grey", "black")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

# Although weighting outperformed sampling techniques in this simulation, it is important to compare different techniques to see which
# works best for your data. There is no huge benefit in using either weighting or sampling techniques when classes are moderately imbalanced.

