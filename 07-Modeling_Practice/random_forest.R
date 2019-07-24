library(tidyverse)
library(randomForest)
library(ROCR)
library(caret)

# https://www.listendata.com/2014/11/random-forest-with-r.html

data = read.csv("https://sites.google.com/site/pocketecoworld/german_credit.csv")

# Check types of variables
str(data)

# Count NAs
sapply(data, function(x) {sum(is.na(x))})

# Check # of rows and columns and structure
str(data)
dim(data)

# Make dependent variable  as a factor (categorical)
data$Creditability <- as.factor(data$Creditability)

set.seed(1000)
mod1 <- randomForest(Creditability ~ .,
                     data = data,
                     ntree = 500)

print(mod1)

# note: if dep variable is a factor, classification is assumed else regression

floor(sqrt(ncol(data) - 1))

# Find the optimal mtry value
mtry <- tuneRF(data[-1],
               data$Creditability, 
               ntreeTry = 500,
               # specifies each iteration, mtry is inflated or deflated by this value
               stepFactor = 1.5, 
               # specifies the relative improvement in OOB error for it to continue
               improve = 0.01,
               # specifies whether to print the progress of search
               trace = TRUE, 
               # specifies whether to plot the OOB error as function of mtry
               plot = TRUE
               )

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

print(mtry)
print(best.m)

# Build model against using best mtry value
set.seed(100)
mod2 <- randomForest(Creditability ~ .,
                     data = data,
                     mtry = best.m,
                     importance = TRUE,
                     ntree = 500
                     )
print(mod2)

# Evaluate variable importance
importance(mod2)
varImpPlot(mod2)
# Higher the value of mean dec accuracy or gini = higher the importance of the variable

# MDA: how much the model acc decreases if we drop that var
# MDG: measure of var importance based on the gini impurity index used for calculation of split in trees

# Prediction and Calculate Performance Metrics
prediction1 <- predict(mod2, type = "prob")
perf <- prediction(prediction1[,2], data$Creditability)

# Area under the curve
auc = performance(perf, "auc")
auc

# Find True positive and negative rate
pred2 <- performance(perf, "tpr", "fpr")

# Plot ROC Curve
plot(pred2, main = "ROC Curve for Random Forest",
     col = 2, lwd = 2
     )

# Replace periods for train model
colnames(data)
colnames(data) = c(
  "Creditability",
  "accountbalance",
  "durationcreditmonth",
  "paymentstatusofpreviouscredit",
  "purpose",
   "CreditAmount" ,                   
   "ValueSavingsStocks"  ,           
   "Lengthofcurrentemployment"   ,  
   "Instalmentpercent"    ,          
   "SexMaritalStatus"  ,           
   "Guarantors"             ,          
   "DurationinCurrentaddress"  ,    
   "Mostvaluableavailableasset",    
   "Ageyears."     ,                 
   "ConcurrentCredits" ,              
   "Typeofapartment"  ,              
   "NoofCreditsatthisBank" ,      
   "Occupation"       ,                
   "Noofdependents",                 
   "Telephone"     ,                   
   "ForeignWorker"   
)

# Select only a few columns and change to df
data2 <- data[,]
names(data2)
data3 <- as.data.frame(data2)
names(data3)

data3$accountbalance <- as.factor(data3$accountbalance)

# Cross validation with manual fine tuning
mtry2 <- sqrt(ncol(data))
control <- trainControl(method = "repeatedcv",
                        # classProbs = TRUE,
                        # summaryFunction = twoClassSummary,
                        search = "random",
                        repeats = 3,
                        number = 3)

set.seed(50)
mod3 <- train(Creditability ~ .,
              data = data3,
              trControl = control,
              tunelength = 10,
              # mtry = mtry2,
              ntree = 500,
              method = "rf",
              # metric = "ROC",
              importance = TRUE
              )
print(mod3)
plot(mod3)
summary(mod3)

# Plot variables importance
# Interpret MDG, MDA (higher = better nodes)
# 
# importance(mod3)
# varImpPlot(mod3)

# Predict new values and plot ROC curve
prediction3 <- predict(mod3, type = "prob")
perf3 <- prediction(prediction3[,2],data3$Creditability)

# AUC
auc3 = performance(perf3, "auc")
auc3

#Find TP/FN
pred3 <- performance(perf3, "tpr", "fpr")
plot(pred3, main = "Model 3", col = 4, lwd = 1)

# Model 4 with randomForest instead of caret
mod4 <- randomForest(Creditability ~ .,
                     data = data3,
                     trControl = control,
                     tunelength = 10,
                     # mtry = mtry2,
                     importance = TRUE,
                     ntree = 500
)

print(mod4)
plot(mod4)
importance(mod4)
varImpPlot(mod4)

prediction4 <- predict(mod4, type = "prob")
perf4 <- prediction(prediction4[,2], data3$Creditability)

auc4 <- performance(perf4, "auc")
auc4

pred4 <- performance(perf4, "tpr", "fpr")
plot(pred4, main = "Model 4", col = 2, lwd = 2)


###########################################################

# https://www.guru99.com/r-random-forest-tutorial.html

# Q1 Try grid and random searches

set.seed(1111)
mod5 <- train(Creditability ~ .,
              data = data3,
              method = "rf",
              trControl = control,
              metric = "Accuracy"
              # tuneGrid = NULL
              )

print(mod5)
plot(mod5)

control_grid <- trainControl(method = "repeatedcv",
                             search = "grid",
                             number = 3,
                             repeats = 3
                             )

set.seed(2222)
mod6 <- train(Creditability ~ .,
              data = data3,
              method = "rf",
              metric = "Accuracy",
              trControl = control_grid
              # tuneGrid = NULL
              )

print(mod6)
plot(mod6)

par(mfrow = c(1,2))
plot(mod5, main = "Model: Random Search")
plot(mod6, main = "Model: Grid Search")

# Q2: Now try best mtry from 1:10

set.seed(1020)
tunegrid <- expand.grid(.mtry = c(1:15))

mod7 <- train(Creditability ~ .,
              data = data3,
              method = "rf",
              metric = "Accuracy",
              tuneGrid = tunegrid,
              trControl = control_grid,
              ntree = 500,
              importance = TRUE
              )

print(mod7)
plot(mod7, main = "Model: tuneGrid 1-15")

# What is the best mtry from mod7?
mod5$bestTune$mtry
mod6$bestTune$mtry
mod7$bestTune$mtry
# Store it and use it when you need to tune other parameters
max(mod5$results$Accuracy)
max(mod6$results$Accuracy)
max(mod7$results$Accuracy)
# Output
best_mtry5 <- mod5$bestTune$mtry 
best_mtry6 <- mod6$bestTune$mtry 
best_mtry7 <- mod7$bestTune$mtry 
best_mtry5
best_mtry6
best_mtry7

ggplot(mod7, aes(y = results$accuracy)) +
  geom_point() +
  labs(x = "Random Predictors",
       y = "Accuracy from Repeated CV",
       title = "Model 7 with .mtry(1:15)",
       subtitle = "Random Forest Practice",
       color = "Max mtry") +
  theme(axis.title =  element_text(face = "bold"),
        axis.text.x = element_text(colour = "red", face = "bold"),
        axis.text.y = element_text(colour = "red", face = "bold"),
        panel.border = element_rect(fill = NA, colour = "grey20"), 
        panel.background = element_rect(colour = "lightblue", fill = "lightblue"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"),
        plot.background = element_rect(colour = "lightgray", fill = "lightgray"),
        plot.title = element_text(hjust = 0.5, size = "12", face = "bold", lineheight= 0.5),
        plot.subtitle = element_text(hjust = 0.5, lineheight = 0.5, face = "italic")
        
        ) +
  coord_flip() +
  geom_line(aes(y = max(mod7$results$Accuracy), colour = "Max Line")) +
  theme(legend.position = c(.15, .9),
        legend.text = element_text(color = "black", face = "bold", size = 8),
        legend.background = element_rect(fill = "lightblue", size = 0.5, colour = "red"),
        legend.title = element_text(size = 10, face = "bold")
        )

theme_custom <- function(base_size = 11, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.title =  element_text(face = "bold"),
           axis.text.x = element_text(colour = "red", face = "bold"),
           axis.text.y = element_text(colour = "red", face = "bold"),
           panel.border = element_rect(fill = NA, colour = "grey20"), 
           panel.background = element_rect(colour = "lightblue", fill = "lightblue"),
           panel.grid.major = element_line(colour = "white"),
           panel.grid.minor = element_line(colour = "white"),
           plot.background = element_rect(colour = "lightgray", fill = "lightgray"),
           plot.title = element_text(hjust = 0.5, size = "12", face = "bold", lineheight= 0.5),
           plot.subtitle = element_text(hjust = 0.5, lineheight = 0.5, face = "italic")
    )

}

ggplot(mod7, aes(y = results$accuracy)) +
  geom_point() +
  labs(x = "Random Predictors",
       y = "Accuracy from Repeated CV",
       title = "Model 7 with .mtry(1:15)",
       subtitle = "Random Forest Practice",
       color = "Max mtry") +
  theme_custom()

# Q3: Search for the best maxnodes
# How? Create a loop to evaluate different values of maxnodes:
# Create a list
# Create a variable with the best value of mtry
# Create the loop
# Store the current value of maxnode
# Summarize the results

store_maxnode <- list() # results of the model will be stored in this list

bestgrid7 <- expand.grid(.mtry = best_mtry7)

for (maxnodes in c(2:7)) {
  set.seed(2002)
  mod8 <- train(Creditability ~ .,
                data = data3,
                method = "rf",
                metric = "Accuracy",
                tuneGrid = bestgrid7,
                trControl = control_grid,
                importance = TRUE,
                # nodesize = 14,
                maxnodes = maxnodes,
                ntree = 500
  )
  current_iteration <- toString(maxnodes) # Store as a string variable the value of maxnode
  store_maxnode[[current_iteration]] <- mod8 # Save the result of the model in the list
}

results_mtry <- resamples(store_maxnode) # Arrange the results of the model
summary(results_mtry) # Print the summary of all the combinations

# Can try with higher values c(20:30) to try to get higher score

# Q4: Search for the best ntrees

store_maxtrees <- list()
for (ntree in c(300, 400, 500, 600, 800, 1000, 2000)) {
  set.seed(5678)
  mod9 <- train(Creditability ~ .,
                data = data3,
                method = "rf",
                metric = "Accuracy",
                tuneGrid = tunegrid,
                trControl = control_grid,
                importance = TRUE,
                nodesize = 14,
                maxnodes = 5,
                ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- mod9
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)


## Found best hyperparameters for final model
# ntree = 800, 800 trees will be trained
# mtry = 4, 4 features will be chosen for each iteration
# maxnodes = 5, max of 5 nodes in the terminal nodes (leaves)

# Q5: Evaludate the model

model_optimal <- train(Creditability ~ .,
                       data = data3,
                       method = "rf",
                       metric = "Accuracy",
                       # mtry = 14,
                       tuneGrid = tunegrid,
                       trControl = control_grid,
                       nodesize = 14, # Change once tested
                       ntree = 300,   # Change once tested
                       maxnodes = 5,  # Change once tested
                       importance = TRUE
                       
)

print(model_optimal)
plot(model_optimal)
prediction_opt <- predict(model_optimal, newdata = test)
prediction_opt2 <- predict(model_optimal) # If no test data

confusionMatrix(prediction_opt, data3$Creditability)
confusionMatrix(prediction_opt2, data3$Creditability)

# Q6: Visualize the Result
# Look at the feature importance
varImpPlot(model_optimal)

# Speed up random forest with R

ggplot(model_optimal, aes(y = results$Accuracy)) +
  labs(x = "Random Predictors",
       y = "Accuracy from Repeated CV",
       title = "Model 7 with .mtry(1:15)",
       subtitle = "Random Forest Practice",
       color = "Max mtry") +
  theme_custom() +
  geom_line(aes(y = max(model_optimal$results$Accuracy), colour = "Max Line")) +
  theme(legend.position = c(.15, .9),
        legend.text = element_text(color = "black", face = "bold", size = 8),
        legend.background = element_rect(fill = "lightblue", size = 0.5, colour = "red"),
        legend.title = element_text(size = 10, face = "bold")
  )




## Solve! Create model with multiple mtry and ntree grids
