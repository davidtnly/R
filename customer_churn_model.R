# Set working directory
setwd("~/R Projects")

# Load libraries
source("Public_R_Projects/libraries.R") # frequently used libraries
source("Public_R_Projects/functions.R") # frequently used functions: check_na, details, convert_date

# Set directories
datadir <- "~/R Projects/Public_R_Projects_Data/"
maindir <- "~/R Projects/Public_R_Projects/"
filename <- "telco_churn_data.csv"

# Load data
data <- read.csv(paste0(datadir,filename))

# Structure
details(data)
check_na(data)

# Check NA data
data[is.na(data$TotalCharges),]
newdata <- data[complete.cases(data),]
nrow(newdata)

# Preprocessing/Cleaning
cols_to_change <- c(10:15)

for(i in 1:ncol(newdata[, cols_to_change])) {
  
  newdata[, cols_to_change][, i] <- as.factor(mapvalues(newdata[, cols_to_change][, i], from = c("No internet service"), to = c("No")))
  
}

newdata$MultipleLines <- as.factor(mapvalues(newdata$MultipleLines, from = c("No phone service"), to = c ("No")))

# Check loop data
prop.table(table(newdata[,10]))
prop.table(table(newdata$MultipleLines))

# Factorize tenure
tenure_fac <- function(x) {
  if (x >= 0 & x <= 12) { return("0-12 Mo")} 
  else if (x > 12 & x <= 24) { return("12-24 Mo") } 
  else if (x > 24 & x <= 36) { return("24-36 Mo") }
  else if (x > 36 & x <= 48) { return("36-48 Mo") }
  else if (x > 48 & x <= 60) { return("48-60 Mo") } 
  else return("> 60 Mo")
}
newdata$tenure_group <- as.factor(sapply(newdata$tenure, tenure_fac))
(table(newdata$tenure_group))

# Separate numerical and categorical variables
num_col <- names(which(sapply(newdata, is.numeric)))
cat_col <- names(which(sapply(newdata, is.character)))

all_num <- newdata[, num_col]
all_cat <- newdata[, cat_col]

# Check for near zero variance
nzv <- nearZeroVar(all_num, foreach = TRUE, saveMetrics =  TRUE)
nzv
corrs <- cor(all_num, use = "complete.obs")
cor_color <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corrs, method = "color", insig = "blank", sig.level = 0.05, col = cor_color(200))
ggcorr(corrs,
       method = c("pairwise", "spearman"),
       nbreaks = 6,
       hjust = 0.8,
       label = TRUE,
       label_size = 5,
       color = "grey50")

# Plots (Just a few)
ggplot(newdata, aes(x = gender)) +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.75, fill = "#2C3E50") +
  theme_tq() +
  labs(x = "Gender",
       y = "Pct",
       title = "Gender Ratio") +
  coord_flip()

ggplot(newdata, aes(x = SeniorCitizen)) +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.75, fill = "#2C3E50") +
  theme_tq() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "SC",
       y = "Pct",
       title = "How Many Senior Citizens Are There?",
       caption = "Roughly 17% of the data are Sr. Citizens") +
  coord_flip()

# Select variables
rmv <- c("customerID", "tenure")
model_data <- newdata[, !colnames(newdata) %in% rmv]
dim(model_data)

# Modeling - Split data
inTrain <- createDataPartition(model_data$Churn, p = 0.8, list = FALSE)
train <- model_data[inTrain,]
test <- model_data[-inTrain,]
dim(train); dim(test)

prop.table(table(train$Churn))
prop.table(table(test$Churn))

# Logistic Regression
log_model <- glm(Churn ~ .,
                 data = train,
                 family = binomial(link = "logit"))
summary(log_model)

# ANOVA - Features
anova(log_model, test = "Chisq")

# Predict
log_predict <- predict(log_model, newdata = test, type = "response")
confusionMatrix(as.factor(ifelse(predict(log_model, newdata = test, type = "response") > 0.5, "Yes", "No")), test$Churn)

# Calculate best cutoff
best_thres <- function(predict,label) {
  k = 0
  accuracy = c()
  sensitivity = c()
  specificity = c()
  
  for(i in seq(from = 0.01 , to = 0.8 , by = 0.01)){
    k = k + 1
    preds_binomial = ifelse(predict > i , 1 , 0) # pred_loop
    confmat = table(label , preds_binomial)
    accuracy[k] = sum(diag(confmat)) / sum(confmat)
    sensitivity[k] = confmat[1 , 1] / sum(confmat[ , 1])
    specificity[k] = confmat[2 , 2] / sum(confmat[ , 2])
  }
  threshold <- seq(from = 0.01, to = 0.8, by = 0.01)
  threshold_df <- data.frame(threshold, accuracy, sensitivity, specificity)
  head(threshold_df)
  return(threshold_df)
}

best_cutoff <- function (df) {
  bst_cut <- max(df$accuracy)
  b1 <- df[df$accuracy == bst_cut,]
  t1 <- min(b1$threshold)
  t2 <- min(round(b1$accuracy,3))
  t3 <- min(round(b1$sensitivity,3))
  t4 <- min(round(b1$specificity,3))
  lst <- c(t1,t2,t3,t4)
  return(lst)
}

log_thres <- best_thres(log_predict, test$Churn)
log_bc <- best_cutoff(log_thres)

# Plot Threshold
ggplot(gather(log_thres, key = "Metric", value = "Value", 2:4),
             aes(x = threshold, y = Value, color = Metric)) +
  geom_line(size = 1.5) +
  labs(caption = paste0("Best Threshold (Cutoff): ",log_bc[1],"\nAccuracy: ",log_bc[2],"\nSensitivity (TP): ",log_bc[3],"\nSpecitivity (TN): ",log_bc[4]),
       title = "Logistic Prediction Threshold Cutoff Graph"
  ) +
  theme_economist() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank()) 

# Best Accuracy: 79.07% w/ 87.50% Sensitivity
confusionMatrix(as.factor(ifelse(predict(log_model, newdata = test, type = "response") > log_bc[1], "Yes", "No")), test$Churn)

# LogOdds - Odds Ratio
exp(cbind(OR = coef(log_model), confint(log_model)))

# Random Forest
rf_model <- train(Churn ~ .,
                  data = train,
                  method = "rf",
                  mtry = expand.grid(.mtry = seq(1:5)))

save(rf_model, file = "rf_model.rda")
rf_model

# Predict
rf_predict <- predict(rf_model, newdata = test, type = "prob")
confusionMatrix(as.factor(ifelse(predict(rf_model, newdata = test, type = "prob")$Yes > 0.5, "Yes", "No")), test$Churn)

rf_thres <- best_thres(rf_predict$Yes, test$Churn)
rf_bc <- best_cutoff(rf_thres)

# Plot Threshold
ggplot(gather(rf_thres, key = "Metric", value = "Value", 2:4),
       aes(x = threshold, y = Value, color = Metric)) +
  geom_line(size = 1.5) +
  labs(caption = paste0("Best Threshold (Cutoff): ",rf_bc[1],"\nAccuracy: ",rf_bc[2],"\nSensitivity (TP): ",rf_bc[3],"\nSpecitivity (TN): ",rf_bc[4]),
       title = "Random Forest Prediction Threshold Cutoff Graph"
  ) +
  theme_economist() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank()) 

# Best Accuracy: 78.58% w/ 85.95% Sensitivity
confusionMatrix(as.factor(ifelse(predict(rf_model, newdata = test, type = "prob")$Yes > rf_bc[1], "Yes", "No")), test$Churn)

var_df <- varImp(rf_model)$importance %>% 
  mutate(names = row.names(.)) %>%
  arrange(-Overall) %>% 
  data.frame()

# Plot Var Importance
ggplot(var_df, aes(reorder(names, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "#2C3E50") +
  geom_text(aes(label = round(Overall, 1)), vjust = 0.25, hjust = 2, size = 4) +
  theme_tq() +
  labs(x = "Overall Improtance",
       y = "",
       title = "Random Forest Variable Importance Rank") +
  coord_flip()

# # Resamples (Error: There are different numbers of resamples in each model)
# resamps <- resamples(list(Logistic = log_model,
#                           RandomForest = rf_model$resample[2,]))
# resamps
# summary(resamps)
# 
# # Boxplots
# theme1 <- trellis.par.get()
# theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
# theme1$plot.symbol$pch = 16
# theme1$plot.line$col = rgb(1, 0, 0, .7)
# theme1$plot.line$lwd <- 2
# trellis.par.set(theme1)
# bwplot(resamps, layout = c(3, 1))
# 
# # Dotplot
# trellis.par.set(caretTheme())
# dotplot(resamps, metric = "ROC")
# 
# # scatterplot matrix
# splom(resamps)
# 
# # Resample diffs
# model_diff <- diff(resamps)
# summary(model_diff)

# # Feature Engineering would best produce a better model

