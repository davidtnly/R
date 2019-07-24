library(tidyverse)
library(caret)
library(gridExtra)
library(corrplot)
library(Hmisc)
library(ROCR)
library(knitr)
library(gridExtra)
library(randomForest) 

# Practice models

## https://www.kaggle.com/erikbruin/titanic-2nd-degree-families-and-majority-voting/notebook


# setwd('C:/Users/User/Documents/R/win-library/3.4')

train <- read.csv('Datasets/Titanic/train.csv', stringsAsFactors = FALSE, na.strings = c("NA", ""))
test <- read.csv('Datasets/Titanic/test.csv', stringsAsFactors = FALSE, na.strings = c("NA", ""))

## Merge for data cleaning and feature engineering
test$Survived <- NA
all <- rbind(train, test)

# Check for NAs
sapply(all, function(x) {sum(is.na(x))})
colSums(sapply(all, is.na))

# Check data structure
str(all)
summary(all)
glimpse(all)

# Update columns to new data types (Erik Bruin's Features - Kaggle)
all$PassengerId <- as.factor(all$PassengerId)
distinct(all, Pclass) # Check for distinct classes
all$Pclass <- factor(all$Pclass, levels = c("1", "2", "3"), ordered = TRUE)
  # all$Pclass <- as.ordered(all$Pclass)

## P2/P3 high death rate for males
all$PclassSex[all$Pclass == '1' & all$Sex == 'male'] <- 'P1Male'
all$PclassSex[all$Pclass == '2' & all$Sex == 'male'] <- 'P2Male'
all$PclassSex[all$Pclass == '3' & all$Sex == 'male'] <- 'P3Male'
all$PclassSex[all$Pclass == '1' & all$Sex =='female'] <- 'P1Female'
all$PclassSex[all$Pclass == '2' & all$Sex =='female'] <- 'P2Female'
all$PclassSex[all$Pclass == '3' & all$Sex =='female'] <- 'P3Female'
all$PclassSex <- as.factor(all$PclassSex)
all$Fare <- as.integer(all$Fare)

all$Surname <- sapply(all$Name, function(x) {strsplit(x, split = '[,.]')[[1]][1]})

## Correcting some surnames that include a maiden name
all$Surname <- sapply(all$Surname, function (x) {strsplit(x, split = '[-]')[[1]][1]})
all$Title <- sapply(all$Name, function(x) {strsplit(x, split = '[,.]')[[1]][2]})
all$Title <- sub(' ', '', all$Title) # Removing spaces before title
kable(table(all$Sex, all$Title))

# Combine titles
head(all$Name,1)
all$Surname_test <- sapply(all$Name, function(x) {strsplit(x, split = "[,.]")})
head(all$Name,1)
# Test split from above
all$Surname_test <- (sapply(all$Surname_test, function(x) {strsplit(x, split = '[-]')[1][1]}))
all$Title_test <- sapply(all$Name, function(x) {strsplit(x, split = "[,.]")[[1]][[2]]})
head(all$Title_test, 1)
# Replace extra spaces in front and behind name
all$Title_test <- sub(" ", '', all$Title_test)
head(all$Title_test, 1)

all$Title[all$Title %in% c("Mlle", "Ms")] <- "Miss"
all$Title[all$Title == "Mme"] <- "Mrs"
all$Title[!(all$Title %in% c('Master', 'Miss', 'Mr', 'Mrs'))] <- "Rare Title"
all$Title <- as.factor(all$Title)
kable(table(all$Sex, all$Title))

glimpse(all)

ggplot(all, aes(x = Pclass, fill = Pclass)) +
  geom_bar(stat = 'count', position = 'dodge') +
  labs(x = 'Pclass, All data') + 
  geom_label(stat = "count",aes(label = ..prop..))

ggplot(all[!is.na(all$Survived),], aes(x = Title, fill = Survived)) +
  geom_bar(stat = "count", position = "stack") +
  theme_grey()


ggplot(all[!is.na(all$Survived),], aes(x = Title, fill = Survived)) +
  geom_bar(stat='count', position='stack') +
  labs(x = 'Title') +theme_grey()

# Engineered variable: Family size
all$FamilySize <- all$SibSp + all$Parch + 1

# Engineered variable: Family
all$Surname <- sapply(all$Name, FUN = function(x) {strsplit(x, split = '[,.]')[[1]][1]})
all$FamilyID <- paste(as.character(all$FamilySize), all$Surname, sep = "")
all$FamilyID[all$FamilySize <= 2] <- 'Small'

# Inspect new feature
table(all$FamilyID)

# Delete erroneous family IDs
famIDs <- data.frame(table(all$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
all$FamilyID[all$FamilyID %in% famIDs$Var1] <- 'Small'

# Convert to a factor
all$FamilyID <- factor(all$FamilyID)
# all$aboveFare <- factor(all$aboveFare)

train <- all[1:891,]
test <- all[892:1309,]
train <- as.data.frame(train)
test <- as.data.frame(test)

model <- glm(Survived ~ Sex + Age + FamilySize + Title #+ aboveFare ## New levels error for FamilyID
             , data = train, family = binomial(link = "logit"))

summary(model)
vif(model)
anova(model, test = "Chisq")

set.seed(110)
model2 <- train(Survived ~ Sex + Age + FamilySize + Title,
                data = train,
                method = "rf",
                importance = TRUE)

print(model2)
plot(model2)

control <- trainControl(method = "repeatedcv"
                        , repeats = 3
                        , number = 7
                        , search = "random")

mtry <- sqrt(ncol(train))
mtry

set.seed(109)
model3 <- train(Survived ~ Sex + Age + FamilySize + Title,
                data = train,
                method = "rf",
                trControl = control,
                importance = TRUE
                )


print(model3)
plot(model3)

control_log <- trainControl(method = "repeatedcv",
                            repeats = 3,
                            number = 5
                            # summaryFunction = twoClassSummary,
                            # classProbs = TRUE
                            
                            )
# Error: At least one of the class levels is not a valid R variable name; 
# This will cause errors when class probabilities are generated 
# because the variables names will be converted to  X0, X1

set.seed(200)
model5 <- train((Survived) ~ Sex + Age + FamilySize + Title,
                data = train,
                method = "rf",
                # metric = "Accuracy",
                trControl = control,
                tuneLength = 5,
                importance = TRUE
                )


plot(model5)
summary(model5)
print(model5)
# varImpPlot(model4)


prediction <- predict(model5, newdata = test, type = "raw")
prediction <- ifelse(prediction >= 0.5, 1 ,0)

prediction3 <- predict(model3, newdata = test, type = "raw")
prediction3 <- ifelse(prediction3 >= 0.5, 1 ,0)

prediction2 <- predict(model2, newdata = test, type = "raw")
prediction2 <- ifelse(prediction2 >= 0.5, 1 ,0)

prediction_test <- predict(model5, type = "raw")
prediction_test <- ifelse(prediction_test >= 0.5, 1 ,0)
prediction_test <- as.data.frame(prediction_test)
prediction3 <- as.data.frame(prediction3)
prediction2 <- as.data.frame(prediction2)

# confusionMatrix(prediction_test, train$Survived)

combined_test <- cbind(train, prediction_test)
head(combined_test)

perf_test <- prediction(prediction, train$Survived)
pred_test <- performance(perf_test, "tpr", "fpr")

# 
# dataset <- data.frame(PassengerId = test$PassengerId, Survived = Predicted)
# write.csv(dataset, file = "Submissions/Titanic_glm.csv", row.names = FALSE)

