library(dplyr)
library(ggplot2)
library(magrittr)
library(car)
library(InformationValue)
library(caret)

# setwd('C:/Users/User/Documents/R/win-library/3.4')

train <- read.csv('Datasets/Titanic/train.csv')
test <- read.csv('Datasets/Titanic/test.csv')

## Engineer new variable; fare > average of embarked group 1 or 0 combined with embarked variable - C1
mean_fare_of_survival_embarked <-train %>% 
  group_by(Survived, Embarked) %>% 
  summarise(mean(Fare)) %>% 
  set_colnames(c("Survived", "Embarked", "meanFare"))

mean_fare_of_survival_embarked[1,]

new_train <-train %>%
  left_join(mean_fare_of_survival_embarked, by = c("Survived" = "Survived", "Embarked" = "Embarked"))

# new_train$`mean(Fare)`
new_train$aboveFare <- ifelse(new_train$meanFare > new_train$Fare, "Y", "N")
new_train[1,]

###########################################################
## Combine datasets then split after data exploration
###########################################################

test$Survived <- NA
test$meanFare <- NA
test$aboveFare <- NA
dim(new_train)
dim(test)
names(new_train)
combined <- rbind(new_train, test)
combined[1,]

###########################################################
## Check data structure and missing values
###########################################################
str(combined)

## Fill missing ages with average
mean_survival_age <- mean(combined$Age, na.rm = TRUE)
missing_age <- is.na(combined$Age)
combined$Age[missing_age] <- mean_survival_age

## Check for missing values
colSums(combined == '')

###########################################################
## Data explore confused variables
###########################################################

## Check survival rate of embarked (doesn't seem important...)
train$Embarked[train$Embarked == ""] <- "S"
table(train$Survived, train$Embarked)
prop.table(table(train$Survived, train$Embarked), 2) ## Use 2 to do column proportion and 1 for row else 0 for all comparison


# Check for status titles
combined$Name <- as.character(combined$Name)
combined$Title <- sapply(combined$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combined$Title <- sub(' ', '', combined$Title)

# Update titles
combined$Title[combined$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer', 'Dr', 'Master')] <- 'Royalty'
combined$Title[combined$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combined$Title[combined$Title %in% c('Miss', 'Mlle', 'Ms')] <- 'Miss'
combined$Title[combined$Title %in% c('Mme', 'Mrs')] <- 'Madame'
combined$Title <- factor(combined$Title)

# Engineered variable: Family size
combined$FamilySize <- combined$SibSp + combined$Parch + 1

# Engineered variable: Family
combined$Surname <- sapply(combined$Name, FUN = function(x) {strsplit(x, split = '[,.]')[[1]][1]})
combined$FamilyID <- paste(as.character(combined$FamilySize), combined$Surname, sep = "")
combined$FamilyID[combined$FamilySize <= 2] <- 'Small'

# Inspect new feature
table(combined$FamilyID)

# Delete erroneous family IDs
famIDs <- data.frame(table(combined$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combined$FamilyID[combined$FamilyID %in% famIDs$Var1] <- 'Small'

# Convert to a factor
combined$FamilyID <- factor(combined$FamilyID)
combined$aboveFare <- factor(combined$aboveFare) ## Causing errors... removed from glm for now

###########################################################
## Build model
###########################################################

train <- combined[1:891,]
test <- combined[892:1309,]

model <- glm(Survived ~ Sex + Age + FamilySize + Title #+ aboveFare ## New levels error for FamilyID
             , data = train, family = binomial(link = "logit"))

summary(model)
vif(model)
anova(model, test = "Chisq")

# Predicted <- ifelse(plogis(predict(model, newdata = test, type = "response")) > 0.5,1,0)
Predicted <- ifelse(plogis(predict(model, newdata = test, type = "response")) > 0.597, 1, 0)
# Predicted <- plogis(predict(model, newdata = test, type = "response"))
summary(Predicted)
Predicted2 <- ifelse(predict(model, newdata = test, type = "response") > 0.3973424,1,0)
# Predicted2 <- predict(model, newdata = test, type = "response")
summary(Predicted2)


# PredictionValue <- ifelse(Predicted)

# Compare train model
# Predicted2 <- plogis(predict(model, newdata = train))
# library(InformationValue)
# optCutOff <- optimalCutoff(train$Survived, Predicted2)[1]
# # plotROC(train$Survived, Predicted2)
# Concordance(Predicted2, train$Survived)
# confusionMatrix(data = Predicted2, reference = train$Survived)
# 
dataset <- data.frame(PassengerId = test$PassengerId, Survived = Predicted)
write.csv(dataset, file = "Submissions/Titanic_glm.csv", row.names = FALSE)
