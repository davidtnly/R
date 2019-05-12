# Introduction

# Goal: Use new ML models: kNN, SVM, xgb
# https://www.kaggle.com/cdeotte/titantic-mega-model-0-84210

# Load Libraries
library(tidyverse)
library(Hmisc)
library(knitr)

# Load data
train <- read.csv('Titanic/train.csv', stringsAsFactors = FALSE)
test <- read.csv('Titanic/test.csv', stringsAsFactors = FALSE)

# Check survival of genders
table(train$Survived[train$Sex == "male" & train$Age < 16])
table(train$Survived[train$Sex == "female" & train$Pclass == 3])

# Data indicates that women and children were prioritized

# Create new feature to identify woman-child groups and survival rate
# Extract titles from Name
# Step by step 
head(train$Name,1)
head(substring(train$Name,regexpr(",", train$Name)),1) # Find comma
head(substring(train$Name,regexpr(",", train$Name)+2),1) # Start at period
head(substring(train$Name,regexpr(",", train$Name)+2, regexpr("\\.", train$Name)),1)
head(substring(train$Name,regexpr(",", train$Name)+2, regexpr("\\.", train$Name)-1),1)
train$Title <- substring(train$Name, regexpr(",", train$Name) + 2, regexpr("\\.", train$Name) - 1)

# New variable for genders
kable(table(train$Title))
train$NewGender <- train$Title
train$NewGender[train$NewGender %in% c("Capt","Don","Major","Col","Rev",
                                        "Dr","Sir","Mr","Jonkheer")] <- "Man"
train$NewGender[train$NewGender %in% c("Dona","the Countess","Mme","Mlle",
                                       "Ms","Miss","Lady","Mrs")] <- "Woman"
train$NewGender[train$NewGender %in% c("Master")] <- "Boy"
kable(table(train$NewGender))

# Table to see survival rate
GenderSurvival <- table(train$NewGender,train$Survived)
colnames(GenderSurvival) <- c("No","Yes")
GenderSurvival

# New Surname variable for group (Man/Single)
train$Surname <- substring(train$Name, 0, regexpr(",", train$Name)-1)
train$Surname[train$NewGender == "Man"] <- "NoGroup"
train$SurnameFreq <- ave(1:891, train$Surname, FUN = length)
train$Surname[train$SurnameFreq <= 1] <- "NoGroup"

# Calculate woman-child group survival rates
train$SurnameSurvival <- ave(train$Survived, train$Surname)
kable(table(train$SurnameSurvival[train$Surname != "NoGroup"]))
kable(table(train$SurnameSurvival))

# Following woman-child groups all perish
x_perish <- train[train$SurnameSurvival == 0, c("Surname")]
unique(x_perish[order(x_perish)])

# Following woman-child groups all survive
x_survive <- train[train$SurnameSurvival == 1, c("Surname")]
unique(x_survive[order(x_survive)]) # Find unique names and order by A-Z

# Explore new variables
train[train$SurnameSurvival == 1/7,c("Surname","NewGender","Survived")]
train[train$SurnameSurvival == 1/3,c("Surname","NewGender","Survived")]
train[train$SurnameSurvival == 3/4,c("Surname","NewGender","Survived")]

# Cross-validation
# Adjust survival rates for use on training set
train$AdjustedSurvival <- (train$SurnameSurvival * train$SurnameFreq - train$Survived)/(train$SurnameFreq-1)

# Apply gender model plus new predictor to training set
train$predict <- 0
train$predict[train$NewGender == "Woman"] <- 1
train$predict[train$NewGender == "Boy"] <- 1
train$predict[train$NewGender == "Woman" & train$AdjustedSurvival == 0] <- 0

# Plot how new predictor changes gender model
ggplot(data = train[train$NewGender == "Woman",]) +
  geom_jitter(aes(x = Pclass, y = predict, color = factor(Survived))) +
  labs(ttle = "Female Predictions on Training Set",
       x = "Pclass",
       y = "New Predictor"
  ) +
  geom_rect(alpha = 0, color = "black", 
            aes(xmin = 2.5, xmax = 3.5, ymin = -0.45, ymax = 0.45))

kable(table(train$Survived[train$NewGender == "Woman" & train$predict == 0]))

# plot how new predictor changes gender model
ggplot(train[train$NewGender!='woman',]) +
  geom_jitter(aes(x = NewGender, y = predict, color = factor(Survived))) +
  labs(title = "16 male predictions change from gender model on training set",
       x="Title",y = "New Predictor") +
  geom_rect(alpha = 0, color = "black",
            aes(xmin = 0.5, xmax = 1.5, ymin = 0.55, ymax = 1.45))

kable(table(train$Survived[train$NewGender!='Woman' & train$predict == 1]))

# Perform cross-validation
trials = 30; sum = 0
for (j in 1:trials){
  x = sample(1:890); s = 0
  for (i in 0:9){
    # Engineer "woman-child-groups" from training subset
    train$Surname <- substring(train$Name,0,regexpr(",",train$Name)-1)
    train$Surname[train$NewGender == 'Man'] <- 'No Group'
    train$SurnameFreq <- ave(1:891,train$Surname,FUN=length)
    train$Surname[train$SurnameFreq <= 1] <- 'No Group'
    train$SurnameSurvival <- NA
    # calculate training subset's surname survival rate
    train$SurnameSurvival[-x[1:89+i*89]] <- ave(train$Survived[-x[1:89+i*89]],train$Surname[-x[1:89+i*89]])
    # calculate testing subset's surname survival rate from training set's rate
    for (k in x[1:89+i*89]) 
      train$SurnameSurvival[k] <- train$SurnameSurvival[which(!is.na(train$SurnameSurvival) & train$Surname==train$Surname[k])[1]]
    # apply gender model plus new predictor
    train$predict <- 0
    train$predict[train$NewGender=='Woman'] <- 1
    train$predict[train$NewGender=='Boy' & train$SurnameSurvival==1] <- 1
    train$predict[train$NewGender=='Woman' & train$SurnameSurvival==0] <- 0
    c = sum(abs(train$predict[x[1:89+i*89]] - train$Survived[x[1:89+i*89]]))
    s = s + c
  }
  cat( sprintf("Trial %d has 10-fold CV accuracy = %f\n",j,1-s/890))
  sum = sum + 1-s/890
}
cat(sprintf("Average 10-fold CV accuracy from %d trials = %f\n",trials,sum/trials))

