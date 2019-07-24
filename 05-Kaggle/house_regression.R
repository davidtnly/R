library(tidyverse)
library(caret)
library(gridExtra)
library(corrplot)
library(Hmisc)
library(knitr)
library(gridExtra)
library(randomForest) 
library(ModelMetrics)
library(e1071) 

## https://www.kaggle.com/c/house-prices-advanced-regression-techniques

# setwd('../Documents/R/win-library/3.4')

# Load Data
# all <- read.csv('C:/Users/David Ly/Documents/R Projects/Kaggle/House Prices/all.csv', stringsAsFactors = FALSE, na.strings = c("NA",""))
# test <- read.csv('C:/Users/David Ly/Documents/R Projects/Kaggle/House Prices/test.csv', stringsAsFactors = FALSE, na.strings = c("NA",""))
train <- read.csv('C:/Users/David Ly/Documents/R Projects/Kaggle/House Prices/train.csv', stringsAsFactors = FALSE)
test <- read.csv('C:/Users/David Ly/Documents/R Projects/Kaggle/House Prices/test.csv', stringsAsFactors = FALSE)

### Step 1: DF Exploratory Analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check for NAs
sapply(train, function(x) {sum(is.na(x))})
colSums(sapply(train, is.na))

# Remove columns large number of missing data
head(train) #<- train[,-c(7)] #large number of missing data
head(test) #<- test[,-c(7)] #large number of missing data

# Combine test and all datasets
test$SalePrice <- NA
all <- rbind(train, test)
sort(colSums(sapply(all,is.na)), decreasing = TRUE)

# Check data structure
glimpse(all)
str(all)

# Create table loop for all variables, head() for any rows above 20
names <- colnames(all)
for(x in 1:ncol(all)) {
  if (nrow((table(all[,x]))) > 20) {
    cat('\n', colnames(all[x]),'\n',print(head(all[,x])), '\n')
  } else {
    cat('\n', colnames(all[x]), '\n', print(table(all[,x])), '\n')
  }
}

# Facet graph of Neighborhoods
ggplot(all, aes(GrLivArea, SalePrice)) +
  geom_point(aes(color = Neighborhood)) +
  scale_x_continuous("GrLivArea") +
  scale_y_continuous("SalePrice") +
  theme_bw() + facet_wrap( ~ Neighborhood) +
  theme(legend.position = "none")

# Split Numeric and Categoric variables (First split - split again after)
num_features_init <- names(which(sapply(all, is.numeric)))
cat_features_init <- names(which(sapply(all, is.character)))
all_numeric_init <- all[,names(all) %in% num_features_init]
all_categoric_init <- all[,names(all) %in% cat_features_init]

# Remove NA values for findCorrelation()
all_numeric_init <- all_numeric_init[,!colnames(all_numeric_init) == "SalePrice"]

# Create correlation plots variations
par(mfrow = c(1,1))
correlations <- cor(all_numeric_init,use = "everything")
corrplot(correlations, method = "circle",  sig.level = 0.01, insig = "blank") # type = "lower"
corrplot(correlations, method = "color", type = "lower", 
         tl.cex = 0.75, tl.col = "black", tl.srt = 45)

## Extract highly, r > 0.5, correlated variables
highlyCor <- findCorrelation(correlations, cutoff = 0.8)
train_corr <- train[,-highlyCor]
ncol(train_corr)
correlations_corr <- cor(all_numeric_init, use = "everything")
# par(mfrow = c(1,2))
corrplot(correlations_corr, method = "color", type = "lower", 
         tl.cex = 0.75, tl.col = "black", tl.srt = 45)
# corrplot(correlations, method = "color", type = "lower", 
#          tl.cex = 0.75, tl.col = "black", tl.srt = 45)
par(mfrow = c(1,1))

# # Create pvalue function
# cor.test.mat <- function(mat){
#   n <- ncol(mat)
#   pmat <- matrix(0, nrow = n, ncol = n)
#   for(i in 1:(n-1)){
#     for(j in (i+1):n){
#       pmat[i,j] <- cor.test(mat[,i], mat[,j], method="pearson")$p.value
#     }
#   }
#   pmat[lower.tri(pmat)] <- t(pmat)[lower.tri(pmat)] #fill lower triangle with upper triangle
#   return(pmat)
# }  
# 
# # Compute matrix of p-values
# pvals <- cor.test.mat(bh)
# 
# corrplot(cor(bh), method="number", order="hclust", addrect=2, diag=F)
# corrplot(cor(bh), p.mat = pvals, sig.level=0, insig = "p-value", method="ellipse", order="hclust", 
#          type="upper", addrect=2, tl.pos = "n", cl.pos="n", diag=F, add=T)

#compute matrix of p-values
pvals <- cor.test.mat(bh)

corrplot(cor(bh), method="number", order="hclust", addrect=2, diag=F)
corrplot(cor(bh), p.mat = pvals, sig.level=0, insig = "p-value", method="ellipse", order="hclust", 
         type="upper", addrect=2, tl.pos = "n", cl.pos="n", diag=F, add=T)



# Create custom theme
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

# change scale values
options(scipen = 10000)

# Create plots to visualize data
pairs( ~ YearBuilt + YearRemodAdd + OverallQual + TotalBsmtSF + GrLivArea, data = all,
       main = "Simple Scatterplot Matrix")

pairs( ~ YearBuilt + OverallQual + TotalBsmtSF + GrLivArea, data = all,
       main = "Simple Scatterplot Matrix")

# Check out max sale price attributes
all[which.max(all$SalePrice),]

ggplot(data = all[all$SalePrice > 0,], aes(x = YearBuilt, y = SalePrice)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  labs(x = "Year Built",
       y = "Sales Price",
       title = "Year Built vs. Sales Price"
  )

ggplot(data = all[all$YrSold > 1900 & all$SalePrice > 0,], aes(x = YrSold, y = SalePrice, group = YrSold)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Year Sold",
       y = "Sales Price",
       title = "Year Sold vs. Sales Price"
  )

ggplot(data = all[all$SalePrice > 0,], aes(x = MoSold, y = SalePrice, group = MoSold)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Month Sold",
       y = "Sales Price",
       title = "Month Sold vs. Sales Price"
  ) + scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))

ggplot(data = all[all$SalePrice > 0,], aes(x = MoSold)) +
  geom_histogram() +
  stat_bin(bins = 12, binwidth = 1) +
  geom_density(aes(y = ..density..)) + # get density plot to overlay
  theme_minimal() +
  labs(x = "Month Sold",
       y = "Sales Price",
       title = "Month Sold vs. Sales Price"
  ) + theme_custom() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))

ggplot(data = all[all$X1stFlrSF < 2500 & all$SalePrice > 0,], aes(x = X1stFlrSF, y = SalePrice)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  labs(x = "Square Footage Floor 1 ",
       y = "Sales Price",
       title = "Square Footage Floor 1 vs. Sales Price"
  )

ggplot(data = all[all$SalePrice > 0,], aes(x = FireplaceQu, y = SalePrice)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "FirePlace",
       y = "Sales Price",
       title = "FirePlace vs. Sales Price"
  )

ggplot(data = all[!is.na(all$MSZoning) & all$SalePrice > 0,], aes(x = MSZoning, y = SalePrice)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "MSZoning",
       y = "Sales Price",
       title = "MSZoning vs. Sales Price"
  )

ggplot(data = all[all$LotArea <= 30000 & all$SalePrice > 0,], aes(x = LotArea, y = SalePrice)) +
  geom_point() +
  geom_smooth(se = TRUE) +
  theme_minimal() +
  labs(x = "LotArea",
       y = "Sales Price",
       title = "LotArea vs. Sales Price"
  )

ggplot(data = all[all$SalePrice > 0,], aes(x = HouseStyle, y = SalePrice)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "House Style",
       y = "Sales Price",
       title = "House Style vs. Sales Price"
  )

ggplot(data = all[all$SalePrice > 0,], aes(x = LotConfig, y = SalePrice)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Lot Config",
       y = "Sales Price",
       title = "Lot Config vs. Sales Price"
  )

all <- all %>% 
  mutate(YrRemodel_Diff = YearRemodAdd - YearBuilt) 

ggplot(data = all[all$SalePrice > 0,], aes(x = YrRemodel_Diff, y = SalePrice, group = YrRemodel_Diff)) +
  geom_boxplot() +
  geom_smooth() +
  theme_minimal() +
  labs(x = "Year Diff",
       y = "Sales Price",
       title = "Year Remodel Difference vs. Sales Price"
  ) +
  scale_y_continuous(breaks = c(100000,200000,300000,400000,500000,600000,700000))

ggplot(data = all[all$SalePrice > 0,], aes(x = GarageType, y = SalePrice)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Garage Type",
       y = "Sales Price",
       title = "Garage Type vs. Sales Price"
  )

ggplot(data = all[all$SalePrice > 0,], aes(x = Fence, y = SalePrice)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Fence",
       y = "Sales Price",
       title = "Fence vs. Sales Price"
  )

ggplot(data = all[all$SalePrice > 0,], aes(x = ScreenPorch, y = SalePrice)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "ScreenPorch",
       y = "Sales Price",
       title = "ScreenPorch vs. Sales Price"
  )

ggplot(data = all[all$SalePrice > 0,], aes(x = LowQualFinSF, y = SalePrice)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "LowQualFinSF",
       y = "Sales Price",
       title = "LowQualFinSF vs. Sales Price"
  )

## Find summary differences in prices for remodeled houses vs non-remodeled
train <- train %>% 
  mutate(YrRemodel_Diff = YearRemodAdd - YearBuilt) 
train %>%
  summarise(AvgSalePrice_Remodel = mean(train[which(train$YrRemodel_Diff > 0),]$SalePrice, na.rm = FALSE),
            AvgSalePrice_NoRemodel = mean(train[which(train$YrRemodel_Diff == 0),]$SalePrice, na.rm = FALSE),
            MaxSalePrice_Remodel = max(train[which(train$YrRemodel_Diff > 0),]$SalePrice, na.rm = FALSE),
            MaxSalePrice_NoRemodel = max(train[which(train$YrRemodel_Diff == 0),]$SalePrice, na.rm = FALSE),
            MinSalePrice_Remodel = min(train[which(train$YrRemodel_Diff > 0),]$SalePrice, na.rm = FALSE),
            MinSalePrice_NoRemodel = min(train[which(train$YrRemodel_Diff == 0),]$SalePrice, na.rm = FALSE)
  )

# Recheck NA counts
sort(colSums(sapply(all, is.na)), decreasing = TRUE)

ggplot(all, aes(LotFrontage, SalePrice)) +
  geom_point(aes(color = Neighborhood)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous("LotFrontage") +
  scale_y_continuous("SalePrice") +
  theme_bw() + facet_wrap( ~ SaleType) +
  theme(legend.position = "none")

### Step 2: Fix NA Values (Explore the variables more) ~~~~~~~~~~~~~~~~~~~~~~~~
# Factors contains NA values which means property does not have
# Categorical values
all$MiscFeature[is.na(all$MiscFeature)] <- "None"
all$Fence[is.na(all$Fence)] <- "None"
all$PoolQC[is.na(all$PoolQC)] <- "None"
all$FireplaceQu[is.na(all$FireplaceQu)] <- "None"
all$Alley[is.na(all$Alley)] <- "None"
all$SaleType[is.na(all$SaleType)] <- "Oth"
# Garage Variables
all$GarageYrBlt[is.na(all$GarageYrBlt)] <- 0
all$GarageFinish[is.na(all$GarageFinish)] <- "None"
all$GarageQual[is.na(all$GarageQual)] <- "None"
all$GarageType[is.na(all$GarageType)] <- "None"
all$GarageCond[is.na(all$GarageCond)] <- "None"
# Other Variables
all$SaleType[is.na(all$SaleType)] <- "Oth"
all$MSZoning[is.na(all$MSZoning)] <- "OTH"
all$Exterior1st[is.na(all$Exterior1st)] <- "Other"
all$Exterior2nd[is.na(all$Exterior2nd)] <- "Other"
all$Functional[is.na(all$Functional)] <- "Oth"
all$Utilities[is.na(all$Utilities)] <- "AllPub"
all$Electrical[is.na(all$Electrical)] <- "SBrkr"
all$KitchenQual[is.na(all$KitchenQual)] <- "TA"
# Bsmt Variables
all$BsmtCond[is.na(all$BsmtCond)] <- "None"
all$BsmtExposure[is.na(all$BsmtExposure)] <- "None"
all$BsmtQual[is.na(all$BsmtQual)] <- "None"
all$BsmtFinType2[is.na(all$BsmtFinType2)] <- "None"
all$BsmtFinType1[is.na(all$BsmtFinType1)] <- "None"
# Other
all$MasVnrType[is.na(all$MasVnrType)] <- "None"
all$MSZoning[is.na(all$MSZoning)] <- "RL"

# Continuous variables
all$LotFrontage[is.na(all$LotFrontage)] <- 0
all$MasVnrArea[is.na(all$MasVnrArea)] <- 0
all$BsmtFullBath[is.na(all$BsmtFullBath)] <- 0
all$BsmtHalfBath[is.na(all$BsmtHalfBath)] <- 0
all$BsmtFinSF1[is.na(all$BsmtFinSF1)] <- 0
all$BsmtFinSF2[is.na(all$BsmtFinSF2)] <- 0
all$BsmtUnfSF[is.na(all$BsmtUnfSF)] <- 0
all$TotalBsmtSF[is.na(all$TotalBsmtSF)] <- 0
all$GarageCars[is.na(all$GarageCars)] <- 0
all$GarageArea[is.na(all$GarageArea)] <- 0

# Create a mode function for next analysis instead of hardcoding unknown values

### Step 3: Feature Engineering ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all <- all %>% 
  mutate(YrRemodel_Diff = YearRemodAdd - YearBuilt) 
all$HouseAge <- (2018 - all$YearBuilt)
all$NewHouse <- (all$YearBuilt == all$YrSold) * 1
all$TotalFloorSF <- all$X1stFlrSF + all$X2ndFlrSF
all$TotalArea <- all$LotFrontage+ all$LotArea + all$MasVnrArea + all$TotalBsmtSF +
all$TotalFloorSF + all$GrLivArea + all$GarageArea + all$WoodDeckSF +
all$OpenPorchSF + all$EnclosedPorch + all$X3SsnPorch + all$ScreenPorch
all$OverallQualCond <- all$OverallCond + all$OverallQual

# Overall quality = Quality * numeric features
all$Mod_year_qual <- all$YearBuilt*all$OverallQual      # overall condition
all$Mod_year_r_qual <- all$OverallQual*all$YearRemodAdd # quality x remodel
all$Mod_bsmt_qual <- all$OverallQual*all$TotalBsmtSF    # quality x basement size
all$Mod_livarea_qual <- all$OverallQual*all$GrLivArea   # quality x living area
all$Mod_qual_bath <- all$OverallQual*all$FullBath       # quality x baths

# Split Numeric and Categoric variables
num_features <- names(which(sapply(all, is.numeric)))
cat_features <- names(which(sapply(all, is.character)))

all_numeric <- all[num_features]
all_categoric <- all[cat_features]

# Fix NAs for Feature Engineered Variables
all$TotalArea[is.na(all$TotalArea)] <- median(all$TotalArea, na.rm = TRUE)

# ## Recheck variables
# GdPrv
# ScreenPorch
# EnclosedPorch
# OpenPorchSF
# next 3 column after garagearea 

kable(table(all$Fence))
kable(table(all$ScreenPorch))

# Check for outliers
# Graph new variables
ggplot(data = all[all$SalePrice > 0 & all$TotalArea < 50000,], aes(x = TotalArea, y = SalePrice)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "TotalArea",
       y = "Sales Price",
       title = "TotalArea vs. Sales Price"
  ) # Outliers after 50K Total Area

all[which(all$TotalArea > 50000 & all$Id < 1461),]


# Remove columns that do not look correlated
# Create correlation plots
all <- all[,!colnames(all) == "MiscVal"] 
all <- all[,!colnames(all) == "PoolArea"] 
all <- all[,!colnames(all) == "PoolQC"] 
all <- all[,!colnames(all) == "X3SsnPorch"] 
all <- all[,!colnames(all) == "MiscVal"] 
all <- all[,!colnames(all) == "BsmtFinSF2"] 
all <- all[,!colnames(all) == "MoSold"] 
all <- all[,!colnames(all) == "ScreenPorch"]
all <- all[,!colnames(all) == "LowQualFinSF"]
all <- all[,!colnames(all) == "EnclosedPorch"]


### Step 4: Modeling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Initial partition (Not for this model)
# inTrain <- createDataPartition(all$SalePrice, p = 0.75, list = FALSE)
# train <- all[inTrain,]
# test <- all[-inTrain,]

# Change characters to factors
all <- all %>%
  mutate_if(is.character, as.factor)

# Initial partition to train main model
train <- all[1:1460,]
test <- all[1461:nrow(all),]
train_rf <- all[1:200,]

# Create testing partitions
inTrain <- createDataPartition(train$SalePrice, p = 0.60, list = FALSE)
training_part <- train[inTrain,]
test_part <- train[-inTrain,]

# Find zero varience factors using caret
nzv_data <- nearZeroVar(training_part, saveMetrics = TRUE)
nzv_T <- which(training_part$nzv == TRUE)  # nzv_T finds those columns which have 
nzv_T
# Remove zero variance factors
# drop.cols <- rownames(nzv.data)[nzv.data$nzv == TRUE]
# training_part <- training_part[,!names(training_part) %in% drop.cols]
dim(training_part)

# Find col num: which( colnames(df_new) == "SalePrice" )
mod_rf_test <- randomForest(SalePrice ~ 
                              ., data = test_part)
start_time <- Sys.time()
mod_rf_test_caret <- train(SalePrice ~ ., data = test_part, method = "rf")
end_time <- Sys.time()
paste("Total time it took to run the model was",difftime(end_time, start_time, units = "min"), "minutes.")

# ,na.action = na.roughfix ) # input median/mode

# Step within modeling: Remove outliers or data that may be bad 

# Initial model: linear regression
set.seed(2000)
mod1 <- lm(SalePrice ~ LotFrontage + SaleType,data = training_part)
summary(mod1)
par(mfrow = c(2,2))
plot(mod1)

prediction_lm <- predict(mod1, newdata = test_part)
rmse(log(test_part$SalePrice),log(prediction_lm))

# Select pvalues
summary(mod1)$coefficients[,4]
summary(mod1)$r.squared

# Create control hyperparameter for rf tuning
control <- trainControl(method = "repeatedcv",
                        search = "random",
                        repeats = 3,
                        number = 10)

tunegrid <- expand.grid(.mtry = sqrt(ncol(training_part))) # store best mtry value

# Tune rf model hyperparameters
# Evaluate the rf model with the default setting
set.seed(5000)
start_time2 <- Sys.time()
mod_rf <- train(SalePrice ~ .,
                data = training_part, # test dataset 
                method = "rf",
                # trControl = control,
                importance = TRUE
                # mtry
                # ntree
                # tuneLength
                # tuneGrid
)
end_time2 <- Sys.time()

print(mod_rf)
plot(mod_rf)
importance(mod_rf)
varImpPlot(mod_rf)

# Find the best number of mtry
start_time3 <- Sys.time()
set.seed(9999)
tunegrid <- expand.grid(.mtry = seq(5,80,5)) # construct vector with values 1:20
rf_mtry <- train(SalePrice ~.,
                 data = training_part,
                 method = "rf",
                 tuneGrid = tunegrid,
                 trControl = control,
                 importance = TRUE
                 # ntree = 500
)
end_time3 <- Sys.time()
best_mtry <- rf_mtry$bestTune$mtry # best value of mtry is stored in here
best_mtry

# Find the best number of maxnodes (create loop to evaluate); largest value with best acc
tunegrid <- expand.grid(.mtry = best_mtry) # store best mtry value

store_maxnode <- list() # results of the model will be stored here
start_time4 <- Sys.time()
for (maxnodes in c(3:30)) { # compute the model with values of maxnodes starting from 3:30
  set.seed(10000)
  rf_maxnode <- train(SalePrice ~ .,
                      data = training_part,
                      method = "rf",
                      tuneGrid = tunegrid,
                      trControl = control,
                      importance = TRUE,
                      maxnodes = maxnodes
                      # ntree = 500
  )
  current_iteration <- toString(maxnodes) # store as a string variable the value of maxnode
  store_maxnode[[current_iteration]] <- rf_maxnode # save result of the model in the list
}
end_time4 <- Sys.time()
results_mtry <- resamples(store_maxnode) # arrange the results of the model
summary(results_mtry) # print summary of all the combinations

# Find the best number of ntrees
store_maxtrees <- list()
start_time6 <- Sys.time()
for (ntree in c(250, 500, 800, 1000, 2000, 2200, 2400, 2600)) {
  set.seed(5678)
  rf_maxtrees <- train(SalePrice ~ .,
                       data = training_part,
                       method = "rf",
                       tuneGrid = tunegrid,
                       trControl = control,
                       importance = TRUE,
                       maxnodes = 24, # find best maxnodes
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
end6 <- Sys.time()
results_tree <- resamples(store_maxtrees)
summary(results_tree)

# Evaluate the model on the test dataset with new hyperparameters
set.seed(19151)
start_time5 <- Sys.time()
mod_rf_tuned <- train(SalePrice ~ .,
                      data = test_part,
                      method = "rf",
                      # tuneGrid = tunegrid,
                      trControl = control,
                      importance = TRUE,
                      ntree = 1000,
                      # mtry = 17
                      maxnodes = 20
)
end_time5 <- Sys.time()
paste("Total time it took to run the ntree was",difftime(end_time5, start_time5, units = "mins"), "minutes.")  

set.seed(2006)
start_time7 <- Sys.time()
mod_rf_tuned3 <- train(SalePrice ~ .,
                      data = training_part,
                      method = "rf",
                      # tuneGrid = tunegrid,
                      trControl = control,
                      importance = TRUE,
                      preProc = c("center", "scale"),
                      ntree = 1001
                      # mtry = 137,
                      # maxnodes = 21
)
end_time7 <- Sys.time()
paste("Total time it took to run the ntree was",difftime(end_time7, start_time7, units = "mins"), "minutes.")            # 2 tuned rf model

print(mod_rf_tuned3)
plot(mod_rf_tuned3, main = "Tuned RF model")
importance(mod_rf_tuned3) # have to use randomForest pkg
varImp(mod_rf_tuned3)

## Warnings from preprocessing indicates: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# These variables have zero variances: 
# MSZoning, Utilities, Condition2, 
# RoofStyle RoofMatl, Exterior1st,
# HeatingQC, Functional, MiscFeature, Exterior2nd

# ## Subsetting technique
# df = subset(mydata, select = -c(x,z) )
# drop <- c("x","z")
# df = mydata[,!(names(mydata) %in% drop)]
# combiFac <- combiFac[,!colnames(combiFac) %in% 'LotShape']

all2 <- all
drop <- c("MSZoning", "Utilities", "Condition2", "RoofStyle",
          "RoofMatl", "Exterior1st", "HeatingQC", "Functional", 
          "MiscFeature", "Exterior2nd")
all2 <- all2[,!names(all2) %in% drop]

# Initial partition to train main model
train2 <- all2[1:1460,]
test2 <- all2[1461:nrow(all2),]

# Create testing partitions
inTrain2 <- createDataPartition(train2$SalePrice, p = 0.75, list = FALSE)
training_part2 <- train2[inTrain2,]
test_part2 <- train2[-inTrain2,]

set.seed(95959)
start_time8 <- Sys.time()
mod_rf_removed_var <- train(SalePrice ~ .,
                       data = training_part2,
                       method = "rf",
                       # tuneGrid = tunegrid,
                       trControl = control,
                       importance = TRUE,
                       preProc = c("center", "scale")
                       # ntree = 1001
                       # mtry = 137,
                       # maxnodes = 21
)
end_time8 <- Sys.time()
paste("Total time it took to run the model was",difftime(end_time8, start_time8, units = "mins"), "minutes.")            # 2 tuned rf model

print(mod_rf_removed_var)
plot(mod_rf_removed_var, main = "Tuned RF model")
# importance(mod_rf_removed_var) # have to use randomForest pkg
varImp(mod_rf_removed_var)

mod_rf_tuned3 <- train(SalePrice ~ .,
                            data = training_part,
                            method = "rf",
                            # tuneGrid = tunegrid,
                            trControl = control,
                            importance = TRUE,
                            preProc = c("center", "scale"),
                            ntree = 1001
                            # mtry = 137,
                            # maxnodes = 21
)

paste("Total time it took to run the model was",difftime(end_time, start_time, units = "mins"), "minutes.")               # test model
paste("Total time it took to run the untuned rf model was",difftime(end_time2, start_time2, units = "mins"), "minutes.") # mtry
paste("Total time it took to run the mtry model was",difftime(end_time3, start_time3, units = "mins"), "minutes.")       # maxnodes
paste("Total time it took to run the maxnodes model was",difftime(end_time4, start_time4, units = "mins"), "minutes.")   # ntree
paste("Total time it took to run the tuned rfmodel was",difftime(end_time5, start_time5, units = "mins"), "minutes.")    # tuned rf model
paste("Total time it took to run the ntree was",difftime(end_time6, start_time6, units = "mins"), "minutes.")            # tuned rf model
paste("Total time it took to run the ntree was",difftime(end_time7, start_time7, units = "mins"), "minutes.")            # 2 tuned rf model

# In case I miss the summary
results_tree <- resamples(store_maxtrees)
summary(results_tree)
results_mtry <- resamples(store_maxnode) # arrange the results of the model
summary(results_mtry) # print summary of all the combinations
best_mtry <- rf_mtry$bestTune$mtry # best value of mtry is stored in here
best_mtry

## check amelia package for missing values

### Step 5: Predicting values and testing RMSE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Predict values from models and measure error
# # Predict and test RMSE
# prediction1 <- predict(mod_rf, newdata = test)
# rmse(log(test_part$SalePrice),log(prediction1))
# 
# # Predict and test RMSE
# prediction2 <- predict(mod2, newdata = test_part)
# rmse(log(test_part$SalePrice),log(prediction2))
# 
# # Predict and test RMSE
# prediction3 <- predict(mod3, newdata = test_part)
# rmse(log(test_part$SalePrice),log(prediction3))

# Predict and test RMSE (removed var)
prediction_rf <- predict(mod_rf_removed_var, newdata = test_part2)
rmse(log(test_part2$SalePrice),log(prediction_rf))

ggplot(test_part2, aes(x = SalePrice, y = prediction_rf)) +
  geom_point(shape = 1) +    # Use hollow circles
  geom_smooth(method = lm)   # Add linear regression line 

# Predict and test RMSE
prediction_rf2 <- predict(mod_rf_tuned3, newdata = test_part)
rmse(log(test_part$SalePrice),log(prediction_rf2))

ggplot(test_part, aes(x = SalePrice, y = prediction_rf2)) +
  geom_point(shape = 1) +    # Use hollow circles
  geom_smooth(method = lm)   # Add linear regression line 

test_part[test_part$SalePrice > 500000,]s
test_part[test_part$prediction_rf2 > 400000,]

# Could remove these rows: id 441,409,1183

# ##### Error searching code if any ######
# # Find NA values 
# test[is.na(test$submission),]
# test[test$Id == 1556,]
##########################################

# Submit new values into test dataset
test$submission <- prediction_rf2

# test add mean values into NA
# test$submission2 <- test$submission
# NA_Values <- which(is.na(testing_all$submission2))
# test$submission2[is.na(test$submission)] <- mean(test$submission, na.rm = TRUE)


# Create new dataframe with submission values
submission <- data.frame(Id = test$Id, SalePrice = test$submission)
write.csv(submission, file = "C:/Users/Sky/Documents/R Projects/Kaggle/House Prices/submission.csv", row.names = FALSE)
