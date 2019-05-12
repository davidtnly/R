# Introduction

# Goal

# Load libraries
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
library(Metrics)
library(mice)
library(skimr)
library(pracma)
library(RColorBrewer)
library(plotly)
library(GGally)
library(knitr)
library(stats)
library(FactoMineR)

# Load Datasets
train <- read.csv('~/R Projects/Kaggle/House Prices/train.csv', stringsAsFactors = FALSE)
test <- read.csv('~/R Projects/Kaggle/House Prices/test.csv', stringsAsFactors = FALSE)

output_1 <- read.csv('~/R Projects/Kaggle/House Prices/output_1.csv', stringsAsFactors = FALSE)
output_2 <- read.csv('~/R Projects/Kaggle/House Prices/output_2.csv', stringsAsFactors = FALSE)


# Pre-processing 
setdiff(names(train), names(test)) # Find different col name to rbind()
test$SalePrice <- NA      # Add NA values to missing col
all <- rbind(train, test) # Combine datasets
skim(all)                 # Alternative to summary()

# Count NA values (Do both colSums() and sapply())
colSums(sapply(train, is.na)) 
na_counts <- sort(sapply(all, function(x) {sum(is.na(x))}), decreasing = TRUE)

# Check data
dim(all)
glimpse(all)

# Show top 20 NA columns
head(na_counts,20)

# Separate numerical and categorical variables
num_col <- names(which(sapply(all, is.numeric)))
char_col <- names(which(sapply(all, is.character)))

# Separate num and char data
all_num <- all[, num_col]
all_num <- all_num[,!colnames(all_num) == "SalePrice"]
all_char <- all[, char_col]
dim(all_num)
dim(all_char)

# Correlations
all_num <- na.omit(all_num)
correlations <- cor(all_num[,!colnames(all_num) == "Id"], use = "everything")

## Correlations without na.omit
# cor(use = "complete.obs") #
# cor() computes the correlation coefficient (R)
# cor.test() tests for correlation between paired samples

# Draw plot
par(mfrow = c(1,1))
corrplot(correlations, method = "circle",
         sig.level = 0.01, insig = "blank")

# Create variable to mark insignification correlations by p-value
cor_5 <- rcorr(as.matrix(all_num))
M <- cor_5$r
p_mat <- cor_5$P

colors2 <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(correlations,
         mar = c(0,0,1,0),
         method = "color",
         order = "hclust", # Hierarchical clustering
         p.mat = p_mat,    # Marks insignificant cols
         insig = "blank",  # Mark type
         col = colors2(200),
         addgrid = TRUE,
         # addCoef.col = "black", # Add coefficient of correlation
         sig.level = 0.05,
         type = "lower", 
         tl.cex = 0.75,    # Text Size
         tl.col = "black", # Text color
         tl.srt = 45       # Text angle
         )

# Add in columns with moderate to high correlations
nms <- c("BsmtFinSF1", "BsmtFullBath", "MasVnrArea",
         "GarageCars", "GarageArea", "TotalBsmtSF",
         "X1stFlrSF", "WoodDeckSF", "OverallQual",
         "YearRemodAdd", "YearBuilt", "GarageYrBlt",
         "FirePlaces", "LotFrontage", "LotArea", "Fireplace",
         "OpenPorchSF", "FullBath", "GrLivArea", "TotRmsAbvGrd")
all_num_high <- (all_num[,colnames(all_num) %in% nms])
correlations_high <- cor(all_num_high, use = "everything")

corrplot(correlations_high,
         method = "color",
         order = "hclust",
         sig.level = 0.05,
         type = "lower",
         tl.cex = 0.75,
         tl.col = "black",
         tl.srt = 45
         )

# Fix NA Values (Explore the variables more) ~~~~~~~~~~~~~~~~~~~~~~~~
# Factors contains NA values which means property does not have
# Categorical values
all$MiscFeature[is.na(all$MiscFeature)] <- "None"
all$Fence[is.na(all$Fence)] <- "None"
all$PoolQC[is.na(all$PoolQC)] <- "None"
all$FireplaceQu[is.na(all$FireplaceQu)] <- "None"
all$Alley[is.na(all$Alley)] <- "None"
all$SaleType[is.na(all$SaleType)] <- "None"
# Garage Variables
# all$GarageYrBlt[is.na(all$GarageYrBlt)] <- 0
all$GarageFinish[is.na(all$GarageFinish)] <- "None"
all$GarageQual[is.na(all$GarageQual)] <- "None"
all$GarageType[is.na(all$GarageType)] <- "None"
all$GarageCond[is.na(all$GarageCond)] <- "None"
# Other Variables
all$MSZoning[is.na(all$MSZoning)] <- "None"
all$Exterior1st[is.na(all$Exterior1st)] <- "None"
all$Exterior2nd[is.na(all$Exterior2nd)] <- "None"
all$Functional[is.na(all$Functional)] <- "None"
all$Utilities[is.na(all$Utilities)] <- "None"
all$Electrical[is.na(all$Electrical)] <- "None"
all$KitchenQual[is.na(all$KitchenQual)] <- "None"
# Bsmt Variables
all$BsmtCond[is.na(all$BsmtCond)] <- "None"
all$BsmtExposure[is.na(all$BsmtExposure)] <- "None"
all$BsmtQual[is.na(all$BsmtQual)] <- "None"
all$BsmtFinType2[is.na(all$BsmtFinType2)] <- "None"
all$BsmtFinType1[is.na(all$BsmtFinType1)] <- "None"
# Other
all$MasVnrType[is.na(all$MasVnrType)] <- "None"
all$MSZoning[is.na(all$MSZoning)] <- "None"

# NA Continuous variables (Use this if not MICE)
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

# Adding new features
all <- all %>% 
  mutate(YrRemodel_Diff = YearRemodAdd - YearBuilt) 
all$HouseAge <- (2018 - all$YearBuilt)
all$NewHouse <- (all$YearBuilt == all$YrSold) * 1
all$TotalFloorSF <- all$X1stFlrSF + all$X2ndFlrSF
all$TotalArea <- all$LotFrontage+ all$LotArea + all$MasVnrArea + all$TotalBsmtSF +
  all$TotalFloorSF + all$GrLivArea + all$GarageArea + all$WoodDeckSF +
  all$OpenPorchSF + all$EnclosedPorch + all$X3SsnPorch + all$ScreenPorch
all$OverallQualCond <- all$OverallCond + all$OverallQual
all$TotalHouse <- all$X1stFlrSF + all$X2ndFlrSF + all$TotalBsmtSF
all$TotalBsmt <- all$BsmtFinSF1 + all$BsmtUnfSF
all$TotalHouse_LotArea <- all$X1stFlrSF + all$X2ndFlrSF + all$BsmtFinSF1 = all$LotArea
all$PorchArea <- all$OpenPorchSF + all$EnclosedPorch + all$X3SsnPorch + all$ScreenPorch

# Overall quality = Quality * numeric features
all$Mod_year_qual <- all$YearBuilt*all$OverallQual      # overall condition
all$Mod_year_r_qual <- all$OverallQual*all$YearRemodAdd # quality x remodel
all$Mod_bsmt_qual <- all$OverallQual*all$TotalBsmtSF    # quality x basement size
all$Mod_livarea_qual <- all$OverallQual*all$GrLivArea   # quality x living area
all$Mod_qual_bath <- all$OverallQual*all$FullBath       # quality x baths
all$Mod_BsmtFinType1_Qual <- all$BsmtFinSF1 * all$OverallQual   

# Rewrite char/int columns
chr <- all[,sapply(all, is.character)]
int <- all[,sapply(all, is.integer)]

# Add "NA" to char variables that are NA
chr[is.na(chr)] <- "None"
chr_fac <- chr %>% 
  lapply(as.factor) %>% 
  as.data.frame()

# Bind new data and fix types
all_data <- bind_cols(chr_fac, int)
head(sort(colSums(sapply(all_data,is.na)),decreasing = TRUE),20)
all_data<- all_data %>% 
  select(-GarageYrBlt)
all_data$MoSold <- as.factor(all_data$MoSold)

# Use Mice pkg to fill NA for numeric variable using RF
# all_data <- bind_cols(chr_fac, int) %>% 
# micemod <- all_data %>% 
#   mice(method = "rf")
# all_data <- complete(micemod)
# dim(all_data)

# Remove columns that do not look correlated
# Create correlation plots
# all_data <- all_data[,!colnames(all_data) == "MiscVal"]
# all_data <- all_data[,!colnames(all_data) == "PoolArea"]
# all_data <- all_data[,!colnames(all_data) == "PoolQC"]
# all_data <- all_data[,!colnames(all_data) == "X3SsnPorch"]
# all_data <- all_data[,!colnames(all_data) == "MiscVal"]
# all_data <- all_data[,!colnames(all_data) == "BsmtFinSF2"]
# all_data <- all_data[,!colnames(all_data) == "MoSold"]
# all_data <- all_data[,!colnames(all_data) == "ScreenPorch"]
# all_data <- all_data[,!colnames(all_data) == "LowQualFinSF"]
# all_data <- all_data[,!colnames(all_data) == "EnclosedPorch"]

# Principal Component Analysis (PCA)
# The goal of PCA is to explain most of the variability in the data 
# with a smaller number of variables than the original data set.
# It finds a low-dimensional representation of a 
# data set that contains as much of the variation as possible.
# pca1 = PCA(all_data, graph = FALSE) # change var to int first
# 








# # Create testing partitions
# inTrain <- createDataPartition(all_data$SalePrice, p = 0.8, list = FALSE)
# train_part <- all_data[inTrain,]
# test_part <- all_data[-inTrain,]
# 
# # Create smaller train/test datasets
# train <- all_data[1:1460,]
# test <- all_data[1461:nrow(all_data),]
# 
# # Use a SVM model and predict new values for test data
# svm_model <- svm(SalePrice ~ .,
#                  data = train,
#                  cost = 3)
# svm_pred <- predict(svm_model, newdata = test, type = "raw")
# svm_pred_df <- as.data.frame(svm_pred)
# 
# 
# # Solution and write to a csv file
# solution <- data.frame(Id = test$Id, SalePrice = svm_pred_df$svm_pred )
# # write.csv(solution, "~/R Projects/Kaggle/House Prices/solution.csv", row.names = FALSE)
# 
# # Solution using Owen's average
# comb_solution <- data.frame(Id = test$Id,
#                             SalePrice = nthroot(output_1$SalePrice*output_2$SalePrice,3))
# # write.csv(comb_solution, "~/R Projects/Kaggle/House Prices/combined_solution.csv", row.names = FALSE)


