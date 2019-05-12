library(tidyverse)
library(caret)
library(gridExtra)
library(corrplot)
library(Hmisc)
library(knitr)
library(gridExtra)
library(randomForest) 
library(ModelMetrics)

## https://www.kaggle.com/c/house-prices-advanced-regression-techniques

setwd('C:/Users/Sky/Documents/R/win-library/3.4')

# Load Data
# all <- read.csv('C:/Users/Sky/Documents/R Projects/Kaggle/House Prices/all.csv', stringsAsFactors = FALSE, na.strings = c("NA",""))
# test <- read.csv('C:/Users/Sky/Documents/R Projects/Kaggle/House Prices/test.csv', stringsAsFactors = FALSE, na.strings = c("NA",""))
train <- read.csv('C:/Users/Sky/Documents/R Projects/Kaggle/House Prices/train.csv', stringsAsFactors = FALSE)
test <- read.csv('C:/Users/Sky/Documents/R Projects/Kaggle/House Prices/test.csv', stringsAsFactors = FALSE)

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

# Sample facet graph of Neighborhoods
# ggplot(all, aes(GrLivArea, SalePrice)) + 
#   geom_point(aes(color = Neighborhood)) + 
#   scale_x_continuous("GrLivArea") +
#   scale_y_continuous("SalePrice") +
#   theme_bw() + facet_wrap( ~ Neighborhood) +
#   theme(legend.position="none")

# # Create correlation plots
# par(mfrow = c(1,1))
# correlations<- cor(all_int[,-1],use = "everything")
# corrplot(correlations, method = "circle", type = "lower",  sig.level = 0.01, insig = "blank")
# 
# # Corr to LotArea, Street, LotShape, LandContour
# correlations <- cor(all[,c(5,6,7,8, 16:25)], use = "everything")
# corrplot(correlations, method = "circle", type = "lower",  sig.level = 0.01, insig = "blank")
# 
# correlations <- cor(all_int[,c(5,6,7,8, 16:25)], use = "everything")
# corrplot(correlations, method = "circle", type = "lower",  sig.level = 0.01, insig = "blank")
# 
# correlations <- cor(all_int[,c(5,6,7,8, 26:35)], use = "everything")
# corrplot(correlations, method = "circle", type = "lower",  sig.level = 0.01, insig = "blank")

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

# Update variable types
# all$HouseStyle <- as.factor(all$HouseStyle)
# all$HouseStyle <- as.integer(all$HouseStyle) # Original type

# All: Update data to 0 or 1 if possible to easier modeling
all$StreetType[all$Street == "Pave"] <- 1
all$StreetType[all$Street != "Pave"] <- 0
all$LotType[all$LotShape == "Reg"] <- 1
all$LotType[all$LotShape != "Reg"] <- 0
# all$Mod_MSZoning[all$MSZoning %in% c("RH","RL","RP","RM")] <- "Residential"
# all$Mod_MSZoning[is.na(all$MSZoning)] <- "Non-Residential" #fix later
all$Mod_LotFrontage <- ifelse(is.na(all$LotFrontage),0,all$LotFrontage)
# all$Mod_Fireplace <- 2
# all$Mod_Fireplace[is.na(all$FireplaceQu) | all$FireplaceQu %in% c("Fa","Po")] <- 0
# all$Mod_Fireplace[all$FireplaceQu %in% c("Ex","Gd","TA")] <- 1
# all$FireplaceQu[all$Mod_Fireplace == 2] # Find the 53 that is still = 2 and fix
# all$Mod_Fireplace <- factor(all$Mod_Fireplace, levels = c(1,0))
all$Mod_Pool <- ifelse(all$PoolArea > 0, 1, 0)
all$KitchenQual[is.na(all$KitchenQual)] <- "TA"
# Kitchen quality is not a nominal category - update to integer
all$Mod_KitchenQual[all$KitchenQual == "Ex"] <- 5 
all$Mod_KitchenQual[all$KitchenQual == "Gd"] <- 4 
all$Mod_KitchenQual[all$KitchenQual == "TA"] <- 3 
all$Mod_KitchenQual[all$KitchenQual == "Fa"] <- 2 
all$Mod_KitchenQual[all$KitchenQual == "Po"] <- 1
all$Mod_BsmtCond[all$BsmtCond == "Ex"] <- 5 
all$Mod_BsmtCond[all$BsmtCond == "Gd"] <- 4 
all$Mod_BsmtCond[all$BsmtCond == "TA"] <- 3 
all$Mod_BsmtCond[all$BsmtCond == "Fa"] <- 2 
all$Mod_BsmtCond[all$BsmtCond == "Po"] <- 1 
all$Mod_KitchenQual <- as.integer(all$Mod_KitchenQual)
all$Mod_BsmtCond <- as.integer(all$Mod_BsmtCond)
all <- all %>% 
  mutate(YrRemodel_Diff = YearRemodAdd - YearBuilt) 
all$Mod_HouseAge <- (2018 - all$YearBuilt)
# all$Mod_MonthYear <- paste(all$MoSold, "", all$YrSold)
all$Mod_NewHouse <- (all$YearBuilt == all$YrSold) * 1
# all$Mod_GarageAttached <- ifelse(all$GarageType == "Attched", 1, 0)
# all$Mod_GarageAttached <- as.integer(all$Mod_GarageAttached)
# all$BsmtQual[is.na(all$BsmtQual)] <- 0
# all$BsmtExposure[is.na(all$BsmtExposure)] <- "None"
# all[all ==  "NA"] <- "None"

# Quantitative areas
all$Mod_TotalArea <- all$LotFrontage+ all$LotArea + all$MasVnrArea + all$TotalBsmtSF +
  all$X1stFlrSF + all$X2ndFlrSF + all$GrLivArea + all$GarageArea
all$Mod_OverallQualCond <- all$OverallCond + all$OverallQual
all$Mod_FlrSF <- all$X1stFlrSF + all$X2ndFlrSF
all$LotFrontage[is.na(all$LotFrontage)] <- 0 

# All: Interactions based on correlation
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

# Fix rest of the NA values
all$MasVnrType[is.na(all$MasVnrType)] <- "None"
all$MasVnrArea[is.na(all$MasVnrArea)] <- 0
all$BsmtQual <- as.double(all$BsmtQual)
all$BsmtQual[is.na(all$BsmtQual)] <- "NonExist"
all$BsmtQual <- as.factor(all$BsmtQual)

all$BsmtCond <- as.double(all$BsmtCond)
all$BsmtCond[is.na(all$BsmtCond)] <- "NonExist"
all$BsmtCond <- as.factor(all$BsmtCond)

all$BsmtExposure <- as.double(all$BsmtExposure)
all$BsmtExposure[is.na(all$BsmtExposure)] <- "NonExist"
all$BsmtExposure <- as.factor(all$BsmtExposure)

all$BsmtFinType1 <- as.integer(all$BsmtFinType1)
all$BsmtFinType1[is.na(all$BsmtFinType1)] <- 0
all$BsmtFinType1 <- as.factor(all$BsmtFinType1)

all$BsmtFinType2 <- as.integer(all$BsmtFinType2)
all$BsmtFinType2[is.na(all$BsmtFinType2)] <- 0
all$BsmtFinType2 <- as.factor(all$BsmtFinType2)
# all[is.na(all$BsmtFinSF1),] # 2121
all$Electrical <- as.factor(all$Electrical)
all$Electrical[is.na(all$Electrical)] <- "SBrkr"
all$BsmtFullBath[is.na(all$BsmtFullBath)] <- 0
all$BsmtHalfBath[is.na(all$BsmtHalfBath)] <- 0
all$Functional[is.na(all$Functional)] <- "Typ"

all$FireplaceQu <- as.character(all$FireplaceQu)
all$FireplaceQu[is.na(all$FireplaceQu)] <- "NonExist"
all$FireplaceQu <- as.factor(all$FireplaceQu)

# change stringAsFactors = FALSE  to avoid ~~~~~~~~~~~~~~~~
all$GarageType <- as.character(all$GarageType)
all$GarageType[is.na(all$GarageType)] <- "NonExist"
all$GarageType <- as.factor(all$GarageType)

all$GarageYrBlt[is.na(all$GarageYrBlt)] <- "NonExist"

all$GarageFinish <- as.character(all$GarageFinish)
all$GarageFinish[is.na(all$GarageFinish)] <- "NonExist"
all$GarageFinish <- as.factor(all$GarageFinish)

all$GarageCars[is.na(all$GarageCars)] <- 0
all$GarageArea[is.na(all$GarageArea)] <- 0

all$GarageQual <- as.character(all$GarageQual)
all$GarageQual[is.na(all$GarageQual)] <- "NonExist"
all$GarageQual <- as.factor(all$GarageQual)

all$GarageCond <- as.character(all$GarageCond)
all$GarageCond[is.na(all$GarageCond)] <- "NonExist"
all$GarageCond <- as.factor(all$GarageCond)

all$PoolQC <- as.character(all$PoolQC)
all$PoolQC[is.na(all$PoolQC)] <- "NonExist"
all$PoolQC <- as.factor(all$PoolQC)

all$Fence <- as.character(all$Fence)
all$Fence[is.na(all$Fence)] <- "NonExist"
all$Fence <- as.factor(all$Fence)

all$MiscFeature <- as.character(all$MiscFeature)
all$MiscFeature[is.na(all$MiscFeature)] <- "NonExist"
all$MiscFeature <- as.factor(all$MiscFeature)

all$SaleType <- as.character(all$SaleType)
all$SaleType[is.na(all$SaleType)] <- "NonExist"
all$SaleType <- as.factor(all$SaleType)

all$Mod_BsmtCond[is.na(all$Mod_BsmtCond)] <- 0

all$Mod_TotalArea[is.na(all$Mod_TotalArea)] <- median(all$Mod_TotalArea, na.rm = TRUE)

all$MSZoning[is.na(all$MSZoning)] <- "RL"

# Initial partition (Not for this model)
# inTrain <- createDataPartition(all$SalePrice, p = 0.75, list = FALSE)
# train <- all[inTrain,]
# test <- all[-inTrain,]

# All: Initial partition
train <- all[1:1460,]
test <- all[1461:nrow(all),]
train_rf <- all[1:200,]

df_new <- all %>% 
  mutate_if(is.character, as.factor)

train_new <- df_new[1:1460,-c(59)]
test_new <- df_new[1461:nrow(df_new),-c(59)]

train_new2 <- train_new[1:1460,]

# Find col num: which( colnames(df_new)=="SalePrice" )
mod_rf_test <- randomForest(SalePrice ~ 
                              ., data = train_new2)
# ,na.action = na.roughfix ) # input median/mode

# new_DF <- subset(train, is.na(train))

# Initial model
set.seed(2000)
mod1 <- lm(SalePrice ~ MSSubClass + LotFrontage + LotArea +
             StreetType + LotType + Mod_LotFrontage +
             Mod_Fireplace + HouseStyle + BsmtUnfSF +
             OverallCond + YearBuilt + YearRemodAdd +
             MasVnrArea + ExterCond + BsmtQual + BsmtCond +
             HeatingQC + X1stFlrSF + X2ndFlrSF + FullBath +
             HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual +
             GarageType + GarageCars + GarageQual +
             PavedDrive + WoodDeckSF + OpenPorchSF + EnclosedPorch +
             ScreenPorch + PoolArea  +
             MiscVal + YrSold + MoSold + SaleType + SaleCondition +
             Mod_qual_bath + Mod_livarea_qual + Mod_bsmt_qual +
             Mod_year_r_qual + Mod_year_qual,  
           data = train)
summary(mod1)
par(mfrow = c(2,2))
plot(mod1)

# Select pvalues
summary(mod1)$coefficients[,4]
summary(mod1)$r.squared

# New model based on new variables
set.seed(3000)
mod2 <- lm( SalePrice ~ MSSubClass + LotArea + Mod_Fireplace + HouseStyle +
              BsmtUnfSF + OverallCond + YrRemodel_Diff + MasVnrArea +
              BsmtQual + HeatingQC + GarageCars + GarageQual + OpenPorchSF +
              X1stFlrSF + X2ndFlrSF + FullBath + HalfBath + KitchenQual +
              ScreenPorch + PoolArea + Mod_qual_bath + Mod_bsmt_qual,
            data = train
)
summary(mod2)

# All: New model based on new variables
set.seed(4)
mod_lm <- lm(formula = SalePrice ~ MSSubClass + LotArea + Mod_Fireplace + 
               BsmtUnfSF + OverallCond + YrRemodel_Diff + MasVnrArea + 
               BsmtQual + GarageCars + Mod_FlrSF + 
               FullBath + HalfBath + KitchenQual + ScreenPorch + PoolArea + 
               Mod_qual_bath + TotRmsAbvGrd +  
               Mod_HouseAge + Mod_TotalArea + Mod_OverallQualCond
             , data = train)

# MiscFeature (really bad predictor)
summary(mod_lm)

mod_test <- lm(formula = SalePrice ~ MSSubClass + LotArea + Mod_Fireplace + 
                 HouseStyle + BsmtUnfSF + OverallCond + YrRemodel_Diff + MasVnrArea + 
                 BsmtQual + HeatingQC + GarageCars + X1stFlrSF + X2ndFlrSF + 
                 FullBath + HalfBath + KitchenQual + ScreenPorch + PoolArea + 
                 Mod_qual_bath + TotRmsAbvGrd, data = train)
summary(mod_test)

control <- trainControl(method = "repeatedcv",
                        search = "random",
                        repeats = 3,
                        number = 3)

set.seed(5000)
mod_rf <- train(SalePrice ~ MSSubClass,
                data = train, # test dataset 
                method = "rf",
                # trControl = control,
                importance = TRUE,
                na.action = na.pass
                # mtry
                # ntree
                # tuneLength
)
print(mod_rf)
plot(mod_rf)
importance(mod_rf)
varImpPlot(mod_rf)


## check amelia package for missing values

# Predict and test RMSE
prediction2 <- predict(mod2, newdata = train)
rmse(log(train$SalePrice),log(prediction2))

# All: Predict and test RMSE
prediction3 <- predict(mod_rf_test, newdata = test)

# Predict RF
prediction4 <- predict(mod4, newdata = train)
rmse(log(train$SalePrice),log(prediction4))

## issue predict() is producing NA values
# Find NA values 
test[is.na(test$submission),]
test[test$Id == 1556,]


rmse(log(test$SalePrice),log(prediction3))

# Submit new values into test dataset
test$submission <- prediction3
# test add mean values into NA
test$submission2 <- test$submission
# NA_Values <- which(is.na(testing_all$submission2))
test$submission2[is.na(test$submission)] <- mean(test$submission, na.rm = TRUE)


# Create new dataframe with submission values
submission2 <- data.frame(Id = test$Id, SalePrice = test$submission2)
write.csv(submission2, file = "C:/Users/Sky/Documents/R Projects/Kaggle/House Prices/submission.csv", row.names = FALSE)
