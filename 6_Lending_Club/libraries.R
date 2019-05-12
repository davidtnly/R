################## List of libraries that are used frequently ################## 
# if (require() == FALSE) {install.packages("")} else library()
################################################################################

# Data manipulation
if (require(plyr)            == FALSE) {install.packages("plyr")}            else library(plyr)
if (require(tidyverse)       == FALSE) {install.packages("tidyverse")}       else library(tidyverse)
if (require(knitr)           == FALSE) {install.packages("knitr")}           else library(knitr)
if (require(GGally)          == FALSE) {install.packages("GGally")}          else library(GGally)
if (require(funModeling)     == FALSE) {install.packages("funModeling")}     else library(funModeling)
if (require(Hmisc)           == FALSE) {install.packages("Hmisc")}           else library(Hmisc)      # Data analysis, graphics, imputing missing values
if (require(missForest)      == FALSE) {install.packages("missForest")}      else library(missForest) # Non parametric imputation method (does not make explicit assumptions about form)
if (require(mice)            == FALSE) {install.packages("mice")}            else library(mice)       # Creates multiple imputations to take care of uncertainty in missing values

# Data visualizing
if (require(RColorBrewer)    == FALSE) {install.packages("RColorBrewer")}    else library(RColorBrewer)
if (require(gridExtra)       == FALSE) {install.packages("gridExtra")}       else library(gridExtra)
if (require(tidyquant)       == FALSE) {install.packages("tidyquant")}       else library(tidyquant)
if (require(choroplethrMaps) == FALSE) {install.packages("choroplethrMaps")} else library(choroplethrMaps)
if (require(ggthemes)        == FALSE) {install.packages("ggthemes")}        else library(ggthemes)
if (require(maptools)        == FALSE) {install.packages("maptools")}        else library(maptools)

# Predictive Modeling
if (require(caret)           == FALSE) {install.packages("caret")}           else library(caret)
if (require(xgboost)         == FALSE) {install.packages("xgboost")}         else library(xgboost)
if (require(corrplot)        == FALSE) {install.packages("corrplot")}        else library(corrplot)
if (require(e1071)           == FALSE) {install.packages("e1071")}           else library(e1071)
if (require(doParallel)      == FALSE) {install.packages("doParallel")}      else library(doParallel)
if (require(Matrix)          == FALSE) {install.packages("Matrix")}          else library(Matrix)
if (require(rsample)         == FALSE) {install.packages("rsample")}         else library(rsample)

# Validation
if (require(pROC)            == FALSE) {install.packages("pROC")}            else library(pROC) # Used to display and analyze ROC curves
if (require(ROCR)            == FALSE) {install.packages("ROCR")}            else library(ROCR) # Used to visualize the performance of scoring classifiers
if (require(ModelMetrics)    == FALSE) {install.packages("ModelMetrics")}    else library(ModelMetrics) 
if (require(MLmetrics)       == FALSE) {install.packages("MLmetrics")}       else library(MLmetrics) 
if (require(PRROC)          == FALSE) {install.packages("PRROC")}            else library(PRROC) 

# Spreadsheets
if (require(xlsx)            == FALSE) {install.packages("xlsx")}            else library(xlsx)