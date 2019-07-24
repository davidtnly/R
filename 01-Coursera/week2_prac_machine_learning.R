########Caret Package
#Note: check this for sum of professor of week 2: http://rstudio-pubs-static.s3.amazonaws.com/19460_cd418196d20548ae9d0f4a8e497f4a31.html


#The caret package (short for Classification And REgression Training) is a set of functions that attempt to streamline
#the process for creating predictive models. The package contains tools for:
#data splitting
#createDataPartition
#createResample
#createTimeSlices
#pre-processing (cleaning)
#preProcess
#Training/testing functions
#train
#predict
#feature selection
#model tuning using resampling
#variable importance estimation
#Model comparison
#confusionMatrix

#Machine Learning Algorithms
#Linear discriminant analysis
#Regression
#Naives Bayes
#Support Vector Machines
#Classification and Regression Trees
#Random Forests
#Boosting
#etc.

##Decision Tree - ID3 Algorithm (Note this is off topic to the contents of this course - Practical Machine Learning)
#Choose the attribute with the highest Information Gain (Pr * Log2(Pr)) (i.e. less Entropy)
#Note: the way Information gain is computed, it is very prone to overfitting, since the probability is just 
#sum of occurences inside a Data Set...
#Create branches for each value of attribute
#Repeat with remaining attributes

#To get arround Overfitting of Decision Trees, remove or aggregate the Subtrees that provide little 
#discriminatory power (C45)

##Random Forests
#Repeat k times:
#Draw a bootstrap sample from the dataset
#Train a decision tree
#Until the tree is max size:
#Choose the next leaf node;
#Select m attributes at random from the p available
#Pick the best attribute/split as usual
#Measure out-of-bag error
#Evaluate the samples that were not selected in the bootrap
#provides measures of strength (inverse error rate), correlation between trees (which increases the forest error rate), 
#and variable importance

#SPAM example - data splitting

#The base R function sample can be used to create a completely random
#sample of the data. The caret package has a function
#createDataPartition that conducts data splits within groups of the
#data.

library(caret); library(kernlab); data(spam)

#About createDataPartition:
#y = what output we want to split on, which is this case are the two types of messages (SPAM and non Spam). 
#p specifies the proportion of data that will exist in each chunk after splitting the data, 
#in this case we split into two chunks of 75% and 25%. 
#We then subset the data using the output from the createDataPartition function.

inTrain <- createDataPartition(y=spam$type, p=0.75,list=FALSE) # Partition data: 75% of the data to train the model
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
dim(training)
#[1] 3451   58
dim(testing)
#[1] 1150   58

#From the Caret package, using Generalized Linear Model with all variables:
set.seed(12345)
modelFit1 <- train(type ~ internet + address + free + credit, data=training, methd="glm")
modelFit
#Generalized Linear Model 

#3451 samples
#  57 predictor
#   2 classes: 'nonspam', 'spam' 

#No pre-processing
#Resampling: Bootstrapped (25 reps) 

#Summary of sample sizes: 3451, 3451, 3451, 3451, 3451, 3451, ... 

#Resampling results

# Accuracy  Kappa  Accuracy SD  Kappa SD
# 0.749     0.428  0.0148       0.0349 


#Using Generalized Linear Model with all variables increases accurecy substancially:
set.seed(12345)
modelFitAll <- train(type ~., data=training, method="glm")
modelFitAll

#Generalized Linear Model 

#3451 samples
#  57 predictor
#   2 classes: 'nonspam', 'spam' 

#No pre-processing
#Resampling: Bootstrapped (25 reps) 

#Summary of sample sizes: 3451, 3451, 3451, 3451, 3451, 3451, ... 

#sampling results

#  Accuracy  Kappa  Accuracy SD  Kappa SD
#  0.922     0.837  0.00673      0.0151  

modelFitAll$finalModel #Final model is general construct for glm models;


###Now, testing predicting power of model Test data:
predictions <- predict(modelFit, newdata=testing)
predictions
#Evaluating how good was the prediction model - use Confusion Matrix

confusionMatrix(predictions, testing$type)
#Confusion Matrix and Statistics

Reference
Prediction nonspam spam
nonspam     664   45
spam         33  408

Accuracy : 0.9322         
95% CI : (0.9161, 0.946)
No Information Rate : 0.6061         
P-Value [Acc > NIR] : <2e-16         

Kappa : 0.8573         
Mcnemars Test P-Value : 0.2129         

Sensitivity : 0.9527         
Specificity : 0.9007         
Pos Pred Value : 0.9365         
Neg Pred Value : 0.9252         
Prevalence : 0.6061         
Detection Rate : 0.5774         
Detection Prevalence : 0.6165         
Balanced Accuracy : 0.9267         

'Positive' Class : nonspam  


###Data Slicing (dividing data sets into training and test sets)

#notes: remember data slicing is key for Cross-validation (validating within the training sets, which is devided into training& test sets)
#There are several ways to do the data slicing for Croos Validation:
#Random subsampling (without replacement, e.g. no element can be selected more than once in the same sample)
#Leave out random N% of the data;
#K-fold Corss-Validation
#Select K folds without replace;
#Leave One Out (reiterate afterwards) Cross-Validation (LOOCV)
#Random sampling with replacement is called Bootstraping (Random sampling with Replacement, 
#e.g. an element may appear multiple times in the one sample)

#Ensembles: Combining Classifiers
#Bagging (Bootstrapping + Aggregating) - Average the results of several Bootstrap samples (regression, classification); 
#works great for overfit models;
#Decreases variance without changing bias;
#Doesn't help much with underfit/high bias models
#Boosting - instead of selecting data points randomly with the bootstrap, FAVOR misclassified points
#Initialize the weights
#Repeat, resample with respect to weights
#Retrain the model
#Recompute weights
#Dt+1(i) = Betat Dt(i)
#Example: Adaboost

#

###Option 1: createDataPartition function to slice
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain, ]
test <- spam[-inTrain, ]

##Option2: Slicing in K-Folds
#K-folds cross-validation and other split methods
#Split data into k number of folds based on the outcome we want to split on (y). This function returns a list of lists.
set.seed(12345)
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain=TRUE)
#To check how data is divided:
sapply(folds, length)
folds[[1]][1:10]

#Or for the test set:
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain = FALSE)
sapply(folds, length)

##Option 3: Resampling
set.seed(12345)
reSample <- createResample(y=spam$type, times=10, list=TRUE)
sapply(reSample,length)

##Option 4: Time Slices:
#This is for time series data slicing. This creates a window of 20 samples, and I plan to forecast 10 samples.
set.seed(12345)
tme <- 1:1000
timeSlices <- createTimeSlices(y=tme, initialWindow=20, horizon=10)
names(timeSlices)
#[1] "train" "test" 
timeSlices$train[[1]]
timeSlices$test[[1]]


##Training options in careat
args(train.default)

function (x,y, method = "rf", preProcess = NULL, ..., weights = NULL, metric = ifelse(is.factor(y), "Accuracy", "RMSE"), 
          maximize = ifelse(metric == "RMSE", FALSE, TRUE), trControl = trainControl(), tuneGrid = NULL, tuneLength = 3)
  
  
  #Continuous outcomes
  
  RMSE = root mean squared error
R2 = a form of linear agreement, from regression models
Categorical outcomes

Accuracy = fraction of correct
Kappa = a measure of concordance
#Then, there is the trainContol() function… This gives you much more control for specifying the model



####4.0 Basic data exploration with plotting

#Usually it is best to do some exploratory plotting with data to get a sense of the basic structure of your data, 
#and any quarks or anomalies.

library(ggplot2); library(ISLR); library(caret)
data(Wage); summary(Wage)

#Splitting data:
wageInTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
wageTraining <- Wage[wageInTrain, ]
wageTesting <- Wage[-wageInTrain, ]
dim(wageTraining); dim(wageTesting)
#[1] 2102   12
#[1] 898  12

###Trying first Plots:

#Histogram:
qplot(data=wageTraining, wage, geom="histogram")

featurePlot(x=wageTraining[,c('age', 'education','jobclass')], y=wageTraining$wage, plot='pairs')
#Note: featurePlot - from Caret Package

#Another optional graphic:
qplot(age, wage, data=wageTraining)
#Same by color with jobclass var
qplot(age, wage, colour=jobclass, data=wageTraining)

#qq
ggplot(data=wageTraining, aes(age, wage, color=education))+geom_smooth()

#Adding regression smoothers:
qq <- qplot(age, wage, colour=education, data=wageTraining)
qq + geom_smooth(method='lm', formula=y~x)


#Another useful method is to table the data
#The cut2 from the Hmisc package will cut a continuous variable into categorical 
#groups based on percentile/quartile data.
#Dividing the Wage data into categories:

library(Hmisc); library(gridExtra)
cutWage <- cut2(wageTraining$wage, g=3) #g - how many groups to seperate
table(cutWage)


#Now can pass the groups to a Boxplot:
p1 <- qplot(cutWage, age, data=wageTraining, fill=cutWage, geom=c("boxplot"))
p1

#To see with jitter (points overlay):
p2 <- qplot(cutWage, age, data=wageTraining, fill=cutWage, geom=c("boxplot", "jitter"))
grid.arrange(p1,p2, ncol=2)

#Alternatively, the same thing, but dividing into two groups simply:
cutWage <-cut(train$wage,breaks=c(0, 250, max(train$wage)))
p1<-ggplot(data=train, aes(cutWage, age, fill=cutWage))+geom_boxplot()
p2<-ggplot(data=train, aes(cutWage, age, fill=cutWage))+geom_boxplot()+geom_jitter(alpha=0.2)
grid.arrange(p1,p2, ncol=2)

t1<-table(cutWage, wageTraining$jobclass)
t1
cutWage         1. Industrial 2. Information
[ 24.0, 92.2)           450            251 #more jobs in industrial and lower age
[ 92.2,118.9)           356            368
[118.9,314.3]           266            411

prop.table(t1, 1) # 1 for proportion in each row, 2 for columns
cutWage         1. Industrial 2. Information
[ 24.0, 92.2)     0.6419401      0.3580599
[ 92.2,118.9)     0.4917127      0.5082873
[118.9,314.3]     0.3929099      0.6070901

#Density plots
qplot(wage, colour=education, data=wageTraining, geom="density")




###PreProcessing Data - data may look very strange, and thus it can be easily transformed and processed;
#Back to spam data
spamTrain <- training
spamTest <- testing

hist(spamTrain$capitalAve, xlab='avg capital run length')
#Here we see that the variable is highly skewed, this skew, especially severe skew is likely to trick up many 
#machine learning techniques, since the variance is larger because of some extreme values. One way to remedy this is 
#standardize variables. One way to do this is to remove the mean value, and divide by standard deviation. 
#This process will center the data around 0 with a standard deviation of 1.
mean(spamTrain$capitalAve)
#[1] 4.847419
sd(spamTrain$capitalAve)
#[1] 29.0158  #High Standard deviation

##Standardizing Variables
trainCapAvg<-spamTrain$capitalAve
trainCapAvgS<-(trainCapAvg - mean(trainCapAvg)) / sd(trainCapAvg)
mean(trainCapAvgS); sd(trainCapAvgS)
[1] -4.863223e-18 #close to zero
[1] 1 
##NOTE: Standardizing the TEST SET: must USE the mean+sd of TRAINING set (not test):
testCapAvg <- spamTest$capitalAve
testCapAvgS<-(testCapAvg - mean(trainCapAvg)) / sd(trainCapAvg)
mean(testCapAvgS); sd(testCapAvgS) 
[1] 0.04744603
[1] 1.335302

#USing the Caret Package (preProcess function) to do exactly the same data transformation as before:
preObj <- preProcess(spamTrain[,-58], method=c("center", "scale"))
trainCapAvgS <- predict(preObj, spamTrain[,-58])$capitalAve
mean(trainCapAvgS); sd(trainCapAvgS)
[1] -4.863223e-18
[1] 1

testCapAvgS <- predict(preObj, spamTest[,-58])$capitalAve
mean(testCapAvgS); sd(testCapAvgS)


#OR: sending the preProcess argument in the train argument:
set.seed(12345)
modelFit <- train(type ~., data=spamTrain, preProcess=c("center", "scale"), method="glm")
modelFit
#Using the Box-Cox transformation:
preObj2 <- preProcess(spamTrain[,-58], method=c("center", "scale"))
trainCapAvgS2 <- predict(preObj, training[,-58])$capitalAve
par(mfrow = c(1,2)); hist(trainCapAvgS2); qqnorm(trainCapAvgS2)



#What about if a dataset has missing data

#We can use imputation to estimate the missing values. There are a lot of very complex ways to do this. 
#One simple way is knn classification; this algorithm tries to find the closest observations across all the variables
# and then takes the average of variable that is being imputed.
set.seed(12234)

#Articifially make some values NA
spamTrain$capAve<-spamTrain$capitalAve # Make a copy
selectNA<-rbinom(dim(spamTrain)[1], size=1, prob=0.05)==1
spamTrain$capAve[selectNA]<-NA

# Impute and standardize missing values
pre<-preProcess(spamTrain[,-58], method='knnImpute') #Using K Nearest Neighbours Imputation
capAve<-predict(pre, spamTrain[,-58])$capAve

# Standardize true values
tmp<-spamTrain$capitalAve
capAveTruth<-(tmp - mean(tmp)) / sd(tmp)

# Inspect the results, to check how close the Imputed values (in simulated NAs) were towards the true values;
quantile(capAve - capAveTruth)

quantile(capAve - capAveTruth)[selectNA]


quantile(capAve - capAveTruth)[!selectNA]


#6.0 Feature creation and engineering

#Feature creation involves transforming or manipulating raw data into a format that is useful for predicting.

#For example, the SPAM dataset we have been working with went through this process so that we can work with it.

#The original data may have been…

HI WHATS IS UP JOHN ID LIKE TO TELL YOU ABOUT THIS AMAZING CREDITCARD OFFERING WE HAVE TODAY SAVE $$$$$ For this email we could calculate:
  
  #The fraction of letters that are capital letters
  #How many times the email contains the word “you”
  #The number of $ signs.
  #This step involves a lot of clever thinking about the data you have and the results that you want for your particular application. We have to balance the compression/quantification of the data, with information loss.
  
  #Text files: frequency of words, frequency of phrases
  #Images: Edges, blobs, ridges, corners, colors
  #People: Height, weight, sex, income
  
  #Good features explain a significant amount of information behind the phenomenon in the data. The more knowledge you 
#have of your system, the better you will be a creating features.

##Covariate creation
#Covariance - sometimes predictor, sometimes features
#Second step: Tidy covariates -> new covariates 
#More necessary for some methods (regression, svms) than others (classification trees)
#Should be done only on the training Set
#The best approach is through exploratory analysis (plotting/tables)
#New covariates should be added to data frames

#Examples

#Creating dummy variables
library(ISLR); library(caret); data(Wage)
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain, ]; testing <- Wage[-inTrain, ]
#So basic idea is to convert factor variables to indicator variables
table(training$jobclass)
#Creating dummy variables
dummies <- dummyVars(wage ~ jobclass, data=training)
head(predict(dummies, newdata=training))

#NearZeroVar (Near Zero Variance Variable) caret function helps researcher identify which variables have very little variability, and are thus 
#less likely to influence/help predict any outcome
nsv <- nearZeroVar(training, saveMetrics=TRUE)
nsv #this shows a table with all variables, indicating which are NZV
#We can see in this obvious example, that sex and region actually have zero variance.


#Non-linear relationships

#Once we have the variables, it still may be necessary to transform these data to make them more representative of 
#the features we are trying to capture with the data. For example, age may be an important variable in our model, 
#but age2, or age3 may also serve as functional predictors, depending on the application. Here we utilize spline basis 
#function. The output in the first column represents the standardized age, the second represents the quadratic age, 
#and the third column represents cubic age.

#To see a curvey model, transform to a polynomial model
library(splines)
bsBasis<-bs(training$age, df=3) # fits 3rd order polynomial
head(bsBasis)

lm1 <- lm(wage ~ bsBasis, data=training)
plot(training$age, training$wage, pch=19, cex=0.5)
points(training$age, predict(lm1, newdata=training), col="red", pch=10, cev=0.5)

#Splines on the test Set
predict(bsBasis, age=testing$age)
