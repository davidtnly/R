#Spam example
library(kernlab)
data(spam)
head(spam)
#Look at the frequency of "your" word and plot it:
plot(density(spam$your[spam$type=="nonspam"]), col="blue", main="", xlab="Frequency of 'your'")
lines(density(spam$your[spam$type=="spam"]), col="red")
abline(v=0.5, col="black")
#Prediction
prediction <- ifelse(spam$your > 0.5, "spam", "nonspam")
table(prediction, spam$type)/length(spam$type)
#prediction   nonspam      spam
#   nonspam 0.4590306 0.1017170
#   spam    0.1469246 0.2923278
#Accuracy ~ 0.459+0.292 = 0.751 (75% accurate)

###In sample VS out of sample
# In sample Error: error rate you get on the same data set you used to build your predictor
# Out of sample Error: Error rate you get on a new data set; sometimes called generalization

#Key Ideas
# 1. Out of sample error is what you care about
# 2. In sample error < out of sample error
# 3. The reason for overfitting:
# Matching your algorithm to the data you have.

library(kernlab); data(spam); set.seed(333)
smallSpam <- spam[sample(dim(spam)[1], size = 10), ] #take a small sample of 10 samples
spamLabel <- (smallSpam$type=="spam")*1 + 1
plot(smallSpam$capitalAve, col=spamLabel) #capital letters
#Conclusion: spam messages have a lot more capital letters;

#Applying Rule 1 to smallSpam:
rule1 <- function(x) {
  prediction <- rep(NA, length(x))
  prediction[x > 2.7] <- "spam"
  prediction[x < 2.40] <- "nonspam"
  prediction[(x >= 2.40 & x <= 2.45)] <- "spam"
  prediction[(x > 2.45 & x <= 2.70)] <- "nonspam"
  return(prediction)
}
table(rule1(smallSpam$capitalAve), smallSpam$type) #this rule makes perfect accuracy, in sample error => overfitting, capturing both signal AND noise;

#Applying Rule 2 to smallSpam
rule2 <- function(x) {
  prediction <- rep(NA, length(x))
  prediction[x > 2.8] <- "spam"
  prediction[x <= 2.8] <- "nonspam"
  return(prediction)
}
table(rule2(smallSpam$capitalAve), smallSpam$type) #this rules makes almost perfect accuracy, in sample error => overfitting, capturing both signal AND noise;

#Now comparing against the complete spam data:
table(rule1(spam$capitalAve), spam$type)
table(rule2(spam$capitalAve), spam$type)

##### General Rules of thumb
#randomly sample training and test; set test/validation aside and don't look at it
#All subsets should reflect as much diversity as possible (random subsets and balancing by features)

##Large sample size
#60% training
#20% test
#20% validation

##Medium sample size
#60% training
#40% test

##Small sample size
#Do cross validation
#Report caveat of small sample size



True Positive (TP) - Sick people correctly diagnosed as sick
False Positive (FP) - Healthy people incorrectly identified as sick
True Negative (TN) - Healthy people correctly identified as healthy
False Negative (FN) - Sick people incorrectly identified as healthy

TP   FP
FN	TN	 

#Sensitivity - Pr (positive test | desease) = TP/(TP+FN) #Probabiity of predicting You are sick, given that you really are sick
#Specificity - Pr (negative test | no desease) = TN /(TN+FP) #Probability of negative prediction, given that patiente is not sick 
#Positive Predictive Value - Pr(desease | positive test)  = TP/(TP+FP) #Looking at all the people that were diagnosed as desease, and trying to see those who were in fact actually desease;
#Negative Predictive Value - Pr(no disease | negative test) = TN/(FN+TN)
#Accuracy - Pr(correct outcome) = (TP+TN) / (TP+TN + FP+FN)

#Example:
Really Sick = 0.1% prevalence in population
Test kit with 99% sensitivity (ability to predict correcty what is intended to prove)
<=> 0.99 = TP/(TP+FN)
99% of specificity
<=> 0.99 = TN / (TN+FP)

What is Pr of a person having the desease, given the the test result is positive, if we randomly select a subject from:
  -  general population?
  TP/(TP+FP)
- a high risk sub-population with 10% desease prevalence?
  
  #For continuous data:
  Mean Squared Error (MSE): 1/n * SUM(Predictioni - Truthi)^2
Root Mean Squared Error (RMSE): ( 1/n*(SUM(Predictioni-Truthi)^2) )^0.5  #Most common measure used;
### IMportant Notes: 
# MSE is sensitive to outliers;
# Median Absolute deviation: for continuous data, often more robust
# Sensitivity: if you want few missed positives
# Specificity: If you want few negatives called positives
# Accuracy: weights false positives/negatives equally
# Concordance: example - kappa

#Receiver Operating Characteristic Curves (ROC)

ROC curves
x (1-Specificity) #In other words, probability of being FP - P(FP)
y Sensitivity #In other words, Pr of being true positive - P(TP)
AUC - Area Under the Curve
AUC = 0.5  random guessing
AUC = 1 perfect classifier
AUC > 0.8 is already considered good generally

#### Cross validation methodology (to avoid overfitting)
1. Use training set
2. split it into training/test sets
3. build a model on the training set
4. evaluate on the test set
5. Repeat and average the estimated errors
## Options for subsetting: 
# Purely random
# K-fold; 
#Note: larger K = less bias, more variance;
#Note: smaller K = more bias, less variance;

# Leave one out


