### Test

x <- list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d = rnorm(100, 5))

x[1]
x[2]

x[[1]][4]

x[[2]][[1]]

lapply(x, mean) # list
sapply(x, mean) # vector

### Start: Working with the Simulation of new data

# Assign store count and weeks
k_stores <- 20
k_weeks <- 104

# Create a data frame of missing alues to hold the data
store_df <- data.frame(matrix(NA, ncol = 10, nrow = k_stores*k_weeks))
names(store_df) <- c("storeNum", "Year", "Week", "p1sales", "p2sales", "p1price", "p2price", "p1prom", "p2prom", "country")

# Create vectors to represent the store number and country for each ob
store_num <- 101:(100+k_stores)
(store_cty <- c(rep("US", 3), rep("DE", 5), rep("GB", 3), rep("BR", 2), rep("JP", 4), rep("AU", 1), rep("CN", 2)))

# Replace columns with values using rep() to expand the vectors to match the number of stores & weeks
store_df$storeNum <- rep(store_num, each = k_weeks) # rep extends each value by n amount
store_df$country <- rep(store_cty, each = k_weeks)

rm(store_cty, store_num)

# Add in Week & Year
rep(1:3, times = 2, each = 2) # times = copies entire vector and appends; each = duplicates value

(store_df$Week <- rep(1:52, times = k_stores*2))
rep(1:2, each = k_weeks/2) # 104/2 = 52; each will duplicate 52 values of 1 then 2
rep(1:2, times = k_stores) # 20; times will copy and append 1:2 20 times
(store_df$Year <- rep(1:2, each = k_weeks/2, times = k_stores))

# Factorize variables
store_df$storeNum <- factor(store_df$storeNum)
store_df$country  <- factor(store_df$country)
head(store_df)

## Simulating Data Points using rbinom(n, size (number of trials), p) - drawing from a binomial distribution
# Also means that we are arbitraily assigning a 15% likelihood of promotion for product 2 and then randomly determine which weeks have it
# The size is the total number of trials, of which size*prob are expected to be successes.
rbinom(100, 10, 0.2) # 100 runs of 10 coin flips each, returning the number of successes in each run
set.seed(98250)
store_df$p1prom <- rbinom(n = nrow(store_df), size = 1, p = 0.10) # 10% promoted (10% probability for product 1)
store_df$p2prom <- rbinom(n = nrow(store_df), size = 1, p = 0.15) # 15% promoted 
head(store_df)

# Price - ranging from $2.19 to $3.19 overall
# Randomly draw a price for each week by defining a vector with data points and sample(x, size, replace)
store_df$p1price <- sample(x = c(2.19, 2.29, 2.49, 2.79, 2.99), size = nrow(store_df), replace = TRUE)
store_df$p2price <- sample(x = c(2.29, 2.49, 2.59, 2.99, 3.19), size = nrow(store_df), replace = TRUE)

# Item sales are in unit counts so we use the Poisson distribution
# Poisson is discrete where i >= 0
# rpois(n, lambda), where n is the number of draws and lambda is the mean value of units per week
tmp_sales1 <- rpois(nrow(store_df), lambda = 120)
tmp_sales2 <- rpois(nrow(store_df), lambda = 100)

# Scale using log(price)
tmp_sales1 <- tmp_sales1 * log(store_df$p2price) / log(store_df$p1price)
tmp_sales2 <- tmp_sales2 * log(store_df$p1price) / log(store_df$p2price)

# Final sales get a 30% or 40% lift when promoted (Assumption)
store_df$p1sales <- floor(tmp_sales1 * (1 + store_df$p1prom * 0.3))
store_df$p2sales <- floor(tmp_sales2 * (1 + store_df$p2prom * 0.4))
head(store_df)

# Inspect data
car::some(store_df, 10)

### End: Created a simulated data set with 20,800 values with a total of 22 commands

### Quick Statistics: Notes
# A covariance refers to the measure of how two random variables will change together and is used to calculate the correlation between variables
# The variance refers to the spread of the data set - how far apart the numbers are in relation to the mean, for instance.
# Correlation standardizes the measure of interdependence between two variables and informs us as to how closely the two variables move together
# Correlation coefficient r measures the linear association between two variables

# Working with feature associations
# Using cor() to find r (correlation coefficient) ~ 0.25 slight correlation
# Then use correlation test as well with cor.test(y, x) ~ checking p-value to look for statistical significance

# Correlation plot is another graph to show correlation between numerous numeric variables
library(corrplot)
library(gplots)
corrplot.mixed(corr = cor(cust_df[ , c(2, 3, 5:12)], use = "complete.obs"),
               upper = "ellipse",
               tl.pos = "lt",
               col = colorpanel(50, "red", "gray60", "blue4"))

### Segment Data - read csv file and update data definitions
seg_df <- read.csv("http://goo.gl/qw303p")

segVars <- c("age", "gender", "income", "kids", "ownHome", "subscribe")
segVarType <- c("norm", "binom", "norm", "pois", "binom", "binom")
segNames <- c("Suburb mix", "Urban hip", "Travelers", "Moving up")
segSize <- c(100, 50, 80, 70)
segMeans <- matrix( c(40, 0.5, 55000, 2, 0.5, 0.1,
                      24, 0.7, 21000, 1, 0.2, 0.2,
                      58, 0.5, 64000, 0, 0.7, 0.05,
                      36, 0.3, 52000, 2, 0.3, 0.2),
                    ncol = length(segVars), byrow = TRUE)

# standard deviations for each segment (NA = not applicable for the variable)
segSDs <- matrix(c(5, NA, 12000, NA, NA, NA,
                   2, NA, 5000, NA, NA, NA,
                   8, NA, 21000, NA, NA, NA,
                   4, NA, 10000, NA, NA, NA),
                   ncol = length(segVars), byrow = TRUE)

# Create sequence
(i_seq <- rep(sqrt(seq(from = 2.1, to = 6.2, by = 1.7)), times = 3))

# For loop sample (Bad)
for (i in 1:length(i_seq)) { cat("Entry", i, "=", i_seq[i], "\n") }

# For loop sample (Good) - seqalong() protects against common errors when the index vector has zero length or is inadvertently reversed
for (i in seq_along(i_seq)) { cat("Entry", i, "=", i_seq[i], "\n") }

## Final Segment Data Generation
# Pseudocode is a good way to outline and debug code conceptually before you actually write it
set.seed(100)

# Iterate over segments and create data for each
for (i in seq_along(segNames)) {
  # Print segment names
  cat(i, segNames[i], "\n")
  
  # Create empty matrix to hold data
  this_seg <- data.frame(matrix(NA, nrow = segSize[i], ncol = length(segVars)))
  
  # Within segment, iterate over variables and draw random data
  for (j in seq_along(segVars)) {
    if (segVarType[j] == "norm") {
      this_seg[, j] <- rnorm(segSize[i], mean = segMeans[i, j]) # Draw random normals
    } else if (segVarType[j] == "pois") { 
      this_seg[, j] <- rpois(segSize[i], lambda = segMeans[i, j])
    } else if (segVarType[j] == "binom") {
      this_seg[, j] <- rbinom(segSize[i], size = 1, prob = segMeans[i, j])
    } else {
        stop("Bad segment data type: ", segVarType[j])
    }
  }
  
  # Add this segment to the total dataset
  seg_df <- rbind(seg_df, this_seg) # 6vs7 columns - missing a col
  
}

# Quick way to aggregate column data
# aggregate() allows us to compute functions of continuous variables
aggregate(seg_df$income, list(seg_df$Segment), mean) # aggregate(x, by, fx)
 
## Descriptive for Two-Way Groups
#A common task in marketing is cross-tabulating, separating customers into groupsaccording to two (or more) factors
aggregate(income ~ Segment + ownHome, data = seg_df, mean) 

# We now have a separate group for each combination of Segment and ownHome and can begin to see how income is related

# Looking for frequency with different combinations
with(seg_df, table(Segment, ownHome))

# Breakdown by kids now
with(seg_def, table(kids, Segment))

# Proportions
prop.table(table(seg_df$subscribe, seg_df$Segment), margin = 2)

### End: Chapter Key Points

# When writing for()loops, use seqalong() instead of 1:length()
# When creating a data object from scratch, pre-populate it with missing data (NA) and then fill it in for speed and reliability

## Describing and visualizing data for groups
# The by() function can split up data and automatically apply functions such as mean()
# aggregate() can understand formula models and produce reusable, indexable object with its results
# Frequency of occurrence can be found with table()

### Chapter 6: Comparing Groups: Statistical Tests

# Answering the question: It looks different but is it really different?
# Using inferential statistical procedures: chi-sq, t-test, ANOVA, Bayesian

# Get data
seg.df <- read.csv("http://goo.gl/qw303p")
summary(seg.df)

### Testing group frequencies; use statistical tests to determine whether differences are real or might be due to random variation (noise)
## Summarizing differences between groups < Statistical Tests

## Chi-sq - used with frequency counts produced by a table; determines whether the frequencies in cells are significantly different
# Basically that means we need categorical variables ( Chisq = sum(O-E)^2/E )
# H null: Variable A and Variable B are independent
# H alt: Variable A and Variable B are not independent

# Example 1 : SRS of 1000 voters split between gender and rep/dem/independent and results shown in a table
# Is there a gender gap? Do the men's voting pref differ significantly from the women's pref? Use 0.05 significance

# State Hypotheses
# H null: Gender and voting preferences are independent
# H alt: Gender and voting preferences are not independent

# Formulate an analysis plan
# Significance is 0.05, we weill conduct a chi-square test for independence
# Analyze the sample data; apply the test by computing deg of freedom, freq counts, and the test statistic to determine the p-value

# Create a table where the data comprises 95 observations ofthe numbers 1-4 and where the counts of each are almost, but not quite identical
tmp.tab <- table(rep(c(1:4), times = c(25,25,25,20)))
tmp.tab
chisq.test(tmp.tab)

tmp.tab <- table(rep(c(1:4), times = c(25,25,25,10))) # unlikely to be equal in the larger population
tmp.tab
chisq.test(tmp.tab)

chisq.test(table(seg.df$Segment))

table(seg.df$subscribe, seg.df$ownHome)
chisq.test(table(seg.df$subscribe, seg.df$ownHome))

# Check for outliers
hist(seg.df$income)
with(seg.df, hist(income[ownHome == "ownYes"]))
with(seg.df, hist(income[ownHome == "ownNo"]))

# Test after removing any outliers
t.test(income ~ ownHome, data = seg.df)

# ANOVA
seg.aov.own <- aov(income ~ ownHome, data = seg.df) # aov() performs 1 way ANOVA
anova(seg.aov.own) # anova() in the car package may be used to get the two way ANOVA table.

seg.aov.seg <- aov(income ~ Segment, data = seg.df)
anova(seg.aov.seg)

anova(aov(income ~ Segment + ownHome, data = seg.df))

# Simulate Amusement Daa
set.seed(100)
nresp <-  600 # Number of survey respondents

# Simulate satisfaction halo with a random variable for each customer
halo <- rnorm(n = nresp, mean = 0, sd = 5)

# Generate response for the satisfaction rating
# Add a constant just to adjust the range slightly and convert to continuous values to integers using floor()
rides <- floor(halo + rnorm(n = nresp, mean = 80, sd = 3) + 1)
games <- floor(halo + rnorm(n = nresp, mean = 70, sd = 7) + 5)
wait <- floor(halo + rnorm(n = nresp, mean = 65, sd = 10) + 9)
clean <- floor(halo + rnorm(n = nresp, mean = 85, sd = 2) + 1)

# By adding halo value to each response, we create a positive correlation between the responses
# Verify correlation between variables that share the halo
cor(rides,games)

# Generate more data using rlnorm(n, meanlog, sdlog) to sample a lognormal distribution for distance and 
# sample(x, size, reaplce) to sample discrete distributions for weekend and number of children (num.child)
distance <- rlnorm(n = nresp, meanlog = 3, sdlog = 1)
num.child <- sample(x = 0:5, size = nresp, replace = TRUE,
                    prob = c(0.3, 0.15, 0.25, 0.15, 0.10, 0.05))

table(data.frame(num.child)) # 2 should populate the most due to prob parameter

weekend <- as.factor(sample(x = c("yes", "no"), size = nresp, replace = TRUE, prob = c(0.5, 0.5)))

# Create overall satisfaction rating as a function of ratings
overall <- floor(halo + 0.5 * rides +
                   0.1 * games +
                   0.3 * wait +
                   0.2 * clean +
                   0.03 * distance +
                   5 * (num.child == 0) +
                   0.3 * wait * (num.child > 0) +
                   rnorm(n = nresp, mean = 0, sd = 7) - 51)

# Combine data points
sat.df <- data.frame(weekend, num.child, distance, rides, games, wait, clean, overall)
rm(nresp, weekend, distance, num.child, halo, rides, games, wait, clean, overall)

# Inspect Data
ggpairs(sat.df) # Visual reveals that distrance variable has a highly skewed distribution (left tailed)

# Distance has a highly skewed distribution; transform variable to a more normal distribution such as log()
sat.df$logdist <- log(sat.df$distance)
ggpairs(sat.df)

# Create a corrplot - remove any if r > 0.8
corrplot::corrplot.mixed(cor(sat.df[ , c(2, 4:9)]), upper = "ellipse")

# Goal of a satisfaction drivers analysis is to discover relationships between satisfaction with features of the service ( or product )
# To what extent is satisfaction with the park's rides related to overall experience? Weak or strong?
plot(overall ~ rides, data = sat.df, 
     xlab = "Satisfaction with Rides", ylab = "Overall Satisfaction")

# Results show on the plot that there is a tendency for people with higher satisfaction with rides to also have higher overall satisfaction

# Linear regression with a single predictor
lm(overall ~ rides, data = sat.df)

# Call:
#   lm(formula = overall ~ rides, data = sat.df)

# Coefficients:
#   (Intercept)  rides  
# -95.734        1.859  

# Formula above can be read as "overall varies with rides" - model
# Model interp - we would expect that a customer who gives a rating of 95 for satisfaction with rides would give an overall rating of:
rating <- -95.734 + 1.859*95 # 80.817 (overall rating)
rating

# When you wish to compare coefficients, it can be helpful to standardize data on acommon scale before fitting a model 
# (and after transforming any variables to a more normal scale). 
# The most common standardization converts values to zero-centered units of standard deviation. 
# This subtracts a variable's mean from each observationand then divides by the standard deviation (sd()).

(sat.df$rides - mean(sat.df$rides)) / sd(sat.df$rides)

# R includes the scale() function to perform it:
scale(sat.df$rides)

# Create scaled version of sat.df
sat.std <- sat.df[ , -3] # sat but remove distance
sat.std[ , 3:8] <- scale(sat.std[ , 3:8])
head(sat.std)

# The question of standardizing values depends primarily on how you want to use a model's coefficients. 
# If you want to interpret coefficients in terms of the originalscales, then you would not standardize data first. 
# However, in driver analysis weare usually more concerned with the relative contribution of different predictors and wish 
#  to compare them, and standardization assists with this. 
# Additionally, we often transform variables before analysis such that they are no longer on the originalscale.

summary(sat.std) # should have a mean of 0

# Note that the column names from summary() have an extra
#  .V1 in the output; this indicates that the column has a more complex data type than a simple vector

# Using factors as predictors
m3 <- lm(overall ~ rides + games + wait + clean + weekend + logdist + num.child, data = sat.std)
summary(m3)

# Converting num.child to a factor and re-estimating the model because num 1-5 doesnt indicate 5 is better
sat.std$num.child.factor <- factor(sat.std$num.child)

m4 <- lm(overall ~ rides + games + wait + clean + weekend + logdist + num.child.factor, data = sat.std)
summary(m4)

# We interpret each coefficient as the difference between that level of the factor and the baseline level. 
# So, parties with 1 child rate their overall satisfaction on average 1.016 standard deviations higher than parties without children.

### Afterthoughts - Marketing Intuition and Experience
# One might further tune the model by considering whether logdist is still needed;
# we'll leave that to the reader and assume that mode lm7 is the final model.
# What do we do with these results as marketers? We identify several possible marketing interventions. 
# If we want to increase satisfaction overall, we could perhaps do so by trying to increase the number of visitors with children. 
# Alternatively, if we want to appeal to visitors without children, we might engage in further research to understand why their ratings are lower. 
# If we are allocating budget to personnel,the importance of cleanliness suggests continuing to allocate resources there (as opposed, say, to games). 
# We might also want to learn more about the association 
# between children and waiting time, and whether there are things we could do to make waiting less frequent or more enjoyable.

library(coefplot)
coefplot(m4, intercept = FALSE, outerCI = 1.96, lwdOuter = 1.5,
         ylab = "Rating of Feature",
         xlab = "Association with Overall Satisfaction")

# Figure summarizing the relative contribution of each element on overall satisfaction

### Notes: When including interaction terms in a model, there are two important points. First, it is especially 
#           important to consider standardizing the predictors when modeling interactions in order to have an interpretable and comparable scale 
#           for coefficients. Second, one should always include main effects (such as x+y) when including an interaction effect (x:y). 
#           If you don't estimate the main effects, you won't know whether a supported interaction is in fact due to an interaction, or 
#           is instead due to one of the individual variables' unestimated main effects.


### Overfitting
# This process of adding too many variables and ending up with a less precise or in appropriate model is called overfitting. 
# One way to avoid it is to keep a close eyeon the standard errors for the coefficients; small standard errors are an indicator that there is 
# sufficient data to estimate the model. Another approach is to select a subset of the data to hold out and not use to estimate the model. 


### Reducing Data Complexity

brand.ratings <- read.csv("http://goo.gl/IQl8nc")
head(brand.ratings)
tail(brand.ratings)

# Questions: 1. How trendy is Intelligentsia Coffee?
# Questions: 2. How much of a category leader is Blue Bottle Coffee?

# Data structure
summary(brand.ratings)
str(brand.ratings)

# Scale features
brand.sc <- brand.ratings
brand.sc[, 1:9] <- scale(brand.ratings[, 1:9])
summary(brand.sc)

# Correlation
library(corrplot)
corrplot(cor(brand.sc[, 1:9]), order = "hclust")

# Aggregate mean ratings by brand
brand.mean <- aggregate(. ~ brand, data = brand.sc, mean)
brand.mean

# Rename brand names
rownames(brand.mean) <- brand.mean[, 1] # use brand for the row names
brand.mean <- brand.mean[, -1] # remove brand name column
brand.mean

# Heatmap to examine results
library(gplots)
library(RColorBrewer)

heatmap.2(as.matrix(brand.mean), 
          col = brewer.pal(9, "GnBu"), 
          trace = "none", 
          key = FALSE, 
          dend = "none", 
          main = "\n\n\n\n\nBrand attributes")

### 8.2 Principal Component Analysis and Perceptual Maps
# Example: Explore PCA first with a simple data set to see and develop intuition about what is happening
# Create highly correlated data by copying a random vector xvar to a new vector yvar while replacing half of the data points

set.seed(98286)
xvar <- sample(1:10, 100, replace = TRUE)
yvar <- xvar
yvar[sample(1:length(yvar), 50)] <- sample(1:10, 50, replace = TRUE)
zvar <- yvar
zvar[sample(1:length(zvar), 50)] <- sample(1:10, 50, replace = TRUE)
my.vars <- cbind(xvar, yvar, zvar)

# Check one of the three possible bivariate plots along with the corr matrix
plot(yvar ~ xvar, data = jitter(my.vars)) # bivariate plot shows a clear linear trend for yvar vs xvar on the diagonal
cor(my.vars) # xvar correlates highly with yvar and less so with zvar

# First use prcomp() to perform PCA
my.pca <- prcomp(my.vars)
summary(my.pca)

# There are three components because we have three variables (look at variance):
# The first component accounts for 65% of the explainable linear variance
# The second accounts for 24%, leaving 11% for the third component.

# Importance of components:
#                          PC1    PC2    PC3
# Standard deviation     3.9992 2.4381 1.6269
# Proportion of Variance 0.6505 0.2418 0.1077
# Cumulative Proportion  0.6505 0.8923 1.0000

# Q: How are those components related to the variables? Check rotation matrix.

# Check extracted PCA components
cor(my.pca$x)

# Visualizing PCA: visualize the data in a lower-dimensional space
biplot(my.pca) # arrows that show the best fit of each of the variables on the principal components - projection of the vars onto the 2-d space of the first 2 PCA components

# Now PCA for Brand Ratings
brand.pc <- prcomp(brand.sc[, 1:9])
summary(brand.pc)

# Plot
plot(brand.pc, type = "l")

## Explanation: A scree plot is often interpreted as indicating where additional components are not worth the complexity;
##  this occurs where the line has an elbow, a kink in the angle of bending
## The elbow occurs at either component three or four, depending on interpretation; 
##  and this suggests that the first two or three components explain most of the variation in the observed brand ratings.

## Note: PCA solution shows the successive variance accounted by each component. 
## For the brand rating data, the proportion largely levels out after the third component.

# Biplot of the first two principal components - which selects by default for a PCA object
biplot(brand.pc)

## Note: Although we see adjective groupings on the variable loading arrows in red, and gain some insight into the areas where ratings cluster 
## (as dense areas of observation points), the chart would be more useful if the data were first aggregated by brand.

# Plot of individual ratings is too dense and does not tell us about the brand positions ^

# A better solution is to perform PCA using aggregated ratings by brand
brand.mean
brand.mu.pc <- prcomp(brand.mean, scale = TRUE) # added scale in order to rescale the data

summary(brand.mu.pc) # results show that the first two components account for 84% of the explainable variance inthe mean ratings; so we focus with them

# Importance of components:
#   PC1    PC2    PC3     PC4     PC5
# Standard deviation     2.1345 1.7349 0.7690 0.61498 0.50983
# Proportion of Variance 0.5062 0.3345 0.0657 0.04202 0.02888
# Cumulative Proportion  0.5062 0.8407 0.9064 0.94842 0.97730
# PC6     PC7     PC8     PC9
# Standard deviation     0.36662 0.21506 0.14588 0.04867
# Proportion of Variance 0.01493 0.00514 0.00236 0.00026
# Cumulative Proportion  0.99223 0.99737 0.99974 1.00000

# Perceptual map of the brands
biplot(brand.mu.pc, main = "Brand positioning", cex = c(1.5, 1))

# A biplot of the PCA solution for the mean ratings gives an interpretable
# perceptual map, showing where the brands are placed with respect to the first two principal components

# Interp: Before interpreting the new map, we first check that using mean data did not greatly alter the structure

## What does the map tell us? 
# First we interpret the adjective clusters and relationships 
# and see four areas with well differentiated sets of adjectives and brands that are positioned in proximity.

## What should you do about the position of your brand? 
# Again, it depends on the strategic goals. If you wish to increase differentiation, 
# one possibility would be to take action to shift your brand in some direction on the map. 
# Suppose you wanted to move in the direction of brand c. You could look at the specific differences from c in the data:
brand.mean["c", ] - brand.mean["e", ]

# ^ This shows you that e is relatively stronger than c on "value" and "fun", which
# suggests dialing down messaging or other attributes that reinforce those 
# (assuming,of course, that you truly want to move in the direction of c).
#  Similarly, c is stronger on "perform" and "serious," so those could be aspects of the product or message for e to strengthen.

# Find out how to position there. Assume that the gap reflects approx the avg of those four brands.
# Find the avg using colMeans() on the brands' rows, then take the diff of e from that avg.
colMeans(brand.mean[c("b", "c", "f", "g"), ]) - brand.mean["e", ]

# ^ This suggests that brand e could target the gap by increasing its emphasis on performance while reducing emphasis on "latest" and "fun."

## Summary
# To summarize, when you wish to compare several brands across many dimensions,it can be helpful to focus on just the first two or three 
# principal components that explain variation in the data. You can select how many components to focus on using a scree plot, which shows
# how much variation in the data is explained by each principal component

# PCA finds linear functions that explain maximal variance in observed data
# A common use for PCA is a biplot of aggregate scores for brands or people to visualize relationships

###  Check for highly correlate vars

library(gpairs)
cust.df <- read.csv("http://goo.gl/PmPkaG")
                    
gpairs(cust.df) # several vars have extreme skew and other pairs are near perfect corr

# Solution - first step: Transform using a box-box transformation
autoTransform <- function(x) {
  library(forecast)
  
  # Boxcox + standardize using scale()
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}

# Select complete cases only and remove ID col
cust.df.bc <- cust.df[complete.cases(cust.df), -1]
cust.df.bc <- subset(cust.df.bc, online.spend > 0)
numcols <- which(colnames(cust.df.bc) != "email")
cust.df.bc[ , numcols] <- lapply(cust.df.bc[ , numcols], autoTransform )

# Results of standardized normally distributed values
summary(cust.df.bc)
gpairs(cust.df.bc)

# Refit model using new df
spend.m2 <- lm(online.spend ~ ., data = cust.df.bc)
summary(spend.m2)

##  Coefficients are smaller now because the data have been standardized

# Using anova() to compare models
spend.m3 <- lm(online.spend ~ online.trans, data = cust.df.bc)
anova(spend.m3, spend.m2)

## Small difference between the model fits is reflected in the highp-value (p = 0.8129), 
## and thus the null hypothesis of no difference between the models cannot be rejected

## The problem here is collinearity: because visits and transactions are so highly related, and also because a linear model assumes that effects are additive
## This will cause the standard errors of the predictors to increase, which means that the coefficient estimates will be highly uncertain or unstable.

### Solution - Assessing by variance inflation factor (VIF)

















