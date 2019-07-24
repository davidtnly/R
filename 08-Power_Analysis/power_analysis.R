##__________________________
## Power Analysis
##__________________________

## Power analysis is an important aspect in experimental design. 
## It allows us to determine the sample size required to detect an effect of a given size with a degree of confidence. 
##   It also allows us to determine the probability of detecting an affect of a given size with a given level of confidence, 
##   under sample size constraints.


## "Typically, before you do an experiment you should perform a power analysis to estimate the number of
##   observations you need to have a good chance of detecting the effect you are looking for."
##   The idea is to minimize cost and time for these tests.


## Parameters: sample size, effect size, significance level, power
# Effect size: size of the difference between your Ho and Ha that you hope to detect.
#   In statistical terminology, the minimum deviation from your null hypothesis that you hope to detect to reject.

# Power: probability of detecting a given effect size with a given sample size

# Significance Level: significance level which the test should be contacted at

# Beta, in a power analysis, is the probability of accepting the null hypothesis, even though it is false (a false negative), 
#   when the real difference is equal to the minimum effect size. 

## Samples are from the links below.
# http://www.biostathandbook.com/power.html
# Chapter 10: http://ceal.fing.uncu.edu.ar/industrial/TyHM/DOE/R_in_Action.pdf

# Install "pwr" package to use power analysis formulas
if(!require(pwr)) {install.packages("pwr")}

## Power analysis for a binomial test

p0 <- 0.75
p1 <- 0.78
h = ES.h(p0, p1) # Calculate effect size

pwr.p.test(
  h = h,
  n = NULL, # NULL tells the function to calculate this
  sig.level = 0.05,
  power = 0.90, #1 - beta (1-.1(Type 2 Error)) = 0.90 
  alternative = "two.sided"
)
## Answer
# proportion power calculation for binomial distribution (arcsine transformation) 
# 
# h = 0.07078702
# n = 2096.953
# sig.level = 0.05
# power = 0.9
# alternative = two.sided

## Power Analysis for unpaired t-test

m1 = 66.6 # mean for sample 1
m2 = 64.6 # mean for sample 2
s1 = 4.8  # sd for sample 1
s2 = 3.6  # sd for sample 2

# Add in cohen's d formula: An effect size used to indicate standardized between between two means
# This can be used in t-test/ANOVA results (appropriate effect size for the comparison of two means)
cohen.d <- (m1 - m2)/sqrt(((s1^2) + (s2^2))/2) 

pwr.t.test(
  n = NULL,
  d = cohen.d,
  sig.level = 0.05, # type 1 probability
  power = 0.80,     # 1 minus type 2 probability
  type = "two.sample",
  alternative = "two.sided"
)
## Answer
# Two-sample t test power calculation
# 
# n = 71.61288 (Approx. 72)
# d = 0.4714045
# sig.level = 0.05
# power = 0.8
# alternative = two.sided
# 
# NOTE: n is number in *each* group

## Calculate sample size for ANOVA (comparison of more than 2 groups) to get power = 0.80

pwr.anova.test(
  n = NULL,
  k = 5,
  f = 0.25, # effect size is f in ANOVA
  sig.level = 0.05,
  power = 0.80
)
## Answer
# Balanced one-way analysis of variance power calculation 
# 
# k = 5
# n = 39.1534 (Approx. 40)
# f = 0.25
# sig.level = 0.05
# power = 0.8
# 
# NOTE: n is number in each group

# Generate distributions
par(mfrow = c(1,2))

x <- rnorm(100000, mean = 10, sd = 2)
hist(x, breaks = 1200, xlim = c(0,20), freq = FALSE)
abline(v = 10, lwd = 5)
abline(v = c(4,6,8,12,14,16), lwd = 3, lty = 3)

# Display the Student's t distributions with various
# degrees of freedom and compare to the normal distribution
x <- seq(-4, 4, length = 100)
hx <- dnorm(x)
degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df = 1", "df = 3", "df = 8", "df = 30", "normal")

plot(x, hx, type = "l", lty = 2, xlab = "x value",
     ylab = "Density", main = "Comparison of t Distributions")

### Question: Detect if there is a difference between group 1 vs. group 2.
#     How many students are needed to sample to have an 80% chance of a difference this large to be significant.

mm1  = 66.6  # Mean for sample 1
mm2  = 64.6  # Mean for sample 2
sd1  = 4.80  # Std dev for sample 1
sd2  = 3.60  # Std dev for sample 2

Cohen.d = (mm1 - mm2)/sqrt(((sd1^2) + (sd2^2))/2)  

pwr.t.test(
  n = NULL,
  d = Cohen.d,
  # sig.level = 0.05,
  sig.level = 0.025, # 99% CI vs. 95% CI
  # power = 0.80,
  power = 0.99, # Changed to test difference between 99% sig diff vs. 80%
  type = "two.sample",
  alternative = "two.sided"
)
## Answer
##  The result is 72, meaning that if group 2 were really 2 inches shorter than group 1 students, 
##    you'd need at least 72 students in each class to detect a significant difference 80% of the time,
##    if the true difference really is 2.0 inches.
##  Tested the results from above vs. 99% significant difference and 99% confidence. We need a difference between 72 vs. 190 students.

# Two-sample t test power calculation 
# 
# n = 71.61288
# d = 0.4714045
# sig.level = 0.05
# power = 0.8
# alternative = two.sided
# 
# NOTE: n is number in *each* group

# Test for 90% power of test (Need at least 96 students from each group to detect a significant difference
#   90% of the time.)

pwr.t.test(
  n = NULL,
  d = Cohen.d,
  sig.level = 0.05,
  power = 0.90,
  type = "two.sample",
  alternative = "two.sided"
)
# Two-sample t test power calculation 
# 
# n = 95.53742
# d = 0.4714045
# sig.level = 0.05
# power = 0.9
# alternative = two.sided
# 
# NOTE: n is number in *each* group

###  8/26/18

# Before you do an experiment, you should perform a power analysis (test types) to estimate the number
#   of obs (n) you need to have a good chance of detecting the effect (power) you're looking for
#   with a degree of confidence (sig.level).

## R in Action: Chapter 10
# Topics: Determine sample size requirements, calculating effect sizes, assessing statistical power
# Types of tests: test of propportions, t-tests, chi-square, ANOVA, linear models, and tests of correlation

## T-test and parameters

# t.test - pwt.t.test()
# pwr.t.test(
#   n,
#   d,
#   sig.level,
#   power,
#   alternative,
#   type
# )

# First example page 251
# conduct study with effect size = 0.8, ci = 95%, 90% sure, two sided
pwr.t.test(
  n = NULL,
  d = 0.80,
  power = 0.90,
  sig.level = 0.05,
  type = "two.sample",
  alternative = "two.sided"
  )


## ANOVA page 252
# n = 40 for one group so 40x5 = 200 estimated on the mean
pwr.anova.test(
  n = NULL,
  k = 5, # number of groups
  power = 0.80,
  f = 0.25,
  sig.level = 0.05
)

# This example requires you to estimate what the means of the five groups will be, along with the common variance
#   When you have no idea what to expect, the approaches in 10.2.7 may help.

## Correlation

# power analysis for tests of correlation coefficients
# sample has null < and alternative > and not =
pwr.r.test(
  n = NULL,
  r = 0.25,
  sig.level = 0.05,
  power = 0.90, # want to be 90% confidence that you'll reject null if it's false
  alternative = "greater" # due to alternative > 0.25
)

# Answer: n = 134, we need 134 participants in order to be 90% confident
#   that you'll reject the null hypothesis if it's false

## Tests of proportions
# power analysis to compare two proportions
# h = effect size = 2arcsin(sqrt(p1) - 2arcsin(sqrt(p2)))
#   in R: ES.h(p1, p2)

p1 = 0.65
p2 = 0.60

# suspect that a popular medication relieves symptoms in 60% of users
# a new medication will be marketed if it improves symptoms in 65%
# how many participants will you need to include in a study comparing these two
#   medications if you want to detect a difference this large?

# assume you want to be 90% confident in a conclusion and 95% confident that you won't
#   reach this conclusion erroneously
#*** Use one-tailed test because you're only interested in assessing whether the new
#       drug is better than the standard.

pwr.2p.test(
  n = NULL,
  h = ES.h(p1, p2),
  sig.level = 0.05,
  power = 0.90,
  alternative = "greater"
)

# based on these results, you'll need to conduct a study with 1605 individuals receiving
#   the new drug and 1605 receiving existing drugs to meet the criteria.

## Chi-square
# tests are often used to assess the relationship between two categorical variables
# null hypothesis is typically that the variables are independent vs. a research hypothesis
#   that they aren't. 

# ex: here, p, is a hypothesized two-way probability table
# anticipate 70% of your sample will be caucasion... etc and belive that 60%
#    of caucasians tend to be promoted comapared to african americans at 30%


## 10.2.7 choosing an appropriate effect size in novel situations
# expected effect size is the most difficult parameter to determine

# the expected sample size is the most difficult parameter to determine
# requires subject matter expertise and measures employed

## look up cohen's effect size benchmarks
## effect size parameter: d, f, f2, h, w
## t-test, anova, linear model, test of proportion, chi-square

pwr.anova.test(
  n = 125, # total obs
  k = 5, # group
  sig.level = 0.05, # 95% confident not due to random sampling
  power = NULL, #confidence in detecting effect size
  f = 0.10 # effect size (general) look at table
)

# sample size curve for detecting correlations of various sizes
r <- seq(.1,.5,.01) # generate range of effect sizes r 
nr <- length(r)
p <- seq(.4,.9,.1)                                       
np <- length(p) # general range of power level r                                             

# use two for loops to cycle through p and r

samsize <- array(numeric(nr*np), dim=c(nr,np)) 
for (i in 1:np){
  for (j in 1:nr){
    result <- pwr.r.test(n = NULL, r = r[j],
                         sig.level = .05, power = p[i],
                         alternative = "two.sided")
    samsize[j,i] <- ceiling(result$n)
  }
}

xrange <- range(r) 
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type="n",
     xlab="Correlation Coefficient (r)",
     ylab="Sample Size (n)" )

par(mfrow = c(1,1))
for (i in 1:np){
  lines(r, samsize[,i], type="l", lwd=2, col=colors[i])
}
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89") 
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
       col="gray89")
title("Sample Size Estimation for Correlation Studies\n
      Sig=0.05 (Two-tailed)")
legend("topright", title="Power", as.character(p),
       fill=colors)

### Chapter 11: Charts
library(mtcars)
library(ggplot2)
ggplot(mtcars, aes(x = wt, y = mpg, color = cyl)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, level = 0.99)

pairs(mtcars[,1:5])
cor(mtcars[,1:5])

df <- as.data.frame(cor(mtcars))

### Bootstrapping


library(boot)

pmean <- 0                     # pop mean
psd <- 1                       # pop SD

plotfactor <- 1000             # scaling factor for h-axis
ybarPlotMin <- plotfactor * -6 # sample mean: min for plot
ybarPlotMax <- plotfactor *  6 # sample mean: max for plot

alpha <- 0.10                  # significance level - prob of rejecting the null
conf_level <- 1 - alpha        # confidence level  - if the same pop is sampled on numerous occasions the true pop parameter will be approximately within 90% 

nboot <- 10000                 # number of bootstrap reps
mean.boot <- function(x,i) mean(x[i]) # necessary for bootstrap

# The Bootstrap :: Table 1
Sample_1_Data  <- c ( 0.422, 1.103, 1.006, 1.034, 0.285, -0.647, 1.235, 0.912 ,1.825 )
Sample_1_Data

obs <- length(Sample_1_Data) # number of obs in the sample
obs
# Generate bootstrap samples
bootstrap_data <- matrix(nrow = obs, ncol = 2)

set.seed(1000)
for (i in 1:3) {
  data <- trunc(runif(obs, min = 1, max = obs + 1))
  for (j in 1:obs) {
    bootstrap_data[j,1] <- data[j]
    bootstrap_data[j,2] <- Sample_1_Data[data[j]]
  }
  print(bootstrap_data[,2])
  sample_mean <- round(mean(bootstrap_data[,2]),3)
  print(sample_mean)
}

sample_1_bootstrap <- boot(Sample_1_Data, mean.boot, nboot)

# Caculate percentiles of bootstrap distribution
l99 <- round(quantile(sample_1_bootstrap$t, probs = c(0.005)),3)
l95 <- round(quantile(sample_1_bootstrap$t, probs = c(0.025)),3)
l90 <- round(quantile(sample_1_bootstrap$t, probs = c(0.05)), 3)
u90 <- round(quantile(sample_1_bootstrap$t, probs = c(0.95)), 3)
u95 <- round(quantile(sample_1_bootstrap$t, probs = c(0.975)),3)
u99 <- round(quantile(sample_1_bootstrap$t, probs = c(0.995)),3)

# Generate a histogram (no ggplot)
hist(sample_1_bootstrap$t,
     axes = FALSE,
     xlab = "Bootstrap sample mean",
     ylab = "",
     main = paste0("Distribution of ", nboot, " bootstrap means")
     )
axis(1) # show x-axis


print(round(mean(sample_1_bootstrap$t),3)) # print avg of bootstrap mean
print(round(sd(sample_1_bootstrap$t),3))   # print sd of bootstrap mean








