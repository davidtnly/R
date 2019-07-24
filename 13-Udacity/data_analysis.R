## Lesson 1

# install and load ggplot
library(ggplot2)
library(RColorBrewer)

# load dataset
data(diamonds)

# create a scatterplot of price vs. carat color coded
qplot(data = diamonds, x = carat, y = price, color = cut) +
  scale_color_brewer(palette = "Accent")

# read and subset data
data <- read.csv('~/R Projects/MiscData/stateData.csv')
head(data)
dim(data)

# subset state.region = 1
subset(data, state.region == 1) 
data[data$state.region == 1,] # same thing

# load mtcars and use two conditions in row
data(mtcars)
str(mtcars)
mtcars[mtcars$mpg > 30 & mtcars$cyl > 1 ,]

# ordered for factor variables
reddit$age.range <- ordered(reddit$age.range, levels = c('',''))

# alternate solution
reddit$age.range <- factor(reddit$age.range, levels = c('',''), ordered = TRUE)

# data munging/wrangling
# https://www.udacity.com/course/data-wrangling-with-mongodb--ud032

# fields: tab-delimited vs. comma-delimited
# read.csv('filename.csv, stringAsFactors = FALSE, sep = ",")
# read.csv('filename.csv, stringAsFactors = FALSE, sep = "\t")

# data at a glance
head(mtcars) # top 5
dim(mtcars) # row and column counts
names(mtcars) # variable names
str(mtcars) # type
summary(mtcars) # quick summary of each column

## Lesson 3: Visualizing and Exploring

# open tab delimited file using sep = '\t'
# read.delim() defaults to the tab character
data <- read.csv('~/R Projects/MiscData/pseudo_facebook.tsv', sep = '\t')
dim(data)
str(data)
names(data)

# using ggthemes/ggplot2
library(ggplot2)
library(ggthemes)

# histogram of users' birthdays
qplot(x = dob_day, data = data) +
  scale_x_continuous(breaks = 1:31) + # days of the month
  # one for each month of the year (12)
  facet_wrap(~ dob_month, ncol = 3) #(~variable, ncol, nrow)
  # looking for anomalies and making analyses on it
  # outliers/anomalies may be correct, make sure it's good data

# create a friend count plot
ggplot(data = data[!is.na(data$gender),], aes(x = friend_count)) +
  geom_histogram(binwidth = 25) + # skewed (long tailed data) - right side
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000,50)) + # limit 0 - 1000
  facet_wrap(~gender, ncol = 1)

# group by count and gender with a summary function
by(data$friend_count, data$gender, summary)

# create another histogram
ggplot(data = data, aes(x = tenure/365)) +
  geom_histogram(binwidth = .25, color = 'black', fill = '#F79420') +
  scale_x_continuous(breaks = seq(1,7,1), limits = c(0,7)) +
  xlab('Number of years using Facebook') +
  ylab('Number of users in sample')

ggplot(data = data, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = '#5760AB') +
  scale_x_continuous(breaks = seq(0, 113, 5))

# use multiple plots at once
library(gridExtra)

# https://www.r-statistics.com/2013/05/log-transformations-for-skewed-and-wide-distributions-from-practical-data-science-with-r/

## logging data
# you ideally want to make sure that the relationship between 
#   input variables and output variables is approximately linear
# basically this means that you want to be able to use and compare your data
#   using the same scale

# transformation of data
qplot(data= data, x = friend_count)
qplot(data= data, x = log10(friend_count))
summary(data$friend_count)
summary(log10(data$friend_count)) # some friend counts are zero

# in calculus, undefined; limit = -inf
summary(log10(data$friend_count + 1)) # add 1 to remove -inf

# sqrt transformation
summary(sqrt(data$friend_count)) # sqrt transformation

# use ggplot
ggplot(data = data, aes(x = friend_count)) +
  geom_histogram() +
  scale_x_log10()

# frequency polygons
ggplot(data = data[!is.na(data$gender),], aes(x = friend_count, y = ..count../sum(..count..))) +
  geom_freqpoly(aes(color = gender), binwidth = 10)  

ggplot(data = data[!is.na(data$gender),], aes(x = www_likes)) +
  geom_freqpoly(aes(color = gender)) +
  # scale_x_continuous()
  scale_x_log10() # better than continuous to see difference

ggplot(data = data[!is.na(data$gender),], aes(x = friend_count, y = ..count../sum(..count..))) +
  geom_freqpoly(aes(color = gender), binwidth = 10) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 100)) +
  xlab("Friend Count") +
  ylab ("Proportion of users with that friend count")
  # to plot proportions within each group use ..density.. instead of sum(..count..)

# check for summary without graph
# which gender has more likes?
#   this could help give an idea on what feature to use at a glance
by(data$www_likes, data$gender, sum)

# box plots
qplot(data = data[!is.na(data$gender),],
      x = gender, y = friend_count,
      geom = "boxplot")

# plot with limits
qplot(data = data[!is.na(data$gender),],
      x = gender, y = friend_count,
      geom = "boxplot") +
  coord_cartesian(ylim = c(0,300)) # used for plotting limits

# find summary
by(data$friend_count, data$gender, summary)

# look at mobile likes
summary(data$mobile_likes)
summary(data$mobile_likes > 0)

#################################################################################

## Section 4
library(ggplot2)
data(diamonds) # load diamond dataset

summary(diamonds)
names(diamonds)

ggplot(data = diamonds, aes(x = cut, y = price)) +
  geom_boxplot() +
  scale_x_log10() +
  facet_wrap( ~ cut) 

# reorder by counts (desc)
ggplot(data = diamonds, aes(reorder(x = cut, cut, fun = count))) +
  geom_histogram(stat = "count") 

# create a histogram of the price of all diamonds
ggplot(data = diamonds) +
  geom_histogram(aes(x = price), binwidth = 100
                 ,color = "black", fill = "lightblue") +
  ggtitle("Diamonds Price Histogram")
 
# describe shape and center of distribution
summary(diamonds$price) # right skewed (max)

# how many diamonds cost less than 500, 250, or 15000+
nrow(diamonds[diamonds$price < 500,])
nrow(subset(diamonds, price < 500))

nrow(diamonds[diamonds$price < 250,])
nrow(subset(diamonds, price < 250))

nrow(diamonds[diamonds$price >= 15000,])
nrow(subset(diamonds, price >= 15000))


# peak
ggplot(diamonds) + geom_histogram(aes(x=price), binwidth = 10, 
                                  color = "black", fill = "lightblue") + 
  ggtitle("Diamonds Price Histogram between $0 and $1500.") + 
  coord_cartesian(xlim=c(0,1500))

# cut by histogram
ggplot(diamonds) + 
  geom_histogram(aes(x=price), binwidth = 100, 
                 color = "black", fill = "lightblue") + 
  ggtitle("Diamonds Price by Cut Histogram") + 
  facet_grid(. ~ cut)

# highest price diamond
diamonds[which.max(diamonds$price),]

# summary by
by(diamonds$price, diamonds$cut, summary)

# scales and multiple histograms
ggplot(diamonds) +
  geom_histogram(aes(x = price), binwidth = 100,
                 color = "black", fill = "lightblue") +
  facet_grid(.~cut)

ggplot(diamonds) +
  geom_histogram(aes(x = price), binwidth = 100,
                 color = "black", fill = "lightblue") +
  facet_wrap(.~cut)

ggplot(diamonds) + 
  geom_histogram(aes(x=price)) + 
  ggtitle("Diamonds Price by Cut Histogram") + 
  facet_wrap(~ cut,scales = "free_y",ncol = 5)

qplot(data = diamonds, x = price) +
  facet_wrap(. ~ cut, scales = "free")

# price per carat by cut
# create a histogram of price per carat and facet it by carat
# ggplot(diamonds) +
#   geom_histogram(aes(x = price/carat), binwidth = 0.05,
#                  color = "black", fill = "lightblue") +
#   ggtitle("Histogram of Price per Carat, facet by Cut.") + 
#   scale_x_log10() +
#   facet_grid(. ~ cut)

# price box plots
# use box plots to look at price of diamonds
ggplot(data = diamonds) +
  geom_boxplot(aes(x = color, y = price, fill = color)) +
  coord_cartesian(ylim = c(0, 8000)) +
  scale_y_continuous(breaks = seq(0, 8000, 500)) + 
  scale_y_continuous(breaks = seq(0,8000,500)) +
  ggtitle("Diamonds Price by Color.")

# use limits in y-axis (limits entire population vs. coord_cartesian)
ggplot(data = diamonds) +
  geom_boxplot(aes(x = color, y = price, fill = color)) +
  # coord_cartesian(ylim = c(0, 8000)) +
  scale_y_continuous(limits = c(0,8000), breaks = seq(0, 8000,500)) + 
  ggtitle("Diamonds Price by Color.")

# same graph but used parameters in ggplot instead of boxplot
ggplot(data = diamonds, aes(x = color, y = price, fill = color)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 8000, 500)) +
  coord_cartesian(ylim = c(0,8000))

## using IQR
# What is the price range for the middle 50% of diamonds with color D?
by(diamonds$price, diamonds$color, summary)

# What is the IQR for diamonds with the best color?
by(diamonds$price, diamonds$color, IQR)

# price per carat box plots by color
# investigate the price per carat of diamonds across colors
ggplot(diamonds) + 
  geom_boxplot(aes(x = color, y = price/carat, fill = color)) +
  scale_y_continuous(breaks = seq(0, 8000, 500)) + # limits data
  coord_cartesian(ylim = c(0,8000)) +  # limits actual scale
  ylab("Price per Carat") +
  ggtitle("Price per Carat by Color")

ggplot(diamonds) + 
  geom_freqpoly(aes(x = carat), binwidth = 0.05) + 
  scale_x_continuous(breaks = seq(0,5,0.1)) +  
  geom_hline(yintercept = 2000, color = "red") + # horizontal line
  ggtitle("Carat Frequency Polygon")

## birthday histograms
# task: investigate the distribution of your friend's bdays

## Potential Questions
# How many people share your birthday? Do you know them?
# Which month contains the most number of birthdays?
# How many birthdays are in each month?
# Which day of the year has the most number of birthdays?
# Do you have at least 365 friends that have birthdays on everyday of the year?
  
# You will need to do some data munging and additional research to
# complete this task. This task won't be easy, and you may encounter some
# unexpected challenges along the way. We hope you learn a lot from it though.

# You can expect to spend 30 min or more on this task depending if you
# use the provided data or obtain your personal data. We also encourage you
# to use the lubridate package for working with dates. Read over the documentation
# in RStudio and search for examples online if you need help.

# You'll need to export your Facebooks friends' birthdays to a csv file.
# You may need to create a calendar of your Facebook friends’ birthdays
# in a program like Outlook or Gmail and then export the calendar as a
# csv file.

# Once you load the data into R Studio, you can use the strptime() function
# to extract the birth months and birth days. We recommend looking up the
# documentation for the function and finding examples online.

# We've included some links in the Instructor Notes to help get you started.

#################################################################################

## Lesson 5: Explore Two Variables

# load friend data
data <- read.delim('~/R Projects/MiscData/pseudo_facebook.tsv')

qplot(data = data, x = age, y = friend_count)

# younger users have more friends
ggplot(data = data, aes(x = age, y = friend_count)) +
  geom_point() +
  xlim(13, 90) # add x limits

# graphs are overplotted, set transparency
ggplot(data = data, aes(x = age, y = friend_count)) +
  geom_point(alpha = 0.5) +
  xlim(13, 90)

# use geom_jitter because of of clusters together
ggplot(data = data, aes(x = age, y = friend_count)) +
  geom_jitter(alpha = 0.1) + # test , .1 shows bulk below 1000 (dark region)
  xlim(13, 90) 

ggplot(data = data, aes(x = age, y = friend_count)) +
  geom_point(alpha = 0.05) + # test , .1 shows bulk below 1000 (dark region)
  xlim(13, 90) +
  coord_trans(y = 'sqrt') # transform using sqrt

library(dplyr)

age_groups <- group_by(data, age)
age_groups

data_age_group <- data %>% 
  group_by(age) %>% 
  summarise(friend_count_mean = mean(friend_count),
            friend_count_median = median(friend_count),
            n = n()
  )

ggplot(data = data_age_group, aes(x = age, y = friend_count_mean)) +
  geom_point(aes(col = friend_count_mean, size = .01, alpha = 0.1)) +
  geom_line() + 
  labs(color = "Mean Age",
       title = "Line Chart",
       labels = "test",
       x = "ages",
       y = "mean friend count",
       caption = "Source: Udacity",
       size = "Fixed")

# overlaying summaries with raw data
ggplot(data = data, aes(x = age, y = friend_count)) +
  geom_point(alpha = 0.05,
             position = position_jitter(h = 0),
             color = "orange"
             ) +
  xlim(13, 90) +
  coord_trans(y = "sqrt") +
  geom_line(stat = "summary", fun.y = mean) +
  geom_line(stat = "summary", fun.y = quantile, fun.args = list(probs = 0.1)
            ,linetype = 2, color = "blue") + # give 10th percentile of plot
  geom_line(stat = "summary", fun.y = quantile, fun.args = list(probs = 0.9)
            ,linetype = 2, color = "red") + # give 90th percentile of plot
  geom_line(stat = "summary", fun.y = quantile, fun.args = list(probs = 0.5)
          ,linetype = 2, color = "green") + # give 50th percentile of plot
  coord_cartesian(xlim = c(13,70), ylim = c(0,3000)) # zooms in


## correlation
# cor.test(x, y, 
#          alternative = c("two.sided", "less", "greater"),
#          method = c("pearson", "spearman", "kendall"),
#          exact = null,
#          conf.level = 0.95)

# run a cor.test() vs. with()
cor.test(data$age, data$friend_count, method = "pearson")
with(data, cor.test(age, friend_count), method = "pearson")

# correlation on subsets
with(data[data$age<70,], cor.test(age, friend_count)) # diff than full data
with(subset(data, age < 70), cor.test(age, friend_count)) # defaults to pearson

# test it with spearman method
with(subset(data, age < 70), cor.test(age, friend_count), method = "spearman")
with(data[data$age<70,], cor.test(age, friend_count), method = "spearman")


# strong correlations (choose data to look at; subset the view)
library(ggplot2)
ggplot(data = data, aes(x = www_likes_received, y = likes_received)) +
  geom_point() +
  xlim(0, quantile(data$www_likes_received, 0.95)) + # get 95th percentile of axis (zooms in)
  ylim(0, quantile(data$likes_received, 0.95)) +
  geom_smooth(method = "lm", color = "red") # slope of the graph
  # geom_smooth(method = "loess", color = "blue")

# so what's the correlation between the two variables above?
# Include the top 5% of values for the variables in the calc 

# answer
cor.test(data$www_likes_received, data$likes_received)

## problem chapter 4

library(ggplot2)
head(diamonds)
str(diamonds)
summary(diamonds)
ggplot(data = diamonds, aes(x=price, y = x)) + geom_point()
ggplot(data = diamonds, aes(x = x, y = price)) + geom_point()

cor.test(diamonds$x, diamonds$price)
cor.test(diamonds$y, diamonds$price)
cor.test(diamonds$z, diamonds$price)

ggplot(data = diamonds, aes(x = depth, y = price)) + geom_point()
# Change the code to make the transparency of the
# points to be 1/100 of what they are now and mark
# the x-axis every 2 units. See the instructor notes
# for two hints.
ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha = 1/100) + 
  scale_x_continuous(breaks =  seq(43,79,2), labels =  seq(43,79,2))
cor.test(diamonds$price, diamonds$depth)

# Create a scatterplot of price vs carat
# and omit the top 1% of price and carat
# values.
summary(diamonds$price)
summary(diamonds$carat)

top_price <- quantile(diamonds$price,probs = 0.99)
top_carat <- quantile(diamonds$carat, probs = 0.99)
data <- subset(diamonds, diamonds$price < top_price & diamonds$carat < top_carat)

ggplot(data = data,
       aes(x = carat, y = price)) + geom_point()

# Create a scatterplot of price vs. volume (x * y * z).
# This is a very rough approximation for a diamond's volume.

diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
ggplot(diamonds, aes(x = volume, y = price)) + geom_point()

vol.cor.data <- subset(diamonds, volume != 0 & volume < 800 )
cor.test(vol.cor.data$volume, vol.cor.data$price)

# Subset the data to exclude diamonds with a volume
# greater than or equal to 800. Also, exclude diamonds
# with a volume of 0. Adjust the transparency of the
# points and add a linear model to the plot. (See the
# Instructor Notes or look up the documentation of
# geom_smooth() for more details about smoothers.)

ggplot(vol.cor.data, aes(x = volume, y = price)) +
  geom_point(alpha = 1/10) +
  geom_smooth(method = "lm")

# Name the data frame diamondsByClarity

# The data frame should contain the following
# variables in this order.

#       (1) mean_price
#       (2) median_price
#       (3) min_price
#       (4) max_price
#       (5) n

library(dplyr)
str(diamonds)
diamondsByClarity <- diamonds %>%
  group_by(clarity) %>%
  summarise(mean_price = mean(price),
            median_price = median(as.numeric(price)),
            min_price = min(price,na.rm = TRUE),
            max_price = max(price,na.rm = TRUE),
            n = n()) %>%
  arrange(clarity)

# We've created summary data frames with the mean price
# by clarity and color. You can run the code in R to
# verify what data is in the variables diamonds_mp_by_clarity
# and diamonds_mp_by_color.

# Your task is to write additional code to create two bar plots
# on one output image using the grid.arrange() function from the package
# gridExtra.

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

library(gridExtra)
p1 <- ggplot(diamonds_mp_by_clarity, aes(x=clarity,y=mean_price)) +
  geom_bar(stat = "identity")
p2 <- ggplot(diamonds_mp_by_color, aes(x=color,y=mean_price)) +
  geom_bar(stat = "identity")

grid.arrange(p1,p2,ncol = 1)

## lesson 5: multivariate
data <- read.delim('~/R Projects/MiscData/pseudo_facebook.tsv')

ggplot(data = data, aes(x = gender, y = age)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = "point", shape = 4)

ggplot(data = data[!is.na(data$gender),], aes(x = age, y = friend_count)) +
  geom_line(aes(color = gender), stat = "summary", fun.y = median)

library(dplyr)
# group by age and gender to create new df group
age_group <- data %>% 
  filter(!is.na(gender)) %>% 
  group_by(age, gender) %>% 
  summarise(mean_friend_count = mean(friend_count),
            median_friend_count = median(friend_count),
            n = n()) %>% 
  ungroup() %>% 
  arrange(age)

ggplot(data = subset(data, !is.na(gender)), 
       aes(x = age, y = friend_count)) +
  geom_line(aes(color = gender), stat = 'summary', fun.y = median)

# reshaping data
library(tidyr)
age_group_wide <-
  subset(age_group[c('age', 'gender', 'median_friend_count')],
         !is.na(gender)) %>%
  spread(gender, median_friend_count) %>%
  mutate(ratio = male / female)

head(age_group_wide)

# ratio plot
ggplot(data = age_group_wide, aes(x = age, y = female/male)) +
  geom_line() + 
  geom_hline(yintercept = 1, alpha = 0.5, linetype = 2)
# younger people have a higher friends ratio

## yogurt dataset
data <- read.csv("~/R Projects/MiscData/yogurt.csv")

library(tidyverse)

head(data)
str(data)

#change the id from an int to a factor
data$id <- factor(data$id)
str(data)

ggplot(data = data, aes(x = price, fill = "#F79420")) +
  geom_histogram() +
  stat_bin(bins = 20)

data<- transform(data, all_purchases = strawberry + blueberry +
                   pina.colada + plain + mixed.berry)

summary(data$all_purchases)

# plot price over time (histogram)
ggplot(data = data, aes(x = all_purchases, fill = "blue")) +
  geom_histogram(bins = 20) # plot shows purchases per household

ggplot(data = data, aes(x = time, y = price)) +
  geom_jitter(alpha = 0.25, shape = 21, fill = "#F79420")

## chapter 9

# scatterplot review
library(tidyverse)

# omit top1% of values for each variable
qplot(data = diamonds, x = carat, y = price,
      xlim = c(0, quantile(diamonds$carat, 0.99)),
      ylim = c(0, quantile(diamonds$price, 0.99))) +
  geom_point(fill = "#F79420", color = "black", shape = 21)


# ggplot version
ggplot(data = diamonds, aes(x = carat, y = price)) +
  geom_point(fill = "#F79420", color = "black", shape = 21) +
  stat_smooth(method = "lm") +
  scale_x_continuous(lim = c(0, quantile(diamonds$carat, 0.99))) +
  scale_y_continuous(lim = c(0, quantile(diamonds$price, 0.99)))

# price and carat relationship
# nonlinear relationship, dispersion (variance) also increases
# as carat increases - price to carat
# adding linear line (stat_smooth) doesnt go through center of data

library(GGally)
library(scales)
library(MASS)

set.seed(20022012)
diamond_samp <- diamonds[sample(1:length(diamonds$price), 10000),]
ggpairs(diamond_samp,
        lower = list(continuous = wrap("points", shape = I('.'))),
        upper = list(combo = wrap("box", outlier.shape = I('.'))))

# demands of diamonds
library(gridExtra)

plot1 <- qplot(data = diamonds, x = price, binwidth = 100, fill = "#099dd9") + 
  ggtitle('Price')

plot2 <- qplot(data = diamonds, x = price, binwidth = 0.01, fill = "#F79420") +
  ggtitle('Price (log10)') +
  scale_x_log10()

grid.arrange(plot1, plot2, ncol = 2)

# looking at these plots, there are two peaks in log10 graph
# prices are skewed to the right 
# with log, looks more normal with a bimodal peak

# plot price on log10() scale
qplot(data = diamonds, x = carat, y = price, color = clarity) +
  scale_y_continuous(trans = log10_trans() ) +
  ggtitle("Price (log10) by Carat")

# overplotting revisted
head(sort(table(diamonds$price), decreasing = TRUE))
head(sort(table(diamonds$carat), decreasing = TRUE))
# 
# ggplot(aes(carat, price), data = diamonds) + 
#   geom_point() + 
#   scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
#                      breaks = c(0.2, 0.5, 1, 2, 3)) + 
#   scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
#                      breaks = c(350, 1000, 5000, 10000, 15000)) +
#   ggtitle('Price (log10) by Cube-Root of Carat')

qplot(data = diamonds, x = carat, y = price, color = cut) +
  scale_y_continuous(trans = log10_trans() ) +
  ggtitle("Price (log10) by Carat")

# let's simulate the data the explanatory variables: temperature (x1),
# precipitation (x2) and the treatment (1=Control, 2= N addition)
set.seed(1)
x1 <- rnorm(100, 10, 2)
x2 <- rnorm(100, 100, 10)
x3 <- gl(n = 2, k = 50)
modmat <- model.matrix(~x1 + x2 + x3, data = data.frame(x1, x2, x3))
# vector of fixed effect
betas <- c(10, 2, 0.2, 3)
# generate data
y <- rnorm(n = 100, mean = modmat %*% betas, sd = 1)
# first model
m <- lm(y ~ x1 + x2 + x3)
summary(m)

plot(y ~ x1, col = rep(c("red", "blue"), each = 50)
     , pch = 16
     , xlab = "Temperature [°C]"
     , ylab = "Soil biomass [mg]")
abline(a = coef(m)[1] + coef(m)[3] * mean(x2), b = coef(m)[2], 
       lty = 2, 
       lwd = 2, 
       col = "red")
abline(a = coef(m)[1] + coef(m)[4] + coef(m)[3] * mean(x2), 
       b = coef(m)[2], 
       lty = 2, 
       lwd = 2, 
       col = "blue")

# averaging effect of the factor variable
abline(a = coef(m)[1] + mean(c(0, coef(m)[4])) + coef(m)[3] * mean(x2), b = coef(m)[2], 
       lty = 1, lwd = 2)
legend("topleft", 
       legend = c("Control", "N addition"), 
       col = c("red", "blue"), 
       pch = 16)









