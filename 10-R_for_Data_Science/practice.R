library(tidyverse)
library(MASS)
library(gridExtra)
library(ggthemes)

## https://www.linkedin.com/pulse/weighted-linear-regression-r-blaine-bateman-eaf-llc/
## Blaine Bateman example problem on Weighted Lin regression
# model <- lm(y ~ x, data = x_data)
# y_pred <- predict(model, data = new_x_data)


## Generate data for regression
set.seed(1)
x_data <- seq(1, 100, 1)
y_raw <- 3.5 + 2.1 * x_data
y_noise <- rnorm(n = 100, mean = 0, sd = 5)

y <- data.frame(x = x_data, y = y_raw + y_noise)

## Model

pred <- lm(y ~ x, data = y)

plot(pred)

resid(pred) # list of residuals

ggplot(data = pred, aes(x = resid(pred))) +
  geom_histogram() +
  geom_density(aes(y = ..count..)) +
  theme_bw()

ggplot(data = pred, aes(x = resid(pred))) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(aes(y = ..density..)) +
  theme_bw()

ggplot(data = pred, aes(x = resid(pred))) +
  stat_bin(geom = "bar", position = "dodge", bins = 15) +
  theme_bw()

summary(pred)

######### Test messier data

set.seed(2)

X_data2 <- seq(1, 1000, 1)

#

# Y is linear in x with uniform, periodic, and skewed noise

#

Y_raw2 <- 1.37 + 2.097 * x_data2

Y_noise2 <- (X_data2 / 100) * 25 * (sin(2 * pi * X_data2/100)) * 
  
  runif(n = length(X_data2), min = 3, max  = 4.5) +
  
  (X_data2 / 100)^3 * runif(n = 100, min = 1, max = 5)

Y <- data.frame(X = X_data2, Y = Y_raw2 + Y_noise2)

model2 <- lm(Y ~ X, data = Y)
plot(model2)
summary(model2)

ggplot(data = model2, aes(x = resid(model2))) +
  geom_histogram(aes(y = ..density..)) +
  geom_density() +
  theme_bw()

## heteroskedasticity: residuals do exhibit the unwanted
## variations we observe (need homoskedasticity)

# Could apply weights to the linear regression model
# How to normalize data


# Weighted_fit <- rlm(Y ~ X, data = Y, weights = 1/sd_variance)

########################################################################

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  labs(title = "Fuel economy declination") +
  theme(plot.title = element_text(size = rel(1.5)),
        plot.background = element_rect(fill = "gray"),
        panel.border = element_rect(linetype = "dashed", fill = NA),
        panel.grid.major = element_line(colour = "black"),
        panel.grid.minor = element_line(colour = "grey"), 
        # panel.grid.major.y = element_blank() # remove y grids
        # panel.grid.major.x = element_blank() # remove x grids
        
        ## Axes --------------------------------------------- 
        
        axis.line = element_line(size = 3, colour = "grey80"),
        axis.text = element_text(colour = "red"),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(0.25, "cm"),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1))
        
  
  )


## Legend ---------------------------------------------------

ggplot(mtcars, aes(wt, mpg)) +
  geom_point(aes(colour = factor(cyl), shape = factor(vs))) +
  labs(
    title = "Weights and Mpg",
    x = "Weight (1000 lbs)",
    y = "Fuel economy (mpg)",
    colour = "Cylinders",
    shape = "Transmission") + 
  theme(legend.justification = c("right","top"),
        legend.title = element_text(face = "bold"),
        legend.position = c(.95, .95), #"bottom"
        legend.box.just = "right",
        legend.margin = margin(2, 2, 2, 2),
        legend.key.size = unit(.25, "cm"),
        #legend.key.width = unit(5, "cm")
        #legend.text = element_text(colour = 'red'
         #, angle = 45, size = 10, hjust = 3, vjust = 3, face = 'bold')
        # legend.box.background =  element_rect(),
        legend.background = element_rect(colour = "light gray", size = .25), 
        plot.title = element_text(size = 2, hjust = 0.5),
        plot.margin = margin(15, 15, 15, 15),
        plot.background = element_rect(fill = "gray", linetype = "dashed"),
        panel.grid.minor = element_line(colour = "grey"),
        panel.border = element_rect(linetype = "dashed", fill = NA),
        axis.title.y = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black")
        )

## Strips -----------------------------------------------------

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  facet_wrap( ~ cyl ) +
  theme(
    strip.background = element_rect(colour = "black", fill = "light pink"),
    strip.text = element_text(colour = "white", face = "bold"),
    panel.spacing = unit(1, "lines")
    
    )


# par(mfrow = c(2,1))

p1 <- ggplot(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  facet_wrap( ~ cyl) +
  theme(
    strip.background = element_rect(colour = "black", fill = "light pink"),
    strip.text = element_text(colour = "white", face = "bold"),
    panel.spacing = unit(1, "cm")
    
  )


ggplot(data = mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  facet_wrap( ~ cyl) +
  labs(
    title = "Weights and Mpg",
    sub = "Subtitle",
    x = "Weight (1000 lbs)",
    y = "Fuel economy (mpg)",
    colour = "Cylinders",
    shape = "Transmission") +
  # ylim(c(0,100)) + # limit y axis range
    # alternatives to limit range
    # scale_x_continuous(limits = c(0,50))
  theme(
    strip.background = element_rect(colour = "black", fill = "light pink"),
    strip.text = element_text(colour = "white", face = "bold"),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(size = 10, hjust = 0.5, lineheight = 0.8,
                              vjust = 1),
     #hjust = 0.5 to center or margin = margin(10, 0 ,10 ,0)
    plot.subtitle = element_text(size = rel(.6)),
    axis.ticks = element_blank(),
    axis.text = element_blank(), # remove tick text and ticks
    #axis.text.x = element_text(color, angle , size , vjust ) # change size of and rotate axis text
    #legend.title = element_blank()
    legend.title = element_text(colour = "chocolate", size = 10, face = "bold"),
    legend.key = element_rect(fill = "white"),
    panel.grid.major = element_line(colour = "grey"),
    panel.grid.minor = element_line(colour = "grey"),
    legend.background = element_rect(colour = "black", linetype = "dashed", fill = "white")
    # plot.background = element_rect(fill = NA)
    
    ) +
  scale_color_discrete(name = "Cylindas")


# grid.arrange(p1,p2,nrow = 2)
# additional practice
# http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/

ggplot(data = mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  geom_smooth(
    aes(group = 1), 
    method = "loess", 
    span = 0.7, 
    se = FALSE,
    col = "black",
    linetype = "dashed"
    ) +
  labs(
    title = "Weight vs. MPG with Linear Regression Line",
    x = "Weight (KG)",
    y = "Miles per Gallon",
    col = "Cylinders"
  ) +
  theme(
    legend.title = element_text(colour = "blue", face = "bold", size = 7),
    legend.key = element_rect(fill = "light grey"),
    legend.background = element_rect(colour = "black", linetype = "dashed"),
    legend.position = c(.9, .70),
    legend.key.size = unit(.5, "cm"),
    legend.margin = margin(4, 4, 4, 4),
    
    plot.title = element_text(face = "bold", size = 10, 
                              hjust = .5, lineheight = 1, vjust = 1),
    plot.background = element_rect(fill = "light grey", linetype = "dashed", colour = "red"),
    
    panel.border = element_rect(linetype = "dashed", fill = NA),
    panel.grid.major = element_line(colour = "light pink"),
    panel.grid.minor = element_line(colour = "black"),
    # axis.title = element_text(face = "bold", colour = "red"),
    # axis.ticks = element_line(colour = "purple"),
    # put axis ticks and text to blank()
    axis.ticks = element_blank(), # line 233
    # axis.text = element_blank(),
    axis.text.x = element_text(colour = "red")
    
  ) 
  # manual colors remove from theme
  # scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) 
  # scale_fill_brewer(palette = "dark")
  # scale_color_gradientn(colours = rainbow(5))

### Gradient colors for normal distribution histogram

set.seed(1234)
x <- rnorm(200)

qplot(x = x, fill = ..count.., geom = "histogram")  +
  # stat_bin(binwidth = .5) +
  scale_fill_gradient(low = "blue", high = "aquamarine4") +
  labs(
    title = "Normal Distribution Chart",
    x = "X",
    y = "Count"
  ) +
  theme(
    ## Start with axis, legend, panel, strip, plot
    
    axis.title.x = element_text(colour = "white"), # xlab("x title, colour = "red")
    axis.title.y = element_text(colour = "Blue", size = 10), # face, family, vjust, hjust
    axis.ticks.x = element_line(colour = "pink"),
    
    plot.title = element_text(face = "bold", size = 10,
                              vjust = 4, hjust = 0.5, lineheight = 10),
    plot.background = element_rect(fill = "darkseagreen", colour = "red", linetype = "dashed"),
    plot.margin = unit(c(1, 1, 1, 1), "cm"), # top, right, bottom, left
    
    ## Remove panel borders and grid lines - change everything to element_blank()
    panel.border = element_rect(linetype = "dashed", fill = NA),
    panel.grid.major = element_line(colour = "light pink"),
    panel.grid.minor = element_line(colour = "black"),
    
    # legend.key = element_rect(fill = "transparent", colour = "transparent")
    legend.key = element_rect(fill = "darkseagreen", colour = "darkseagreen"),
    # linetype: blank, solid, dashed, dotted, dotdash, longdash, twodash
    legend.background = element_rect(fill = "darkseagreen", linetype = "solid", colour = "white")

  )

### theme_set(theme_bw()) # theme set for entire session

### study ggthemes: 
 ##Take a look at all the default themes (theme_economist) + scale_colour_economist()
 ## theme_stata() + scale_color_stat()
 ## theme_wsj() + scale_color_wsj("colors6")

#### Subtopics: Functions, etc.
## scale_color_gradient2( midpoints = mid, low = "", mid = "",  high" = "")
## geom_tile, scale_fill_gradient2

## Practice stat_smooth()

ggplot(data = mtcars, aes(x = wt, y = mpg, colour = factor(cyl))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, span = .7)

# myColors <- c(brewer.pal(3, "Dark2"), "black")

ggplot(data = mtcars, aes(x = wt, y = mpg, colour = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  stat_smooth(aes(group = 1, col = "All"), # add All to legend (cant add colour or wont show)
              method = "loess",
              span = 0.7,
              se = FALSE,
              linetype = "dashed",
              size = 2) 
  # scale_color_manual("Cylinders", values = myColors)

## http://r-statistics.co/Complete-Ggplot2-Tutorial-Part2-Customizing-Theme-With-R-Code.html
## Part 2: Customizing the look and feel

## Samples: http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#1.%20Correlation

library(ggcorrplot)

## Build a correlation matrix

mtcars <- data(mtcars)
corr <- round(cor(mtcars), 1)

# Plot

ggcorrplot(corr, 
           hc.order = TRUE, 
           type = "lower", # direction of charts
           lab = TRUE, 
           lab_size = 3, # text size 
           method = "square", # shapes of each correlation
           colors = c("tomato2", "white", "springgreen3"), 
           title = "Correlogram of mtcars", 
           ggtheme = theme_bw
           
           )

# Build a diverging bar chart of positive and negative values
# data("mtcars")  # load data
mtcars$`car name` <- rownames(mtcars)  # create new column for car names
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)  # compute normalized mpg
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")  # above / below avg flag
mtcars <- mtcars[order(mtcars$mpg_z), ]  # sort
mtcars$`car name` <- factor(mtcars$`car name`, levels = mtcars$`car name`)  # convert to factor to retain sorted order in plot.

## Diverging Barcharts

ggplot(mtcars, aes(x = `car name`, y = mpg_z, label = mpg_z)) + 
  geom_bar(stat = 'identity', aes(fill = mpg_type), width = .5)  +
  geom_point() +
  scale_fill_manual(name = "Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above" = "#00ba38", "below" = "#f8766d")) + 
  labs(subtitle = "Normalised mileage from 'mtcars'", 
       title = "Diverging Bars") + 
  coord_flip() +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(0.2, "cm"),
    legend.background = element_rect(colour = "black", fill = "lightblue"),
    plot.title = element_text(hjust = 0.5, face = "bold", lineheight = 0.5),
    plot.subtitle =  element_text(hjust = 0.5, size = 8, face = "italic", lineheight =  0.5),
    plot.background = element_rect("lightblue"),
    axis.title = element_text(colour = "black", face = "bold"),
    axis.text = element_text(colour = "black"),
    panel.grid.major = element_line(colour = "black"),
    panel.grid.minor = element_line(colour = "black")   
    
  )

## Diverging Lollipop Chart
# lollipop chart shows bar info and diverging bar but more modern
# how: replace geom_bar with geom_point and geom_segment

theme_set(theme_bw())

ggplot(mtcars, aes(x = `car name`, y = mpg_z, label = mpg_z)) + 
  geom_point(stat = 'identity', fill = "black", size = 6)  +
  geom_segment(aes(y = 0, 
                   x = `car name`, 
                   yend = mpg_z, 
                   xend = `car name`), 
               color = "black") +
  geom_text(color = "white", size = 2) +
  labs(title = "Diverging Lollipop Chart", 
       subtitle = "Normalized mileage from 'mtcars': Lollipop") + 
  ylim(-2.5, 2.5) +
  coord_flip()

## Set theme - Create custom function for theme: theme_custom()

theme_custom <- theme(
  legend.position = "bottom",
  legend.key.size = unit(0.2, "cm"),
  legend.background = element_rect(colour = "black", fill = "lightblue"),
  plot.title = element_text(hjust = 0.5, face = "bold", lineheight = 0.5),
  plot.subtitle =  element_text(hjust = 0.5, size = 8, face = "italic", lineheight =  0.5),
  plot.background = element_rect("lightblue"),
  axis.title = element_text(colour = "black", face = "bold"),
  axis.text = element_text(colour = "black"),
  panel.grid.major = element_line(colour = "black"),
  panel.grid.minor = element_line(colour = "black")   
  
)


theme_example <- function (base_size = 11, base_family = "") 
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      panel.background = element_rect(fill = "white", colour = NA), 
      panel.border = element_rect(fill = NA, colour = "grey20"), 
      panel.grid.major = element_line(colour = "grey92"), 
      panel.grid.minor = element_line(colour = "grey92", size = 0.25), 
      strip.background = element_rect(fill = "grey85", colour = "grey20"), 
      legend.key = element_rect(fill = "white", colour = NA), 
      complete = TRUE)
}


theme_custom <- function (base_size = 11, base_family = "") 
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      legend.position = "bottom",
      legend.key.size = unit(0.2, "cm"),
      legend.background = element_rect(colour = "black", fill = "lightblue"),
      plot.title = element_text(hjust = 0.5, face = "bold", lineheight = 0.5),
      plot.subtitle =  element_text(hjust = 0.5, size = 8, face = "italic", lineheight =  0.5),
      plot.background = element_rect("lightblue"),
      axis.title = element_text(colour = "black", face = "bold"),
      axis.text = element_text(colour = "black"),
      panel.grid.major = element_line(colour = "black"),
      panel.grid.minor = element_line(colour = "black"))  

}

## Diverging dot plot (similar to lollipop without stick)

# Plot

ggplot(mtcars, aes(x = `car name`, y = mpg_z, label = mpg_z)) + 
  geom_point(stat = 'identity', aes(col = mpg_type), size = 6)  +
  scale_color_manual(name = "Mileage", 
                     labels = c("Above Average", "Below Average"), 
                     values = c("above" = "#00ba38", "below" = "#f8766d")) + 
  geom_text(color = "white", size = 2) +
  labs(title = "Diverging Dot Plot", 
       subtitle = "Normalized mileage from 'mtcars': Dotplot") + 
  theme_custom() +
  ylim(-2.5, 2.5) +
  coord_flip()

## Create funnel pyramid chart: Email campaigns
options(scipen = 999)  # turns of scientific notations like 1e+40

# Read data
email_campaign_funnel <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/email_campaign_funnel.csv")

# X Axis Breaks and Labels 
brks <- seq(-15000000, 15000000, 5000000)
lbls = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")

# Plot
ggplot(email_campaign_funnel, aes(x = Stage, y = Users, fill = Gender)) +   # Fill column
  geom_bar(stat = "identity", width = .6) +   # draw the bars
  scale_y_continuous(breaks = brks,   # Breaks
                     labels = lbls) + # Labels
  coord_flip() +  # Flip axes
  labs(title="Email Campaign Funnel") +
  theme_tufte() +  # Tufte theme from ggfortify
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_brewer(palette = "Dark2")  # Color palette


## Create heatmap for weekdays/weekmonth

library(scales)
library(zoo)
library(plyr)

df <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/yahoo.csv")
df$date <- as.Date(df$date)  # format date
df <- df[df$year >= 2012, ]  # filter reqd years

# Create Month Week
df$yearmonth <- as.yearmon(df$date)
df$yearmonthf <- factor(df$yearmonth)
df <- ddply(df,.(yearmonthf), transform, monthweek = 1 + week - min(week))  # compute week number of month
df <- df[, c("year", "yearmonthf", "monthf", "week", "monthweek", "weekdayf", "VIX.Close")]
head(df)
#>   year yearmonthf monthf week monthweek weekdayf VIX.Close
#> 1 2012   Jan 2012    Jan    1         1      Tue     22.97
#> 2 2012   Jan 2012    Jan    1         1      Wed     22.22
#> 3 2012   Jan 2012    Jan    1         1      Thu     21.48
#> 4 2012   Jan 2012    Jan    1         1      Fri     20.63
#> 5 2012   Jan 2012    Jan    2         2      Mon     21.07
#> 6 2012   Jan 2012    Jan    2         2      Tue     20.69


# Plot
ggplot(data = df, aes(monthweek, weekdayf, fill = VIX.Close)) + 
  geom_tile(colour = "white") + 
  facet_grid(year~monthf) + 
  scale_fill_gradient(low = "red", high = "green") +
  labs(x = "Week of Month",
       y = "",
       title = "Time-Series Calendar Heatmap", 
       subtitle = "Yahoo Closing Price", 
       fill = "Close")
