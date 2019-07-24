library(tidyverse)


### Chapter 17: Iteration with purrr

# purrr pkg

## Reducing code duplication has 3 main benefits
# 1. It's easier to see the intent of the code
# 2. It's easier to response to changes in requirements. It's easier to make changes in one place than several.
# 3. You're likely to have fewer bugs because each line of code is used in more places.

## Tools to Reduce
# One tool for reducing duplication is functions, which reduce duplication by identifying same patterns.
# Another tool to reduce duplication is iteration.

## This chapter will teach you about two important iteration paradigms:
# 1. Imperative Programming
  # Tools like for loops and while loops
# 2. Functional Programming
  # Tools to extract out this duplicated code, so for each common for loop pattern gets its own function

#**************
# Master vocabulary for FP, you can solve many common iteration problems with less code, more ease, and fewer errors.
#**************

# Create tibble
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

# Compute the median of each column
# Manual
median(df$a) # repeat for b - d

# Create a for loop so you do not repeat
output <- vector("double", ncol(df))

for (i in seq_along(df)) {
  output[[i]] <- median(df[[i]])
}
output

# Calculate the mean for every column in mtcars
output_m <- vector("double", ncol(mtcars))

for (i in seq_along(mtcars)) {
  output_m[[i]] <- median(mtcars[[i]])
}
output_m

# Number generators
# seq(), rnorm(), runif()

# Create for loop
out <- ""
for (x in letters) {
  out <- stringr::str_c(out, x)
}

x <- sample(100)
sd <- 0
for (i in seq_along(x)) {
  sd <- sd + (x[i] - mean(x))^2
}
sd <- sqrt(sd / length(x) - 1)
sd

## Variations of the basic theme of the for loop:
# 1. Modifying an existing object, instead of creating a new object.
# 2. Looping over names or values, instead of indices.
# 3. Handling outputs of unknown length.
# 4. Handling sequences of unknown length.

df2 <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
# Rescale using for loop
for (i in seq_long(df2)) {
  df2[[i]] <- rescale01(df2[[i]])
}

# Create a loop over numeric indicies to extract name and value
for (i in seq_along(df)) {
  name <- names(df)[[i]] # names(df)[[loop position]]
  value <- x[[i]]       # df[[loop position]]
}

# Repeat
for (i in seq_along(df)) {
  names <- names(df)[[i]]
  x <- df[[i]]
}

# Create a vector called out that stores a list, length of means
out <- vector("list", length(means))
for (i in seq_along(means)) {
  n <- sample(100,1)
  out[[i]] <- rnorm(n, means[[i]])
}
str(out)
out

# Create a function and while loop to find 3 heads in a row
flip <- function() sample(c("T", "H"), 1)
flips <- 0
nheads <- 0

# While() loops usually used in simulations - not widely used

while (nheads < 3) {
  if (flip() == "H") {
    nheads <- nheads + 1
  } else {
    nheads <- 0
  }
  flips <- flips + 1
}
flips

# What does this code do?
trans <- list(
  disp = function(x) x * 0.01673871,
  am = function(x) {factor(x, labels = c("auto", "manual"))}
)
for (var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}
trans[1]

# For loops vs. Functionals (More valuable in R)

# Create a tibble
df3 <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

# Create a function to extract means
col_mean <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- mean(df[[i]])
  }
  output
}

col_mean(df3)

# Repeat

col_mean_s <- function(df) {               # Create a function
  output <- vector("double", length(df))   # Create an empty vector to hold the data - vector("data type", length)
  for (i in seq_along(df)) {               # Create a for loop to loop through df
    output[i] <- mean(df[[i]])             # Input data into new vector for each position 
  }
}

# Summary by function (Functional programming)
col_summary <- function(df, fun) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- fun(df[[i]])
  }
  output
}

col_summary(df3, median)
col_summary(df3, mean)

# Fit a linear model to each cylinder group
models <- mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df))

models

models <- mtcars %>% 
  split(.$cyl) %>% 
  map( ~ lm(mpg ~ wt, data = .)) # used . as a pronounce; refers to current list element
models

# Summary (R^2)
models %>% 
  map(summary) %>% 
  map_dbl(~.$r.squared)

models %>% 
  map(summary) %>% 
  map_dbl("r.squared")

col_summary <- function(df, fun) {         # Create a function with two parameters
  output <- vector("double", length(df))   # Create an empty vector to store mean
  for (i in seq_along(df)) {               # Create a for loop to loop through df
     output[i] <- mean(df[[i]])            # Input new mean data into vector position
  }
}


df %>% 
  map_dbl(mean)

df %>% 
  map_dbl(median)

df %>% 
  map_dbl(sd)


models %>% 
  map(summary) %>% 
  map_dbl(~.$r.squared)

models %>% 
  map(summary) %>% 
  map_dbl("r.squared")

# lapply() is basically identical to map() except that map() is consistent with all other functions

looper("numeric", mtcars, mean)



















