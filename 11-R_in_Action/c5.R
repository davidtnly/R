
## Generating data from a multivariate normal distribution
library(MASS)
options(digits = 3)
set.seed(1234)

mean <- c(230.7, 146.7, 3.6)
sigma <- matrix(c(15360.8, 6721.2, -47.1,  
                  6721.2, 4700.9, -16.5,                 
                  -47.1,  -16.5,   0.3), nrow = 3, ncol = 3
)
sigma

###############
## mvrnorm() ##
##    produces one or more samples from the specific multivariate normal distribution

# Generate data into a dataframe
mydata <- mvrnorm(500, mean, sigma) # Generate data (500 pseudo-random obs)
mydata <- as.data.frame(mydata) # For convenience, results are converted from a matrix to df

names(mydata) <- c("y", "x1", "x2")

# View Data
dim(mydata) # View results
colSums(sapply(mydata,is.na))
head(mydata, n = 10)

## Number generator quick review

set.seed(199)
x <- runif(10) # generates random values from the uniform
x

set.seed(19)
x2 <- rnorm(10, mean = 0, sd = 1) # generate random values from norm dist
x2

set.seed(16)
x3 <- seq(1,10,2) # generate values with a min and max
x3

# create a random sequence with runif and seq
xx <- seq(1:20)
yy <- xx + runif(20) # randomization
xx
yy

# use sample()
sample_x <- sample(1:20, 5, replace = TRUE)
sample_x


## Character Functions
x <- c("ab", "cde", "fghij")
nchar(x[3]) # looks at third element of vector

x <- "abcdefg"
substr(x, 2, 4) # returns "bcd"
substr(x, 2, 4) <- "222" # replaces bcd with 222
x

grep("A", c("b", "A", "c")) # searches for pattern in x and returns matching indice

sub("\\s", ".", "Hello There") # finds pattern text in x and substitutes with replacement

y <- strsplit("abc", "a") # split elements of vector x at split
y

y <- strsplit("abc", "c") # split elements of vector x at split
y

# substr(x, start, stop)
# grep(pattern, x)
# sub(pattern, replacement, x)
# strsplit(x, split)
# seq(to, from, by)
# length(x)
# rep(x, n)

# Create a small table with new columns
treatment <- rep(c("Placebo", "Treated"), times = 3)
improved <- rep(c("None", "Some", "Marked"), each = 2)
treatment
improved

freq <- c(29, 13, 7, 17, 7, 21)
freq

mytable <- as.data.frame(cbind(treatment, improved, freq))
mytable












