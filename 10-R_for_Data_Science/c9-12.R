## setwd('C:/Users/User/Documents/R/win-library/3.4')

library(tidyverse)

####### Chapter 10: Introduction to Wrangling: Tibbles

## Import --> Tidy --> Transform <--> Visualize <--> Model --> Communicate

head(as.data.frame(iris))
as.tibble(iris)

## Create a head-tail dataframe glance
ht <- function (df) 
  rbind(head(df, 5), tail(df, 5)
)

ht(iris)

## CReate a new tibble from individual vectors
## tibble() will automatically recycle inputs of length 1

tibble (
  x = 1:5,
  y = 1,
  z = x ^ 2 + y
)

## tibble is simpler version that will not convert variable types/row names

## Use `` for non-syntactic names

tb <- tibble (
  `:)` = "smile",
  ` `  = "space",
  `2000` = "number"
)

tb 

## tribble() = transposed tibble
## tribble is customised for data entry in code
## col head are defined by formulas (starts with ~)

tribble(
  ~x, ~y, ~z,
  #--/--/----,
  "a", 2, 3.6,
  "b",1, 8.5
)

# There are two main differences in the usage of a 
#  tibble vs. a classic data.frame: printing and subsetting.

tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)

## tibbles are designed so that you dont accidentally overwhelm
##  your console when you print large dfs

nycflights13::flights %>% 
  print(n = 10, width = Inf) ## display all columns

nycflights13::flights %>% 
  View ## opens up new tab of dataframe

set.seed(1)
df <- tibble(
  x = runif(5),
  y = rnorm(5)
)
# Extract by name
df$x
df[["x"]]

# Extract by position
df[[1]]

# To use these in a pipe, you’ll need to use the special placeholder .:
df %>% .$x
df %>% .[["x"]]


####### Chapter 11: Data Import

## read_*()
## * = [csv, csv2. tsv, delim, fwf, log]

heights <- read_csv("data/heights.csv")

## Useful for experimenting writing it out
read_csv("Col1,Col2,Col3
          1,2,3
          4,5,6")

## Parameters 

# col_names = FALSE
# \n ## new line

read_csv("1,2,3\n4,5,6", col_names = c("X","y","z"))

## readr functions are faster than base
## read_csv() > read.csv()

#### Performance indicator

## data.table() uses tibbles but doesnt fit into tidyverse
## data.table() is faster in raw speed terms
## base functions also inherit some behavior from user's computer
##  may not work on someone else's


## Identify what is wrong

read_csv("a,b\n1,2,3\n4,5,6") ## wrong because you do not need extra commas
read_csv("a,b,c\n1,2\n1,2,3,4") ## doesn't match row/col count
  read_csv("a,b,c\n1,2,3\n1,2,3") ## answer

## 11.3 Parsing a Vector (readr)

# str(parse_logical(c()))
  # logical, integer, date

parse_integer(c("1", "231", ".", "456"), na = ".")
  
x <- parse_integer(c("123", "345", "abc", "123.45")) 

# Use problems() to get parsing failures (returns a tibble)
problems(x)  


### Issues: formmatting, non-numeric char

## To address number formmatting problems, use locale() parameter

parse_double("1.23")

parse_double("1,23", locale = locale(decimal_mark = ","))

# parse_number() ignores non-numeric characters

parse_number("$123,456,789")
parse_number("123'456'789", locale = locale(grouping_mark = "'"))
parse_number("123.456.789", locale = locale(grouping_mark = "."))

## Strings for another language - Encoding

(x1 <- "El Ni\xf1o was particularly bad this year")
(x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd")
parse_character(x1, locale = locale(encoding = "Latin1"))
parse_character(x2, locale = locale(encoding = "Shift-JIS"))

## Attempt to let R to find the encoding for you

guess_encoding(charToRaw(x1))
guess_encoding(charToRaw(x2))

fruit <- c("apple", "banana")
parse_factor(c("apple","banana","bananana"), levels = fruit)

# parse_datetime() expects an ISO8601 date-time

parse_datetime("2010-10-01T2010")
parse_datetime("20101010")

# parse_date() expects a four-digit year, a - or /, the month, day
# format = "yyyy/mm/dd"

# library(hms)
parse_time("01:10 am")
parse_time("20:10:01")

## Test out parse_date() to find out formmatting

parse_date("01/02/15", "%m/%d/%y")
parse_date("01/02/15", "%d/%m/%y")
parse_date("01/02/15", "%y/%m/%d")

####### Skipped some work based on reading a file and writing it
####### Work on examples for csv files and look at parameters 



####### Chapter 12: Tidy Data

tidy4a <- table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")

tidy4b <- table4b %>% 
  gather(`1999`, `2000`, key = "year", value = "population")

left_join(tidy4a, tidy4b)


stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>% 
  spread(year, return) %>% 
  gather("year", "return", `2015`:`2016`)


