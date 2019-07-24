library(tidyverse)
library(caret)
library(gridExtra)
library(corrplot)
library(Hmisc)
library(knitr)
library(gridExtra)
library(randomForest) 
library(ModelMetrics)
library(e1071) 
library(ngram)
# library(doMC)



## https://www.kaggle.com/c/santander-value-prediction-challenge

## Step 1: Process the Data
# Load Data
# train <- read.csv('SantanderValue/train.csv', stringsAsFactors = FALSE, header = TRUE)
# test <- read.csv('SantanderValue/test.csv', stringsAsFactors = FALSE, header = TRUE)
 
# Check for NAs
# sapply(train, function(x) {is.na(x)})
# colSums(sapply(train,is.na))

## Functions
numerify <- function(x, omit, dig) {
  x1 <- data.matrix(x[, -omit])
  x2 <- round(x1, dig)
  x3 <- data.frame(cbind(x[, omit], x2))
  return(x3)
}

find_ngrams <- function(x, n) {
  get.ngrams(ngram(paste(x, collapse = " "), n))
}

find_matches <- function(x, look_in) {
  result <- lapply(look_in, FUN = function(y, x) x %in% y, x)
  return(unlist(result))
}

test_matchmaker <- function(n, tries) {
  #registerDoMC(cores = 2)
  foreach(r = 1:nrow(new_train), .combine = c) %do% { # %dopar%
    rownum <- r
    x <- new_train$code[rownum]
    matches <- find_matches(x, ngrams_corpus)
    matches[rownum] <- FALSE
    num_match <- sum(matches)
    if(num_match >= 40 | num_match < 1) {
      answer <- NA
    } else {
      x_ng <- ngrams_train[[rownum]]
      ng_common <- foreach(i = which(matches == TRUE), .combine = c) %do% sum(x_ng %in% ngrams_corpus[[i]])
      matches_ordered <- which(matches == TRUE)[order(ng_common, decreasing = TRUE)]
      found <- 0
      j = 1
      while(found == 0 & j <= tries & j <= length(matches_ordered)) {
        match_index <- matches_ordered[j]
        match_row <- corpus[match_index, ]
        test_row <- new_train[rownum, ]
        lag <- first(which(match_row == test_row[, 1])) - 1
        if(lag <= (ncol(match_row) - 4) & lag > 1) {
          double_check <- 1*(match_row[, (lag + 2)] == test_row[, 2]) +
            1*(match_row[, (lag + 3)] == test_row[, 3]) +
            1*(match_row[, (lag + 4)] == test_row[, 4])
        } else {
          double_check <- 0
        }
        if(double_check == 3) {
          answer <- match_row[, (lag - 1)]
          found <- 1
        } else {
          answer <- NA
          j <- j + 1
        }
      }
    }
    answer == train_data$target[rownum]
  } ->
    result
  sum(na.omit(result))/nrow(new_train)
}

run_matchmaker <- function(new_data, ngrams, n, tries) {
  #registerDoMC(cores = 2)
  foreach(r = 1:nrow(new_data), .combine = c) %do% { # %dopar%
    rownum <- r
    x <- new_data$code[rownum]
    matches <- find_matches(x, ngrams_corpus)
    matches[rownum] <- FALSE
    num_match <- sum(matches)
    if(num_match >= 40 | num_match < 1) {
      answer <- NA
    } else {
      x_ng <- ngrams[[rownum]]
      ng_common <- foreach(i = which(matches == TRUE), .combine = c) %do% sum(x_ng %in% ngrams_corpus[[i]])
      matches_ordered <- which(matches == TRUE)[order(ng_common, decreasing = TRUE)]
      found <- 0
      j = 1
      while(found == 0 & j <= tries & j <= length(matches_ordered)) {
        match_index <- matches_ordered[j]
        match_row <- corpus[match_index, ]
        test_row <- new_data[rownum, ]
        lag <- first(which(match_row == test_row[, 1])) - 1
        if(lag <= (ncol(match_row) - 4) & lag > 1) {
          double_check <- 1*(match_row[, (lag + 2)] == test_row[, 2]) +
            1*(match_row[, (lag + 3)] == test_row[, 3]) +
            1*(match_row[, (lag + 4)] == test_row[, 4])
        } else {
          double_check <- 0
        }
        if(double_check == 3) {
          answer <- match_row[, (lag - 1)]
          found <- 1
        } else {
          answer <- NA
          j <- j + 1
        }
      }
    }
    return(answer)
  } ->
    result
  return(result)
}

## Load Data
cat("Loading data...")

train_data <- train
test_data <- test
# sample_submission <- read.csv("../input/sample_submission.csv")

cat("\n")