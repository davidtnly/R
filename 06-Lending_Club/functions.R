################## List of functions that are used frequently ################## 
################################################################################

#############################################################
# Check for NA values & Sort by descending order
#############################################################
check_na <- function(x, n = 30) {
  t <- colSums(sapply(x, is.na)) # Count NAs
  na_features <- head(sort(t, decreasing = TRUE), n) # Sort top 30 NA features
  return(na_features)
}

#############################################################
# Summary; get summary level details such as structure
#############################################################
details <- function(x) {
  str(data)
  colnames(data)
  glimpse(data)
  summary(data)
  dim(data)  
}

#############################################################
# Convert date (%d-%b-%Y); format must be like "01-01-2019"
#############################################################
convert_date <- function(x) {
  as.Date(paste0("01-",x), format = "%d-%b-%Y")
}