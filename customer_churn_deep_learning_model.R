# Set working directory
setwd("~/R Projects")

# Load libraries
source("Public_R_Projects/libraries.R") # frequently used libraries
source("Public_R_Projects/functions.R") # frequently used functions: check_na, details, convert_date
library(keras)
library(lime)
library(rsample)
library(recipes)
library(yardstick)
library(corrr)

# Set directories
datadir <- "~/R Projects/Public_R_Projects_Data/"
maindir <- "~/R Projects/Public_R_Projects/"
filename <- "telco_churn_data.csv"

# Load data
data <- read.csv(paste0(datadir,filename))

# Following some M.Dancho workflow
# Structure
details(data)
check_na(data)

# Check NA data
data[is.na(data$TotalCharges),]
newdata <- data[complete.cases(data),]
nrow(newdata)

# Preprocessing/Cleaning
cols_to_change <- c(10:15)

for(i in 1:ncol(newdata[, cols_to_change])) {
  
  newdata[, cols_to_change][, i] <- as.factor(mapvalues(newdata[, cols_to_change][, i], from = c("No internet service"), to = c("No")))
  
}

newdata$MultipleLines <- as.factor(mapvalues(newdata$MultipleLines, from = c("No phone service"), to = c ("No")))

# Check loop data
prop.table(table(newdata[,10]))
prop.table(table(newdata$MultipleLines))

# Factorize tenure
tenure_fac <- function(x) {
  if (x >= 0 & x <= 12) { return("0-12 Mo")} 
  else if (x > 12 & x <= 24) { return("12-24 Mo") } 
  else if (x > 24 & x <= 36) { return("24-36 Mo") }
  else if (x > 36 & x <= 48) { return("36-48 Mo") }
  else if (x > 48 & x <= 60) { return("48-60 Mo") } 
  else return("> 60 Mo")
}
newdata$tenure_group <- as.factor(sapply(newdata$tenure, tenure_fac))
(table(newdata$tenure_group))

# Separate numerical and categorical variables
num_col <- names(which(sapply(newdata, is.numeric)))
cat_col <- names(which(sapply(newdata, is.factor)))

all_num <- newdata[, num_col]
all_cat <- newdata[, cat_col]

# Check for near zero variance
nzv <- nearZeroVar(all_num, foreach = TRUE, saveMetrics =  TRUE)
nzv
corrs <- cor(all_num, use = "complete.obs")
cor_color <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corrs, method = "color", insig = "blank", sig.level = 0.05, col = cor_color(200))
ggcorr(corrs,
       method = c("pairwise", "spearman"),
       nbreaks = 6,
       hjust = 0.8,
       label = TRUE,
       label_size = 5,
       color = "grey50")

# Effectiveness of variables
# Chi-square test; wonder how well the model reflects the data, how close the observed values are
chisq.test(newdata$Churn, all_cat$gender) # X-squared = 0.47545, df = 1, p-value = 0.4905; highly independent
chisq.test(newdata$Churn, all_num$SeniorCitizen) # X-squared = 158.44, df = 1, p-value < 2.2e-16; highly dependent
chisq.test(newdata$Churn, all_cat$InternetService) # X-squared = 728.7, df = 2, p-value < 2.2e-16; highly dependent
chisq.test(newdata$Churn, all_cat$tenure_group) # X-squared = 881.76, df = 5, p-value < 2.2e-16; highly dependent
chisq.test(newdata$SeniorCitizen, all_cat$InternetService) # X-squared = 493.52, df = 2, p-value < 2.2e-16; highly dependent
chisq.test(newdata$Churn, all_num$MonthlyCharges) # Significant here but not in logistic model ******
chisq.test(newdata$Churn, all_cat$PhoneService) # X-squared = 0.87373, df = 1, p-value = 0.3499; Not sig in log model as well

ggplot(newdata, aes(x = Churn)) +
  geom_density(aes(group = SeniorCitizen, color = SeniorCitizen, fill = SeniorCitizen), alpha = 0.3) # Is this normally distributed?

# Covariance - can tell you if your results are significant overall but not what the differences are
fit_aov <- aov(TotalCharges ~ SeniorCitizen, data = newdata) # One variable
summary(fit_aov)

fit_aov <- aov(TotalCharges ~ PaymentMethod + InternetService, data = newdata) # Two variables (two way anova: compared by 2 or more factors)
summary(fit_aov) # Looks significant, but which treatments differ?

# Check pairwise comparisons; are there significant differences between payment means? Credit Card (auto) looks like the odd one
# After running ANOVA, run HSD to find out which specific group's means are different (look at pvalue)
TukeyHSD(fit_aov)
mosaic::mplot(TukeyHSD(fit_aov), system = "ggplot")

# Pairwise T-Test to reduce chances of incorrectly rejecting Ho; all but Credit Card (automatic) looks significant
pairwise.t.test(newdata$TotalCharges,
                newdata$PaymentMethod, 
                p.adj = "bonferroni",
                paired = FALSE)

# Plots (Just a few)
ggplot(newdata, aes(x = gender)) +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.75, fill = "#2C3E50") +
  theme_tq() +
  labs(x = "Gender",
       y = "Pct",
       title = "Gender Ratio") +
  coord_flip()

ggplot(newdata, aes(x = SeniorCitizen)) +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.75, fill = "#2C3E50") +
  theme_tq() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "SC",
       y = "Pct",
       title = "How Many Senior Citizens Are There?",
       caption = "Roughly 17% of the data are Sr. Citizens") +
  coord_flip()

ggplot(newdata, aes(x = Churn, y = 100*(..count..)/sum(..count..))) +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.75, fill = "#2C3E50") +
  theme_tq() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid( ~ SeniorCitizen) +
  labs(x = "SC",
       y = "Pct",
       title = "How Many Senior Citizens Are There?",
       caption = paste0("If you churned, higher chance you were a senior","\n",
                        "X-squared = 158.44, df = 1, p-value < 2.2e-16","\n",
                        "Chi-square test provides storng evidence to suggest that senior citizens and churns are dependent or have some association."))

# Select variables
rmv <- c("customerID", "tenure")
model_data <- newdata[, !colnames(newdata) %in% rmv]
dim(model_data)

# Modeling - Split data
inTrain <- createDataPartition(model_data$Churn, p = 0.8, list = FALSE)
train <- model_data[inTrain,]
test <- model_data[-inTrain,]
dim(train); dim(test)

prop.table(table(train$Churn))
prop.table(table(test$Churn))

# Logistic Regression
log_model <- glm(Churn ~ .,
                 data = train,
                 family = binomial(link = "logit"))
summary(log_model)

# Tests
anova(log_model, test = "Chisq")
