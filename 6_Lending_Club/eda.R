########### Load ###########
############################

# Load libraries
suppressPackageStartupMessages(source("libraries.R")) # frequently used libraries
source("functions.R") # frequently used functions: check_na, details, convert_date
theme_set(theme_bw()) # theme_tq() "#233140"

# Set paths
mainpath <- "/R Projects/LendingClub/"

# Load data
data <- read.csv("data.csv", stringsAsFactors = FALSE)

# Structure
details(data)
check_na(data)

########### Exploratory ###########
###################################

# Create table for unique data
meta_data <- df_status(data, print_results = FALSE)

unique_df <- meta_data %>% 
  mutate(unique_ratio = (unique)/nrow(data))

unique_df %>% 
  select(variable, unique, unique_ratio) %>% 
  mutate(unique = unique, unique_ratio = scales::percent(unique_ratio)) %>% 
  kable()

# Remove NA loans without annual_inc
data <- data[!is.na(data$annual_inc), ]

# Remove variables with high % of NA values
na_list <- c("mths_since_last_delinq", "mths_since_last_record", "mths_since_last_major_derog",
             "annual_inc_joint", "dti_joint", "open_acc_6m", "open_il_6m", "open_il_12m",
             "open_il_24m", "mths_since_rcnt_il", "total_bal_il", "il_util", "open_rv_12m",
             "open_rv_24m", "max_bal_bc", "all_util", "inq_fi", "total_cu_tl", "inq_last_12m")

data <- data %>% 
  select(-one_of(na_list))

# Update new default feature
default <- c("Default", "|Does not meet the credit policy. Status:Charged Off",
             "In Grace Period", "Late (16-30 days)", "Late (31-120 days)")

data <- data %>%
  mutate(defaulted = ifelse(!(loan_status %in% default), 0, 1))

# Split between numeric / categorical variables
num_col <- names(data[, sapply(data, class) %in% c("integer", "numeric")])
cat_col <- names(data[, sapply(data, class) %in% c("character", "factor")])

num_data <- data[, num_col]
cat_data <- data[, cat_col]

# Check for near zero variance
nzv <- nearZeroVar(num_data[, !(colnames(num_data) == "defaulted")], foreach = TRUE, saveMetrics = TRUE, allowParallel = TRUE)
nzv

# Remove TRUE nzv features
num_data <- num_data[!nzv$nzv]

# Plot numerical variables
corrs <- cor(num_data, use = "complete.obs")

# Matrix of correlation (p-values)
res <- rcorr(as.matrix(corrs))
p <- res$P

corrplot(corrs,
         orders = "hclust",
         addgrid.col = "black",
         sig.level = 0.05,
         outline = TRUE,
         addrect = 4,
         rec_lwd = 5,
         number.digits = 2,
         number.cex = 0.75,
         col = colorRampPalette(c("darkred", "white","midnightblue"))(100),
         title = "Numeric Feature Correlations"
         )

# Plot
ggcorrplot::ggcorrplot(corrs, 
           hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method = "circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title = "Correlogram of Features",
           ggtheme = theme_bw
           )

# Test out features that have high correlations; ANOVA to decide best feature (if categorical)

# Test out features using cor.test()
cor.test(num_data$funded_amnt, num_data$loan_amnt, method = "pearson")

# ANOVA for highly correlated features; determining whether the means from more than two populations or groups are equal or not
summary(aov(num_data$defaulted ~ cat_data$sub_grade + cat_data$grade, test = "chisq"))

### Plot variables ###
options(scipen = 999)

# Defaulted
data %>% 
  filter(defaulted == 1) %>% 
  ggplot(aes(x = grade, y = (..count..)/sum(..count..), fill = grade)) +
  geom_bar(fill = "#233140") +
  geom_text(stat = 'count', aes(label = paste0(round((..count..)/sum(..count..), 2)*100,"%"), vjust = 1, size = 3), color = "white") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Grade",
       y = "Percentage of Defaults",
       title = "Percentage of Defaulted Loans by Grade",
       caption = "Default rates are pretty high as grades drop") +
  theme(plot.title = element_text(hjust = 0.5, color = "#233140"),
        axis.line = element_line(color = "#233140"),
        axis.text = element_text(color = "#233140"),
        axis.title = element_text(color = "#233140"),
        legend.position = "none"
        )

# Non-defaulted
data %>% 
  filter(defaulted == 0) %>% 
  ggplot(aes(x = grade, y = (..count..)/sum(..count..), fill = grade)) +
  geom_bar(fill = "#233140") +
  geom_text(stat = 'count', aes(label = paste0(round((..count..)/sum(..count..), 2)*100,"%"), vjust = 1, size = 3), color = "white") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Grade",
       y = "Percentage of Non-Defaults",
       title = "Percentage of Non-Defaulted Loans by Grade",
       caption = "Non-Default percentage drops as grades gets worse") +
  theme(plot.title = element_text(hjust = 0.5, color = "#233140"),
        axis.line = element_line(color = "#233140"),
        axis.text = element_text(color = "#233140"),
        axis.title = element_text(color = "#233140"),
        legend.position = "none"
  )


# Home Ownership
data$home_ownership = ifelse(data$home_ownership == "MORTGAGE", "MORTGAGE",
                        ifelse(data$home_ownership == "OWN", "OWN",
                               ifelse(data$home_ownership == "RENT", "RENT", "OTHERS")))
data %>% 
  group_by(home_ownership, defaulted) %>% 
  summarise(n = n()) %>% 
  spread(key = defaulted, value = n) %>% 
  mutate(default_ratio = `1`/(`1` + `0`)) %>%
  ggplot(aes(x = reorder(home_ownership, default_ratio), y = default_ratio, fill = home_ownership)) +
  geom_bar(stat = "identity", fill = "#233140") +
  geom_text(aes(label = paste0(round(default_ratio,3)*100,"%"), vjust = 1, size = 3), color = "white") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Home Ownership",
       y = "Percentage of Ownership",
       title = "Home Ownership Risk",
       caption = "OTHERS bucket is nonexistent as there is a 0% default rate") +
  theme(plot.title = element_text(hjust = 0.5, color = "#233140"),
        axis.line = element_line(color = "#233140"),
        axis.text = element_text(color = "#233140"),
        axis.title = element_text(color = "#233140"),
        legend.position = "none"
  )

# Grade vs Int
data %>% 
  ggplot(aes(x = grade, y = int_rate, fill = grade)) +
  geom_boxplot(show.legend = F) +
  facet_grid(~ defaulted) +
  labs(x = "Grade",
       y = "Interest Rate",
       title = "Loan Grades vs. Interest Rates",
       subtitle = "Expect interest rates to rise as loans get worse",
       caption = "Spread is also higher as grades lower") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        strip.background = element_rect(fill = "#233140"),
        strip.text = element_text(color = "white"),
        axis.text = element_text(color = "#233140"),
        axis.title = element_text(color = "#233140")
        )

# Grade vs Int vs Term (Most loans are 36 months and majority of the better loans are shorter term)
data %>% 
  ggplot(aes(x = grade, y = int_rate, fill = grade)) +
  geom_boxplot(show.legend = F) +
  facet_grid(~ term) +
  labs(x = "Grade",
       y = "Interest Rate",
       title = "Loan Grades vs. Interest Rates",
       subtitle = "Expect interest rates to rise as loans get worse",
       caption = "Spread is also higher as grades lower") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        strip.background = element_rect(fill = "#233140"),
        strip.text = element_text(color = "white"),
        axis.text = element_text(color = "#233140"),
        axis.title = element_text(color = "#233140")
  )

# Sub Grade
data %>%
  group_by(sub_grade) %>%
  select(sub_grade, int_rate) %>% 
  summarise(n = n(), mean_int = mean(int_rate)) %>% 
  arrange(desc(mean_int)) %>% 
  ggplot(aes(x = reorder(sub_grade, mean_int), y = mean_int, fill = mean_int)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(mean_int,1),"%")), hjust = 0) +
  coord_flip() +
  scale_fill_continuous(low = "navy", high = "firebrick1") +
  labs(x = "Sub-Grade",
     y = "Mean IR",
     title = "Sub-Grades vs. Mean Interest Rate",
     caption = "Highest interest rates gradually increase linearly until grade C where it mixes up a bit") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        strip.background = element_rect(fill = "#233140"),
        strip.text = element_text(color = "white"),
        axis.text = element_text(color = "#233140"),
        axis.title = element_text(color = "#233140"))

# Annual Income (Jitter less data because of size)
data %>% 
  top_n(1000) %>% 
  filter(annual_inc < 5000000) %>% 
  ggplot(aes(x = grade, y = annual_inc)) +
  geom_jitter(alpha = 0.20, color = "#233140", fill = "#233140") +
  geom_boxplot(position = "dodge", outlier.color = "red", outlier.shape = 1) +
  labs(x = "Grade",
       y = "Annual Income",
       title = "Annual Income Bracket by Grade",
       subtitle = "Income density lowers as grade value drops") +
  theme(plot.title = element_text(hjust = 0.5, color = "#233140"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "#233140"),
        strip.background = element_rect(fill = "#233140"),
        strip.text = element_text(color = "white"),
        axis.text = element_text(color = "#233140"),
        axis.title = element_text(color = "#233140")
        ) +
  ggforce::facet_zoom(y = annual_inc < 500000 & annual_inc > 50000 , zoom.size = 1.2)

# Verification Status (Default rate is pretty low but once we plot it, it shows a slight increase when not verified)
data %>% 
  group_by(verification_status, defaulted) %>% 
  summarise(n = n()) %>% 
  spread(key = defaulted, value = n) %>% 
  mutate(Verification_Default_Rate = `1`/`1`/(`1` + `0`)) %>% 
  ggplot(aes(x = reorder(verification_status, Verification_Default_Rate), y = Verification_Default_Rate, fill = verification_status)) +
  geom_bar(stat = "identity", fill = "#233140") +
  geom_text(aes(label = paste0(round(Verification_Default_Rate,8)*100,"%"), vjust = 1, size = 3), color = "white") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Verification Status",
       y = "Percentage",
       title = "Verification Status Risk",
       subtitle = "Verification risk is higher when it's not verified") +
  theme(plot.title = element_text(hjust = 0.5, color = "#233140"),
        plot.subtitle = element_text(hjust = 0.5, color = "#233140", size = 10),
        axis.line = element_line(color = "#233140"),
        axis.text = element_text(color = "#233140"),
        axis.title = element_text(color = "#233140"),
        legend.position = "none")

# Loan Purpose
data %>% 
  count(purpose, defaulted) %>% 
  ggplot(aes(x = reorder(purpose, n, sum), y = n, fill = defaulted)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_color_manual(breaks = c(0,1),
                     labels = c("No", "Yes"),
                     values = c("#233140", "red")) +
  facet_wrap(~ defaulted, scales = "free") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        strip.background = element_rect(fill = "#233140"),
        strip.text = element_text(color = "white"),
        axis.text = element_text(color = "#233140"),
        axis.title = element_text(color = "#233140"))

# Grade & Loan Amount (Loan Amount is higher as loan grade drops)
data %>%
  ggplot(aes(x = grade, y = loan_amnt, fill = grade)) +
  geom_boxplot(outlier.colour = "red") +
  facet_grid(~ defaulted) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        strip.background = element_rect(fill = "#233140"),
        strip.text = element_text(color = "white"),
        axis.text = element_text(color = "#233140"),
        axis.title = element_text(color = "#233140"),
        legend.position = "none")

# Ant plot
data[1:30000,] %>% 
  ggplot(aes(x = grade, y = loan_amnt)) +
  geom_boxplot(aes(fill = grade), outlier.colour = "red") +
  geom_jitter(aes(fill = grade, color = defaulted), alpha = 0.50) +
  # facet_grid(~ defaulted) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        strip.background = element_rect(fill = "#233140"),
        strip.text = element_text(color = "white"),
        axis.text = element_text(color = "#233140"),
        axis.title = element_text(color = "#233140"),
        legend.position = "none") 

# Loss % per term
data[data$defaulted == 1,] %>% 
  select(total_pymnt, funded_amnt, term) %>%
  group_by(term) %>%
  summarise(total_pay = sum(total_pymnt), funded = sum(funded_amnt)) %>%
  mutate(loss.perc = 100*(total_pay - funded)/funded)

# Remove more variables
nzv
rmv_vars <- c("id", "member_id", "out_prncp", "out_prncp_inv", "revol_bal", "total_pymnt", "total_pymnt_inv",
              "total_rec_prncp", "total_rec_int", "last_pymnt_amnt", "tot_cur_bal", "total_rec_late_fee",
              "recoveries", "collection_recovery_fee", "policy_code", "acc_now_delinq", "tot_coll_amt", "total_rev_hi_lim",
              "title")

data <- data %>% 
  select(-one_of(rmv_vars))

# Split between numeric / categorical variables
num_col <- names(data[, sapply(data, class) %in% c("integer", "numeric")])
cat_col <- names(data[, sapply(data, class) %in% c("character", "factor")])

num_data <- data[, num_col]
cat_data <- data[, cat_col]

# Check for near zero variance
nzv <- nearZeroVar(num_data[, !(colnames(num_data) == "defaulted")], foreach = TRUE, saveMetrics = TRUE, allowParallel = TRUE)
nzv

# Remove TRUE nzv features
num_data <- num_data[!nzv$nzv]

### Geo Plot
def <- sum(ifelse(data$defaulted == 1,1,0))

st <- data %>% 
        group_by(addr_state) %>% 
        summarise(mean_amnt = mean(loan_amnt))
names(st) <- c("state.abb", "Mean_Amnt")

# Add state column
st$states <- tolower(state.name[match(st$state.abb, state.abb)])

# Convert to a SpatialPolygon
country_map <- map('state', fill = TRUE, plot = FALSE)
nms <- sapply(strsplit(country_map$names, ":"), function (x) x[1])
polys <- maptools::maps2SpatialPolygons(country_map, IDs = nms, CRS('+proj=longlat'))

data %>% 
  # filter(defaulted == 1) %>% 
  group_by(addr_state, defaulted) %>% 
  summarise(n = n()) %>% 
  spread(key = defaulted, value = n) %>% 
  mutate(Default_Pct = `1`/(`1`+`0`)) %>% 
  arrange(desc(Default_Pct)) %>% 
  `colnames<-` (c("state", "No_Default", "Defaulted", "Default_Pct")) %>% 
  left_join(st)
  






########### Feature Engineering ###########
###########################################

check_na(data)


