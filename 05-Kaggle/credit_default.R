## Introduction

# Section 1: Process the Data

### 1.1 Loading libraries and data into R
paste0("Loading libraries...")

library(tidyverse)
library(plotly)
library(GGally)
library(knitr)
library(gridExtra)
library(corrplot)
library(RColorBrewer)

#---------------------------
paste0("Loading data...")
train <- read.csv("~/R Projects/KaggleData/CreditDefault/application_train.csv", stringsAsFactors = FALSE)
test <- read.csv("~/R Projects/KaggleData/CreditDefault/application_test.csv", stringsAsFactors = FALSE)
bureau <- read.csv("~/R Projects/KaggleData/CreditDefault/bureau.csv", stringsAsFactors = FALSE)
cc_balance <- read.csv("~/R Projects/KaggleData/CreditDefault/credit_card_balance.csv", stringsAsFactors = FALSE)
prev <- read.csv("~/R Projects/KaggleData/CreditDefault/previous_application.csv", stringsAsFactors = FALSE)

### 1.2 Data Size & Structures of Training Set
dim(train)
dim(test)
dim(bureau)
dim(cc_balance)
dim(prev)

### 1.3 Check for Missing Values, Errors, Corrupted Values
setdiff(names(train), names(test)) # TARGET is the missing variable (Response)
sort(colSums(sapply(train, is.na)), decreasing = TRUE) #sapply(train, function(x) {sum(is.na(x))})
# colSums(is.na(train))

### 1.4 Combine train and test Datasets
test$TARGET <- NA
all <- rbind(train, test) # 1:307511 for test data OR all[,!is.na(all$TARGET)] 

# Section 2: Exploratory Data Analysis

### 2.1 Custom Theme (Practice theme() arguments)

theme_custom <- function(base_size = 11, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.title =  element_text(face = "bold"), 
          axis.text.x = element_text(colour = "red", face = "bold"),
          axis.text.y = element_text(colour = "red", face = "bold"),
          panel.border = element_rect(colour = "white", fill = "white"),
          panel.background = element_rect(colour = "white", fill = "white"),
          panel.grid.major =  element_line(colour = "white"),
          panel.grid.minor = element_line(colour = "white"),
          plot.background = element_rect(colour = "lightgray", fill = "lightgray"),
          plot.title = element_text(hjust = 0.5, size = "12", face = "bold", lineheight = 0.5),
          plot.subtitle = element_text(hjust = 0.5, lineheight = 0.5, face = "italic")
          # legend
    )
}

### 2.2 Graphing & Summaries
paste0("Let the graphing begin...")

# TARGET is a binary variable and is highly imbalanced with most values in 0.
train %>% 
  count(TARGET) %>% 
  kable()

# GGally examples in graphing (Correlations)
names(head((train[, sapply(train, is.numeric)]))) # 106/122 columns are numeric

# Split numeric and categorical features (another way shown later)
train_split <- train[1:10000,]
num_features <- names(which(sapply(train_split, is.numeric)))
cat_features <- names(which(sapply(train_split, is_character)))
trainsplit_numeric <- train_split[, names(train_split) %in% num_features]
trainsplit_cat <- train_split[, names(train_split) %in% cat_features]
all_numeric_nonNA <- trainsplit_numeric[, !colnames(trainsplit_numeric) == "TARGET"]
all_cat_nonNA <- trainsplit_cat[, !colnames(trainsplit_cat) == "TARGET"]

# Create new df without NA values
col_NA <- (colSums(is.na(all_numeric_nonNA)) > 0) 
t <- na.omit(all_numeric_nonNA) # Omit rows of NA data

# Produce correlation charts and variations
correlations <- cor(t, use = "everything")
corrplot(correlations[1:10, 1:10], method = "shade",
         type = "upper",
         # upper = "number",
         # lower = "shade",
         tl.cex = 0.50,
         tl.srt = 45,
         number.cex = 0.70, # Size for values inside method
         tl.col = "black",
         addCoef.col = "black", # Add coefficient of correlation
         # addCoefasPercent =  TRUE,
         order = "hclust", # for hierarchical clustering order
         na.label = "na", # not needed if omitted na values aleady
         # na.label.col = "white",
         # p.mat = p.mat,
         sig.level = 0.01,
         insig = "blank", # insignificant correlation are left blank
         outline = TRUE,
         diag = T,
         # tl.pos = "y",
         col = colorRampPalette(c("darkred","white","midnightblue"))(100))

corrplot.mixed(correlations[1:10, 1:10],
               lower = "shade",
               upper = "number",
               number.cex = 0.75,
               tl.cex = 0.5,
               tl.srt = 45,
               addgrid.col = "white",
               bg = "black"
               # col = colorRampPalette(c("darkred","black","midnightblue"))(100)
               )
# plot.ly is a nice interactive way to play with charts in R (https://plot.ly/r/pie-charts/)
d <- train %>% 
  count(TARGET) %>% 
  plot_ly(labels = ~TARGET, values = ~n,
          # type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          hoverinfo = 'text',
          text = ~paste("Type:", TARGET),
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1))
  ) %>% # Creates an interative plot
  add_pie(hole = 0.5) %>%  # Adds a donut chart
  layout(title = "Target Distribution (RV)",
         showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
  )
d
## Create a reproducible graph in plot.ly
# chart_link <- api_create(d, filename = "Sample Target Distribution")
# chart_link

train %>% 
  count(CODE_GENDER) %>% 
  plot_ly(labels = ~CODE_GENDER,
          values = ~n,
          textposition = "inside",
          textinfo = "label+percent",
          insidetextfont = list(color = "FFFFF"),
          hoverinfo = "text",
          text = ~paste("Gender:", CODE_GENDER),
          marker = list(colors = colors,
                        line = list(color = "#FFFFFF", width = 1))
          ) %>% 
  add_pie(hole = 0.50) %>% 
  layout(title = "Gender Distribution",
         showlegend = TRUE,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

short_df <- all_cat_nonNA[,1:3]

# Occupation Type - Bar
train %>% 
  mutate(TARGET = as.factor(TARGET)) %>% 
  count(OCCUPATION_TYPE, TARGET) %>% 
  na.omit() %>% 
  plot_ly(x = ~OCCUPATION_TYPE,
          y = ~n,
          color = ~TARGET,
          type = "bar") %>% 
  layout(title = "Occupation Type by Group",
         barmode = "group",
         # xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         # yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
         xaxis = list(title = ""),
         yaxis = list(title = ""))



## Problem: More intermediate practice: Create a loop through categorical variables and graph

ggpairs(data = train_split,
        columns = 1:5,
        title = "Correlation Matrix",
        mapping = aes(colour = CODE_GENDER),
        lower = list(
          continuous = "smooth",
          combo = "facetdensity",
          mapping = aes(colour = CODE_GENDER)),
        upper = list(
          continuous = wrap("cor", size = 3, hjust = 0.8))
        )

## More graphing practice (ggpairs) - group by TARGET

train_split[, sapply(train_split, is.numeric)] %>% # correlation needs numeric only
  na.omit() %>% # remove any NA values so NA does not show
  select(TARGET,1:5) %>% # select TARGET & compared columns
  mutate(TARGET = factor(TARGET)) %>% 
  ggpairs(aes(col = TARGET, alpha = 0.5))

# Can see that there are some columns have high correlation

train_split[, sapply(train_split, is.numeric)] %>% # correlation needs numeric only
  na.omit() %>% # remove any NA values so NA does not show
  select(TARGET,5:10) %>% # select TARGET & compared columns
  mutate(TARGET = factor(TARGET)) %>% 
  ggpairs(aes(col = TARGET, alpha = 0.5))

# Another way to split numeric & categorical data
char_var <- all[, sapply(all, is.character)]
num_var <- all[, sapply(all, is.numeric)]
char_var[is.na(char_var)] <- "Not Available"

fac_var <- char_var %>% 
  lapply(as.factor) %>% 
  as_data_frame()

all_new <- bind_cols(fac_var, num_var)
# Optional: rm(char_var, num_var, fac_var) # Memory space
all_new[is.na(all_new)] <- 0 # Replace NA values with zero (numeric)

fn <- funs(mean, sd, min, max, sum, n_distinct, .args = list(na.rm = TRUE))

sum_cc_balance <- cc_balance %>% 
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)

head(sum_cc_balance)
