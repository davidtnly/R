# Introduction

# Goal: Use new ML models: kNN, SVM, xgb

# Load Libraries
library(tidyverse)
library(Hmisc)
library(knitr)
library(gridExtra)

# Load data
train <- read.csv('Titanic/train.csv', stringsAsFactors = FALSE)
test <- read.csv('Titanic/test.csv', stringsAsFactors = FALSE)

# Test # Create Surname variable
# head(data$Name,1)
# head(substring(data$Name, 0, regexpr(',', data$Name)),1)
# head(substring(data$Name, 0, regexpr(',', data$Name)-1),1)
# head(data$Surname)
# paste("original: ",head(train$Ticket,1))
# paste("first letter by '.': ", head(sub(".", "X", train$Ticket), 1))
# paste("question mark does nothing: ", head(sub("?", "X", train$Ticket), 1))
# paste("use '.?':", head(sub(".$", "X", train$Ticket), 1))
# 

# Engineer GroupId for woman-child groups
test$Survived <- NA; data <- rbind(train,test)
data$Surname = substring( data$Name,0,regexpr(',',data$Name)-1)
data$GroupId = paste( data$Surname, data$Pclass, sub('.$','X',data$Ticket), data$Fare, data$Embarked, sep='-')
data[c(195,1067,59,473,1142),c('Name','GroupId')]

# Identify Nannies and Relatives traveling in woman-child-groups
data$Title <- 'man'
data$Title[data$Sex=='female'] <- 'woman'
data$Title[grep('Master',data$Name)] <- 'boy'
# color variable is used in plots below
data$Color <- data$Survived
# engineer "woman-child-groups"
data$GroupId[data$Title=='man'] <- 'noGroup'
data$GroupFreq <- ave(1:1309,data$GroupId,FUN=length)
data$GroupId[data$GroupFreq<=1] <- 'noGroup'
data$TicketId = paste( data$Pclass,sub('.$','X',data$Ticket),data$Fare,data$Embarked,sep='-')
count = 0
# add nannies and relatives to groups
for (i in which(data$Title!='man' & data$GroupId=='noGroup')){
  data$GroupId[i] = data$GroupId[data$TicketId==data$TicketId[i]][1]
  if (data$GroupId[i]!='noGroup') {
    # color variable is used in plots below
    if (is.na(data$Survived[i])) data$Color[i] = 5
    else if (data$Survived[i] == 0) data$Color[i] = -1
    else if (data$Survived[i] == 1) data$Color[i] = 2
    count = count + 1
  }
}
cat(sprintf('We found %d nannies/relatives and added them to groups.\n',count))

# Classify unknown woman-child groups
data$GroupName = substring( data$GroupId,0,regexpr('-',data$GroupId)-1)
data$Color[is.na(data$Color) & data$Title=='woman'] <- 3
data$Color[is.na(data$Color) & data$Title=='boy'] <- 4
x = data$GroupId[data$GroupId!='noGroup']; x = unique(x); x=x[order(x)]
plotData <- list(); g <- list()
for (i in 1:3) plotData[[i]] <- data[data$GroupId %in% x[(27*(i-1))+1:27],]
for (i in 1:3) g[[i]] = ggplot(data=plotData[[i]], aes(x=0,y=factor(GroupName))) +
  geom_dotplot(dotsize=0.9,binwidth=1,binaxis='y',method="histodot",stackgroups=T,
               aes(fill=factor(Color),color=Title )) +
  scale_color_manual(values=c('gray70','blue','gray70'),limits=c('man','boy','woman')) +
  scale_fill_manual(values=c('#BB0000','#FF0000','#009900','#00EE00','gray70','gray70','white'),
                    limits=c('0','-1','1','2','3','4','5')) +
  scale_y_discrete(limits = rev(levels(factor(plotData[[i]]$GroupName)))) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        legend.position='none')
grid.arrange(g[[1]],g[[2]],g[[3]],nrow=1,top='All 80 woman-child-groups in the test and training datasets combined (228 passengers).
Red = deceased female or boy, Green = survived, White or Gray = unknown survival, 
White or LightGreen or LightRed = different surname same ticket, Blue outline = boy')

# Explore which woman-child groups live or die
data$Survived <- factor(data$Survived)
data$CabinLetter <- substring(data$Cabin,0,1)
g1 = ggplot(data=data[data$GroupId!='noGroup' & !is.na(data$Survived),]) +
  geom_bar(stat='count',aes(x=Pclass,fill=Survived))
g2 = ggplot(data=data[data$GroupId!='noGroup' & !is.na(data$Survived) & !is.na(data$Age),]) +
  geom_histogram(bins=20,aes(x=Age,fill=Survived))
g3 = ggplot(data=data[data$GroupId!='noGroup' & !is.na(data$Survived),]) +
  geom_bar(stat='count',aes(x=Embarked,fill=Survived))
g4 = ggplot(data=data[data$GroupId!='noGroup' & !is.na(data$Survived),]) +
  geom_bar(stat='count',aes(x=CabinLetter,fill=Survived))
grid.arrange(g1,g2,g3,g4,nrow=2,top='Analysis of training set\'s 156 Woman-Child-Group passengers')

## From this analysis, we see that woman-child-groups traveling in Pclass=1 or 2 mostly 
## survived and woman-child-groups in Pclass=3 mostly died. The Gibsons had Pclass=1, 
## Embarked=C, and no Cabin, therefore we should predict that the Gibsons survived. 
## The Klasen's, Peacock's, and van Billiards had Pclass=3, Embarked=S, and no Cabin. 
## Therefore we should predict that those three families perished.

# Cross-validation unknown groups on training dataset
train$Title <- 'man'
train$Title[train$Sex=='female'] <- 'woman'
train$Title[grep('Master',train$Name)] <- 'boy'
# Perform 25 trials of 10-fold cross validation
trials = 25; sum = 0
for (j in 1:trials){
  x = sample(1:890); s = 0
  for (i in 0:9){
    # engineer "woman-child-groups"
    train$Surname <- substring(train$Name,0,regexpr(",",train$Name)-1)
    train$GroupId = paste( train$Surname, train$Pclass,sub('.$','X',train$Ticket),train$Fare,train$Embarked,sep='-')
    train$GroupId[train$Title=='man'] <- 'noGroup'
    train$GroupFreq <- ave(1:891,train$GroupId,FUN=length)
    train$GroupId[train$GroupFreq<=1] <- 'noGroup'
    # add nannies and relatives to groups.
    train$TicketId = paste( train$Pclass,sub('.$','X',train$Ticket),train$Fare,train$Embarked,sep='-')
    for (k in which(train$Title!='man' & train$GroupId=='noGroup'))
      train$GroupId[k] = train$GroupId[train$TicketId==train$TicketId[k] & train$PassengerId != train$PassengerId[k]][1]
    train$GroupId[is.na(train$GroupId)] <- 'noGroup'
    train$GroupFreq <- ave(1:891,train$GroupId,FUN=length)
    # calculate training subset's group survival rate
    train$GroupSurvival <- NA
    train$GroupSurvival[-x[1:89+i*89]] <- ave(train$Survived[-x[1:89+i*89]],train$GroupId[-x[1:89+i*89]])
    # calculate testing subset's group survival rate from training set's rate
    for (k in x[1:89+i*89]){ 
      train$GroupSurvival[k] <- train$GroupSurvival[which(!is.na(train$GroupSurvival) & train$GroupId==train$GroupId[k])[1]]
      if (is.na(train$GroupSurvival[k])) train$GroupSurvival[k] <- ifelse(train$Pclass[k]==3,0,1)
    }
    # apply gender model plus WCG
    train$predict <- 0
    train$predict[train$Title=='woman'] <- 1
    train$predict[train$Title=='boy' & train$GroupSurvival==1] <- 1
    train$predict[train$Title=='woman' & train$GroupSurvival==0] <- 0
    c = sum(abs(train$predict[x[1:89+i*89]] - train$Survived[x[1:89+i*89]]))
    s = s + c
  }
  #cat( sprintf("Trial %d has 10-fold CV accuracy = %f\n",j,1-s/890))
  sum = sum + 1-s/890
}
cat(sprintf("Average 10-fold CV accuracy from %d trials = %f\n",trials,sum/trials))

# Make Predictions
# Remmeber 2 prediction rules:
# 1) If an unknown passenger's wc group all live or died predict same
# 2) ... has no members in the training dataset, use Pclass to predict survival
# Pclass 3 = die, P/2 = live

data$GroupSurvival <- NA
data$Survived <- as.numeric(as.character(data$Survived))
data$GroupSurvival[1:891] <- ave(data$Survived[1:891],data$GroupId[1:891])
for (i in 892:1309) data$GroupSurvival[i] <- data$GroupSurvival[which(data$GroupId==data$GroupId[i])[1]]
data$GroupSurvival[is.na(data$GroupSurvival) & data$Pclass==3] <- 0
data$GroupSurvival[is.na(data$GroupSurvival) & data$Pclass!=3] <- 1
data$Predict <- 0
data$Predict[data$Sex=='female'] <- 1
data$Predict[data$Title=='woman' & data$GroupSurvival==0] <- 0
data$Predict[data$Title=='boy' & data$GroupSurvival==1] <- 1

cat('The following 8 males are predicted to live\n')
data[data$Sex=='male' & data$Predict==1 & data$PassengerId>891,c('Name','Title')]
cat('The following 14 females are predicted to die\n')
data[data$Sex=='female' & data$Predict==0 & data$PassengerId>891,c('Name','Title')]
cat('The remaining 258 males are predicted to die\n')
cat('and the remaining 138 females are predicted to live\n')

# submit <- data.frame(PassengerId = 892:1309, Survived = data$Predict[892:1309])
# write.csv(submit,'survivalpredictions.csv',row.names=F)

########## Part B: Non-WC Group Passengers















