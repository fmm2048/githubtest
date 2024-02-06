# Full Data Mining Example - Regression 01  
#
# Let's redo the case in workshop 1 and 3 by R
# Readings: http://www.learn-r-the-easy-way.tw/chapters
# Readings: https://www.statmethods.net/r-tutorial/index.html

# Testing Github

###########################################################################
# S1. Data Acquisition ----------------------------------------------------
###########################################################################

# Data sourcing
# Note: you can try setting stringAsFactors=T to see what will happen
data.1A.raw <- read.csv("1A_data.csv", header=T, sep=",", stringsAsFactors = F)
data.1B.raw <- read.csv("1B_data.csv", header=T, sep=",", stringsAsFactors = F)
data.1A <- data.1A.raw
data.1B <- data.1B.raw

###########################################################################
# S2. Data Preparation ----------------------------------------------------
###########################################################################

# Exploratory Analysis ----------------------------------------------------

# For data.1A

# To understand data (exploratory analysis)
summary(data.1A) 	# check the summary to see what to clean  OR
str(data.1A) 		# check the data structure to see what to clean
#factor(data.1A$Test)
#factor(data.1A$Prog.Exp)

# To understand missing data visually (with Amelia or nanair package)
install.packages("Amelia")	# Find and install relevant packages
library(Amelia)			# Load relevant packages
missmap(data.1A)		# Explore the data with relevant commands

# Data Cleansing: 1A -------------------------------------------------------

# Cleaning data - clean problematic data "Manually"
data.1A$Test[data.1A$Test=='AB']=0   			# Replace AB by 0 in Test Col
data.1A$Final.Test[data.1A$Final.Test=='AB']=0		# Replace AB by 0 in Final.Test Col
data.1A$Prog.Exp[data.1A$Prog.Exp=='N']=0 		# Replace N by 0 in Prog.Exp Col
data.1A$Prog.Exp[data.1A$Prog.Exp=='']=NaN 		# '' --> NaN or 0 based on yr judgement
data.1A$Prog.Exp[data.1A$Prog.Exp=='Never']=0		# Never --> 0 
data.1A$Prog.Exp[data.1A$Prog.Exp=='Y']= mean(data.1A$Prog.Exp, na.rm=TRUE)  # Y --> mean(Prog.Exp)

# Converting columns to correct datatype for processing
str(data.1A)
data.1A$Prog.Exp = as.numeric(as.character(data.1A$Prog.Exp)) # Convert factor/Char to Numeric
data.1A$Test = as.numeric(as.character(data.1A$Test)) # Convert factor/Char to Numeric
data.1A$Final.Test = as.numeric(as.character(data.1A$Final.Test)) # Convert factor/Char to Numeric
data.1A$att = as.numeric(data.1A$att) # Convert factor/Char to Numeric
str(data.1A)

# Cleaning data - by Packages
# Some popular packages include Hmisc (Harrell Miscellaneous), mice or Tidyverse
# Missing Data Treatment - For numerical data: Mean; For non-numerical data: Mode

install.packages("Hmisc")
require(Hmisc) # for impute function
data.1A$Prog.Exp = impute(data.1A$Prog.Exp, mean)  # replace NAs with mean/median/a fixed no., say 3
data.1A$X5SP = impute(data.1A$X5SP, mean)  # replace NAs with mean
missmap(data.1A)
View(data.1A) # To view data as a table

# Delete unwanted cases if necessary - MAY AFFECT FINAL RESULT !!
data.1A = data.1A[which(!(data.1A$CA==0)),] # To delete the row with CA=0 (M1) - early dereg cases
data.1A = subset(data.1A, CA>0, ) # To delete the row with CA=0 (M2)
missmap(data.1A)

# Add two new columns to specify the class and P/F
data.1A$class = 'A'
data.1A$clabel[data.1A$Total>=40]="P"
data.1A$clabel[data.1A$Total<40]="F"

# Check the NA map again, if okay can proceed to class 1B
missmap(data.1A)


# Data Cleaning: Class 1B -------------------------------------------------
# This time we can speed up by writing our own functions and using packages

# understand data
str(data.1B)
missmap(data.1B)

# You can write your own functions to process data like this:
source("clean.r")
old = c('Never','AB', 'N', '') ; new = c(0,0,0,NaN)
data.1B = clean.v2(data.1B, old, new)

# Converting to correct datatype for processing
data.1B$Test = as.numeric(as.character(data.1B$Test)) # Convert factor/Char to Numeric
data.1B$Final.Test = as.numeric(as.character(data.1B$Final.Test)) # Convert factor/Char to Numeric
data.1B$Prog.Exp = as.numeric(as.character(data.1B$Prog.Exp)) # Convert factor/Char to Numeric
data.1B$att = as.numeric(data.1B$att) # Convert factor/Char to Numeric

# Fill Missing Values as appropriate
data.1B$Prog.Exp = impute(data.1B$Prog.Exp, mean)  # replace with mean OR
data.1B$X5SP = impute(data.1B$X5SP, mean)  # replace with mean OR

# Delete unwanted cases if necessary - MAY AFFECT FINAL RESULT !!
# data.1B = data.1A[which(!(data.1B$CA==0)),] # To delete the row with CA=0 (M1)
data.1B = subset(data.1B, CA>0, ) # To delete the row with CA=0 (M2)

# Add two new columns to specify the class and P/F
data.1B$class = 'B'
data.1B$clabel[data.1B$Total>=40]="P"
data.1B$clabel[data.1B$Total<40]="F"
missmap(data.1B)

# Combining Data: ---------------------------------------------------------

data.all = rbind(data.1A,data.1B)


###########################################################################
# S3. Data Modeling -----------------------------------------------------------
###########################################################################

# Modeling

# Scaling the dataset (optional)
# Scale/Normalize the data to reduce problems caused by absolute difference of factors
# data.scaled<-as.data.frame(sapply(data.all[2:10],scale)) # (M1) for numerical columns only
# data.scaled <- as.data.frame( scale(data.all[2:10] )) # (M2) for numerical columns only
data.scaled = data.all

# Split training and testing datasets
set.seed(4567)  # Set Random seed
train <- sample(nrow(data.scaled), 0.7*nrow(data.scaled))
data.train <- data.scaled[train,]  # Randdom Sampled 70%
data.test  <- data.scaled[-train,]  # Remaining 30%

# Build LR Model with different paramters (x) vs Total (y) and Show Model
# model.lm <- lm(Total ~ X5SP+att, data = data.train)
model.lm <- lm(Total ~ Test+ASG, data = data.train)
summary(model.lm)
print(model.lm)

# K-fold cross-validation
# install.packages("DAAG")
# library(DAAG)
# CV.lm = CVlm(data.train, form.lm = formula(Total ~ X5SP+att), m=3, plotit = FALSE) # 3 fold cross-validation
# CV.lm = CVlm(data.train, form.lm = formula(Total ~ ASG+Test), m=3, plotit = FALSE) # 3 fold cross-validation
# CVResults = CVlm(data.train, model.lm, m=2, plotit = FALSE) # 3 fold cross-validation
# The model with smaller the Mean Square Error is the better

###########################################################################
# S4 Evaluation --------------------------------------------------------------
###########################################################################

# Prediction
prediction <- predict(model.lm, data.test)
print(prediction)

# Add a new column for class label (clabel)
prediction <- as.data.frame(prediction)
prediction$clabel[prediction$prediction>=40]="P"
prediction$clabel[prediction$prediction<40]="F"

# Tally the confusion matrix input
cfm = table(prediction$clabel, data.test$clabel)
print(cfm)

#install.packages("caret")
#install.packages("e1071")
#require("caret")
#require("e1071")
confusionMatrix(cfm, positive="P")
