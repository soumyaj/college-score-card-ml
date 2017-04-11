# Library imports
library(MASS)
library(plyr)
library(dplyr)
library(tidyr)

### PREPROCESSING 
# 1. Import data set
# 2. Handle Privacy Suppressed and NULL values
# 3. Change continuous response variable into categorical variable
#college_data <- read.csv("~/workspace/ml/project/Most-Recent-Cohorts-Treasury-Elements.csv",header = TRUE)

# How many NAs in the dataset? 
sapply(college_data, function(x) sum(is.na(x)))
# Remove NAs - doesnt remove anything yet!!
college_data <- college_data %<>% na.omit()
college_data <- Filter(function(x)!all(is.na(x)), college_data)

college_subset <- college_data[complete.cases(college_data),]

# cols - all columns except college name
cols <- c(1,3:ncol(college_subset))

# Convert all strings to numeric, before applying PCA
college_subset[,cols]= apply(college_subset[,cols],2, function(x) as.numeric(as.character(x)))

## 4. Centering and scaling 
# Scale all columns before applying PCA - prerequisite
college_subset <- scale(college_subset[,cols], center = TRUE, scale = TRUE)

## impute a column with means
college_subset2 =as.data.frame(lapply(college_data, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))
write.csv(college_subset2,"dataset_imputed.csv")

#df = data.frame(x = 1:20, y = c(1:10,rep(NA,10)))
#df$y[is.na(df$y)] = mean(df$y, na.rm=TRUE)

## impute a column with kmeans
library("VIM")
?kNN
kNN(college_data, variable=college_data$COUNT_WNE_INC3_P10, k=2)

# Classify the response variable - 
df <- data.frame(college_subset$MN_EARN_WNE_P8)

df.2 <- lapply(df, function(x) table(cut(x, breaks = c(0, 30000, 75000, Inf))))
output <- matrix(unlist(df.2), ncol = 3, byrow = TRUE)

colnames(output) <- c("0-30000","30001-75000", ">75001")
rownames(output) <- c("low","med", "high")

output <- as.data.frame(output)

## 5. Feature selection
## 5 a) PCA 
# Apply PCA for all columns except college name
college_pca <- prcomp(college_subset)


## 6. Methods
## a) Logistic regression






