setwd('~/workspace/ml/college-score-card-ml')
#college_data <- read.csv("final_dataset.csv",header = TRUE, na.strings=c("NA", "NULL"))
college_data <- read.csv("final_dataset.csv",header = TRUE,na.strings=c("NA", "NULL"))

## Original missing value counts
missing.values.counts <- sapply(college_data, function(x) sum(is.na(x)))
missing.values.counts.df <- data.frame(as.list(missing.values.counts))
write.csv(missing.values.counts.df,"missing_values_counts.csv")


df <- college_data
df <- df[!(is.na(df$LOCALE) | df$LOCALE==""), ]
df <- df[!(is.na(df$RELAFFIL) | df$RELAFFIL==""), ]
df <- df[!(is.na(df$PCIP01) | df$PCIP01==""), ]
df <- df[!(is.na(df$UGDS) | df$UGDS==""), ]
df <- df[!(is.na(df$MN_EARN_WNE_P10) | df$MN_EARN_WNE_P10==""), ]
df$PCTPELL[is.na(df$PCTPELL)] <- 0
df$PCTFLOAN[is.na(df$PCTFLOAN)] <- 0
df$UG25ABV[is.na(df$UG25ABV)] <- 0
df$PPTUG_EF[is.na(df$PPTUG_EF)] <- 0

df =as.data.frame(lapply(df, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))

sd <- sapply(df, function(x) sum(is.na(x)))
sd <- data.frame(as.list(sd))

df$CAT_EARN<-cut(df$MN_EARN_WNE_P10, c(0,30000,75000,max(df$MN_EARN_WNE_P10)),labels=c(0,1,2))

## 70% of the sample size
smp_size <- floor(0.70 * nrow(df))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]

write.csv(train,"final_train.csv")
write.csv(test,"final_test.csv")

