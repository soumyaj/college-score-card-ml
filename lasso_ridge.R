setwd('~/workspace/ml/college-score-card-ml')

train <- read.csv("train.csv",header = TRUE)
test <- read.csv("test.csv",header = TRUE)
validation <- read.csv("validation.csv",header = TRUE)

### LASSO - works, dont delete

x<- as.matrix(train[, names(train) != "MN_EARN_WNE_P8"])
y<- as.numeric(train$MN_EARN_WNE_P8)
lasso.fit <- glmnet(x,y,alpha=1)
lasso.fit
summary(lasso.fit)

# Coefficients vs L1-norm plot
plot(lasso.fit,label = TRUE)

# Coefficients vs log lambda plot
plot(lasso.fit, xvar = "lambda", label = TRUE)

# Coefficients vs fraction of deviance explained plot
plot(lasso.fit, xvar = "dev", label = TRUE)

# Coefficients for a particular lambda
coef(lasso.fit,s=24)

# LASSO with cross validation
cv.lasso <- cv.glmnet(x=x,y=y,alpha = 1)

# MSE vs log lambda plot
plot(cv.lasso)

# All lambda values
cv.lasso$lambda

## LASSO predictions

newx<- as.matrix(test[, names(test) != "MN_EARN_WNE_P8"])
testy <- as.numeric(test$MN_EARN_WNE_P8)

# Apply model to testing dataset
lasso.prob <- predict(cv.lasso,newx=newx,s=c(4900,3000,570))
lasso.prob
pred <- prediction(lasso.prob, testy)

# calculate probabilities for TPR/FPR for predictions
perf <- performance(pred,"tpr","fpr")
performance(pred,"auc") # shows calculated AUC for model

plot(perf,colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )
