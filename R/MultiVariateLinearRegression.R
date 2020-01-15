# Title: Predicting price sof Homes in Boston
# Author: "bneella@google.com"
# Date: "13th Jan 2020"

rm(list = ls(all=T))
raw_data = read.csv('R/housing_data.csv')

# output: 'data.frame':	500 obs. of  14 variables:
# $ CRIM : num  0.00632 0.02731 0.02729 0.03237 0.06905 ...
# $ ZN   : num  18 0 0 NA 0 0 12.5 12.5 12.5 12.5 ...
# $ INDUS: num  2.31 7.07 7.07 2.18 2.18 ...
# $ CHAS : int  0 0 0 0 0 0 0 0 0 0 ...
# $ NOX  : num  0.538 0.469 0.469 0.458 0.458 ...
# $ RM   : num  6.57 6.42 7.18 7 7.15 ...
# $ AGE  : num  65.2 78.9 61.1 45.8 54.2 ...
# $ DIS  : num  4.09 4.97 4.97 6.06 6.06 ...
# $ RAD  : int  1 2 2 3 3 3 5 5 5 5 ...
# $ TAX  : int  296 242 242 222 222 222 311 311 311 311 ...
# $ PT   : num  15.3 17.8 17.8 18.7 18.7 ...
# $ B    : num  NA 397 393 395 397 ...
# $ LSTAT: num  4.98 9.14 4.03 2.94 5.33 ...
# $ MV   : num  24 21.6 34.7 33.4 36.2 ...

# Convert CHAS ab d RAD to categorical data type

raw_data$CHAS <- as.factor(raw_data$CHAS)
raw_data$RAD <- as.factor(raw_data$RAD)

# Let us first split the vailable data to train and test by 70 and 30 percent respectively.

set.seed(29)
train_rows <- sample(x=1 : nrow(raw_data), size = 0.7 * nrow(raw_data))
train_data <- raw_data[train_rows, ]
test_data <- raw_data[-train_rows, ]

# Lets us now find missing (na) values

sum(is.na(train_data))

# Missing values Imputation

# install.packages('caret')
library(caret)
#create object with medianImpute values based on trainset
imputer_values <- preProcess(x = train_data, method = 'medianImpute')
sum(is.na(train_data)) # before imputing
# plug medianInpute values in trainset's NA values
train_data <- predict(object = imputer_values, newdata = train_data)
sum(is.na(train_data))
# now plug medianInpute values in trainset's NA values
test_data <- predict(object = imputer_values, newdata = test_data)
sum(is.na(test_data))

# Impute categorical values with mode

library(DMwR)
train_data <- centralImputation(train_data)
test_data <- centralImputation(test_data)

# Scatter Plots

par(mfrow = c(2,2))
par("mar")
plot(raw_data$LSTAT, raw_data$MV, ylab = 'Mean Value', xlab = 'Economic status', main = 'Price Vs Economic Status')
plot(raw_data$CRIM, raw_data$MV, ylab = 'Mean Value', xlab = 'Crime rate', main = 'Price Vs Crime Rate')
plot(raw_data$NOX, raw_data$MV, ylab = 'Mean Value', xlab = 'Nitrous oxides', main = 'Price Vs Nitrous Oxides')
plot(raw_data$INDUS, raw_data$MV, ylab = 'Mean Value', xlab = 'Proportion of non-retail business acres per town', main = 'Price Vs Business Acres / Town')

# Split the numerical and categorical attributes

# First select columns of interest (last column(MV) is our target and we do not want it)
columns_of_interest <- colnames(train_data[, !names(train_data)%in%c('MV')])

numAttr <- c()
catAttr <- c()

for (column in columns_of_interest) {
  if (is.numeric(train_data[,c(column)]) == TRUE) {
    numAttr <- c(numAttr, column)
  } else {
    catAttr <- c(catAttr, column)
  }
}

# Now that we have numerical features segregated, we can go ahead and plot correlation plots for numerical data

library(corrplot)
corrplot(cor(raw_data[, numAttr], use = 'complete.obs'), method = 'number')

# Now we can go ahead and conduct correlation test for categorical data (i.e., chi-sq test)
str(train_data$CHAS)
contingency <- table(train_data$CHAS, train_data$RAD)
chisq.test(contingency)

# Standardizing data

library(caret)
std_model  <- preProcess(train_data[, numAttr], method = c('center', 'scale'))
train_data[, numAttr] <- predict(object = std_model, newdata = train_data[, numAttr])
test_data[, numAttr] <- predict(object = std_model, newdata = test_data[, numAttr])

# Modelling the data

# Basic Data
model_basic <- lm(formula = MV~. , data = train_data)
summary(model_basic)

par(mfrow = c(2,2))
plot(model_basic)

# Step AIC

library(MASS)
model_aic <- stepAIC(model_basic, direction = 'both')
summary(model_aic)

par(mfrow = c(2,2))
plot(model_aic)

# Modifying the model with VIF

library(car)

vif(model_basic)
vif(model_aic)

model_3 <- lm(formula =  MV ~ CRIM + ZN + CHAS + NOX + RM + DIS + RAD + PT + B + LSTAT, data = train_data)
summary(model_3)
vif(model_3)


preds_model <- predict(model_3, test_data[, !(names(test_data) %in% c('MV'))])
summary(preds_model)

library(DMwR)
regr.eval(test_data$MV, preds_model)
