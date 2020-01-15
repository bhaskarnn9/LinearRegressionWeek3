# Title  <- Build a model for predicting prices of cars based on age w/o lm
# Author <- bneella@ltu.edu
# Date   <- 15th January, 2019

# Understand the problem statement
# Understand the data
# Conduct an EDA on the data
# Prepare and process the data
# Build the model
# Evaluation of the model and communication

# clear environment
rm(list = ls(all=TRUE))

# open csv
cars_data = read.csv('R/Toyota_SimpleReg.csv')

# check for na values
sum(is.na(cars_data))

# for the analysis, we don't need id and model as they are insignificant
drop_cols = c('Id', 'Model')
cars_data[drop_cols] <- NULL
str(cars_data)

# rename the 'Age_06_15' column to just the age
names(cars_data)[2] <- 'Age'
str(cars_data)

# scatter plot Age vs Price so visualize the data
plot(cars_data$Age, cars_data$Price, main = 'Price Vs Age', xlab = 'Price', ylab = 'Age')

# plot cor-plot
library(corrplot)
M <- cor(cars_data)
corrplot(M, method = 'number')

# split 70 percent
set.seed(123)
library(caret)
train_rows = createDataPartition(cars_data$Price, p=0.7, list = F)
train_data = cars_data[train_rows, ]
test_data = cars_data[-train_rows, ]

# linear regression
lin_reg = lm(Price ~ Age, data = cars_data)
coefficients(lin_reg)
summary(lin_reg)


