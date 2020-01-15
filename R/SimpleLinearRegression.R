# Title  <- Build a model for predicting prices of cars based on age
# Author <- bneella@ltu.edu
# Date   <- 14th January, 2019

# AGENDA:
# Understand the problem statement
# Understand the data
# EDA - Exploratory Data Analysis
# Clean and process the data
# Model the data
# Evaluation and Communiacation

# In this example, we set Age of the car as independent variable (x) and y (Price of the car) as dependent

# always start by resetting the environment
rm(list = ls(all=T))

# read the input file
cars_data <- read.csv('R/Toyota_SimpleReg.csv')

# get the feel of data
summary(cars_data)
str(cars_data)

# check if there are any na values
sum(is.na(cars_data$Age_06_15))

# since Id and model-name do not hold any value in our context, we would drop them
cols_to_droppped <- c('Id', 'Model')
cars_data[cols_to_droppped] <- NULL

# get the feel of data again after dropping insignifacnt columns
str(cars_data)

# we camn see that col_name : Age_06_15 is very non-intuitive
# let us rename it to just 'Age' for easy inferability
colnames(cars_data)[2] <- 'Age'

# verify if the change has taken effect or not
str(cars_data)

# scatter plot Age vs Price so visualize the data
plot(y=cars_data$Price, x=cars_data$Age, main = 'Toyota : Age vs Price', xlab = 'Age of the car', ylab = 'Price of the car')

# calculate covariance and correlation
cov(cars_data$Age, cars_data$Price)
cor_data = cor(cars_data$Age, cars_data$Price)
cor_data

# plot cor-plot
library(corrplot)
M <- cor(cars_data)
corrplot(M, method = 'number')

# let's split 70% of the data for training and rest for test
set.seed(123)
library(caret)
train_indices <- createDataPartition(cars_data$Price, p=0.7, list = F)
class(train_indices)
cars_train = cars_data[train_indices,]
cars_test = cars_data[-train_indices]

# model building
lin_reg <- lm(Price ~ Age, data = cars_train)
coefficients(lin_reg)
summary(lin_reg)
