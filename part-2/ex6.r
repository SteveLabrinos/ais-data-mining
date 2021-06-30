# Title     : Linear Regression of Insurance Data
# Objective : Perform a lr model to study how other characteristics affect charges
# Created by: steve_lab
# Created on: 30/6/21

# Load the csv file into a dataframe using a relative path
insurance <- read.csv("./input/insurance.csv", stringsAsFactors = T)

# Exploring and studing the data
# Showing the first reconds to examine the data structure
head(insurance)

# Also styding the statistcs of the data set for better examination
# and watch the proportial distribution of th nominal features (sex, smoke, region)
summary(insurance)

# Charges is our depented variable so we plot a histogram to better show the variances
hist(insurance$charges, breaks = 30)

# Using a correlation matrix between nominal features
cor(insurance[c("age", "bmi", "children", "charges")])
# Using scatterplots matrices to vicualize the correlation
pairs(insurance[c("age", "bmi", "children", "charges")])

# importing psych package to visualize correlation in more interesting ways
library('psych')
pairs.panels(insurance[c("age", "bmi", "children", "charges")])

# Train a linear regression model on the data with charges as a depented variable
insurance_model <- lm(charges ~ ., data = insurance)
# Evaluating model performance
summary(insurance_model)

# Imporiving the model
# Removing sex and region variable as they are not significant
insurance_model_2 <- lm(charges ~ age + children + bmi + smoker,
                        data = insurance)
# Evaluating model performance
summary(insurance_model_2)

# Applying the square of age as the increase in age and the charges
# is not on a linear fashion
# Also creating 2 categories for the bmi
insurance$age2 <- insurance$age^2
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)
insurance$bmi25 <- ifelse((insurance$bmi >= 25 | insurance$bmi <= 18.5), 1, 0)

# Creating our improved model
insurance_model3 <- lm(charges ~ age + age2 + children + bmi25 + sex +
                   bmi30*smoker + region, data = insurance)

summary(insurance_model3)
