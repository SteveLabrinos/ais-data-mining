# Title     : 2 sample T - test with independent groups
# Objective : Compare the GPM means for different transmittion types  using t-test
# Created by: steve_lab
# Created on: 29/6/21

# Printing the first results of the dataset to examine the structure
head(mtcars)

# plot the boxes of the mpg in transittion groups (namual, automatic)
boxplot(mtcars$mpg ~ mtcars$am)

# Using the t.test function find the means for each transmission
t.test(mtcars$mpg ~ mtcars$am, conf = 0.95)
