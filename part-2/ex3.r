# Title     : Data analysis
# Objective : Examine octapus dataset with a signle variable
# Created by: chaniotis - steve_lab
# Created on: 3/7/21


# Load the file into a table
octapus <- read.table("./input/OctopusF.txt", header = TRUE)

# Calculate the statistics of the set (Mean and Quartiles)
summary(octapus)
sd(octapus$Weight)

#Create histogram
hist(octapus$Weight,
     col = "blue",
     nclass = 12,
     freq = FALSE,
     xlab = "Weight",
     ylab = "Density",
     main = "Weight",
     las = 1)

#Data regulation check
nWeight <- rnorm(length(octapus$Weight), mean = mean(octapus$Weight), sd = sd(octapus$Weight))
qqnorm(octapus$Weight, pch = 19)
qqline(nWeight, col = "red")
# Testing if the data are nornally distributed
shapiro.test(octapus$Weight)

#Confidence interval
t.test(octapus$Weight)

