# Title     : Hierachical clustering
# Objective : Create and compare different approaches of H.C.
# Created by: steve_lab
# Created on: 1/7/21

# Creating the vectors based on the exercise assignment
x <- c(0.4005, 0.2148, 0.3457, 0.2652, 0.0789, 0.4548)
y <- c(0.5306, 0.3854, 0.3156, 0.1875, 0.4139, 0.3022)

# Storing the fectors into a DataFrame
data <- data.frame(X = x, Y = y)

# Calculating Euclidead distance for the different points
d <- dist(data, method = "euclidean")
d

# Applying hierarchical clustering to the data using single method
hc1 <- hclust(d, method = "single")
plot(hc1, hang = -1)

# Applying hierarchical clustering to the data using complete method
hc2 <- hclust(d, method = "complete")
plot(hc2, hang = -1)
