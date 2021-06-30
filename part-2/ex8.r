# Title     : Unsupervised learning
# Objective : Using DBSCAN and k-means in a given data set
# Created by: steve_lab
# Created on: 23/4/21

# WSS function to plot the best k clusters for k-means using elbow rule
wssplot <- function(data, nc) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    wss[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:nc, wss, type = 'b', xlab = 'Number of Clusters',
       ylab = 'within groups sum of squares')
}

# Creating X and Y vectors based on the exercise diagram
x <- c(1, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 7, 8, 9, 9)
y <- c(5, 6, 2, 3, 5, 6, 7, 8, 3, 5, 7, 8, 4, 6, 8, 4, 5, 6, 7, 4, 3, 2, 5)
# Creating the DataFrame
data <- data.frame(x = x, y = y)

# 1rst step is using DBSCAN with e = 1 and MinPts = 3
library('fpc')
dbscan(data, eps = 1, MinPts = 3, showplot = TRUE)

# Using wss to find the best number of clusters for k-means
wssplot(data, nrow(data) - 1)

# Using 4 clusters for k-means because the elbow rule doesn't give
# a solid result and the closest point to the rule is 4
# set.seed(123)
kc <- kmeans(data, 4)
plot(data, col = kc$cluster)
points(kc$centers[, c("X", "Y")], col = 1:4, pch = 8, cex = 2)

