# Title     : Decatholon clustering
# Objective : Cluster the Decathlon dataset with trees and k-means algorithm
# Created by: steve_lab
# Created on: 22/4/21

library(ggplot2)
library(FactoMineR)
library(reshape2)

# load the Decathlon dataset
# the dataset contains 10 different competitions
data(decathlon)

# pre-processing the data frame
# keep only the Olympic Games results
decathlon <- subset(decathlon, Competition == "OlympicG")
str(decathlon)
summary(decathlon)
# check for missing values
any(is.na(decathlon))

# Add the Rank to the athlete in the row names
rownames(decathlon) <- paste(1:28, rownames(decathlon))

# Fix the column (events) names so they don't start with a number
names(decathlon) <- c("X100m", "Long.Jump", "Shot.Put", "High.Jump", "X400m",
    "X110m.Hurdle", "Discus", "Pole.Vault", "Javeline", "X1500m", "Rank", "Points",
    "Competition")

# Create copy of decathlon data, but add column Athlete and it's Rank
decathlon.with.athlete.name <- data.frame(Athlete = factor(row.names(decathlon),
    levels = row.names(decathlon)), decathlon)

# Remove the Rank column and Competition column
decathlon.with.athlete.name$Rank <- NULL
decathlon.with.athlete.name$Competition <- NULL

# Exploring the data
# First visualize the data, after we change types from wide format to long, so the ggplot2 can use it
decathlon.long <- melt(decathlon.with.athlete.name, id.vars = "Athlete", value.name = "Score",
    variable.name = "Event")
decathlon
# Plot the data using ggplot2
gg <- ggplot(decathlon.long, aes(y = Score, x = 1))
gg <- gg + geom_violin(alpha = 0.2, fill = "blue")
gg <- gg + facet_wrap(~Event, ncol = 3, scales = "free_y")
gg <- gg + geom_point(alpha = 0.7)
gg

# Normalize the decatlhon data (except first colum which is name)
decathlon.normal <- as.data.frame(scale(decathlon.with.athlete.name[-1]))
summary(decathlon.normal)

# Add athlete names
decathlon.normal <- data.frame(Athlete = factor(row.names(decathlon), levels = row.names(decathlon)),
    decathlon.normal)

decathlon.normal[, c("X100m", "X400m", "X110m.Hurdle", "X1500m")] <- decathlon.normal[,
    c("X100m", "X400m", "X110m.Hurdle", "X1500m")] * -1

# Convert normalized data from wide to long format needed for ggplot2
decathlon.normal.long <- melt(decathlon.normal, id.vars = "Athlete", value.name = "Z.Score",
    variable.name = "Event")

gg <- ggplot(decathlon.normal.long, aes(x = Event, y = Z.Score))
gg <- gg + geom_bar(stat = "identity", fill = "blue", alpha = 0.5)
gg <- gg + coord_flip()
gg <- gg + facet_wrap(~Athlete, ncol = 3)
gg

# Reverse the order of the athlete so the graph is sorted from the best to
# the worst
decathlon.normal.long$Athlete <- with(decathlon.normal.long, factor(Athlete,
    levels = rev(levels(Athlete))))

gg <- ggplot(decathlon.normal.long, aes(x = Athlete, y = Z.Score))
gg <- gg + geom_bar(stat = "identity", fill = "blue", alpha = 0.5)
gg <- gg + coord_flip()
gg <- gg + facet_wrap(~Event, ncol = 2)
gg

# Clustering
# similarities between athletes, or in other words, which athletes have similar profiles

# create the distance matrix with Euclidean distance
dist_mat <- dist(decathlon.normal, method = 'euclidean')

# Hierarchical Clustering
# A. with ward
HC_ward <- hclust(dist_mat, method = "ward.D2")
plot(HC_ward, xlab = "", ylab = "", yaxt = "n", main = "Athletes")

# B. with average distance
HC_avg <- hclust(dist_mat, method = "average")
plot(HC_avg, xlab = "", ylab = "", yaxt = "n", main = "Athletes")

#  same for events – try to find events that are similar
HC_ward_t <- hclust(d = dist(t(decathlon.normal[2:11])), method = "ward")
plot(HC_ward_t, xlab = "", ylab = "", yaxt = "n", main = "Events")

#  same for events – try to find events that are similar
HC_avg_t <- hclust(d = dist(t(decathlon.normal[2:11])), method = "average")
plot(HC_avg_t, xlab = "", ylab = "", yaxt = "n", main = "Events")

# Number of final clusters
k <- 2
# cutting the htrees
cut_ward <- cutree(HC_ward, k)
cut_avg <- cutree(HC_avg, k)

# Plotting the cutted dendrograms
plot(HC_ward, xlab = "", ylab = "", yaxt = "n", main = "Athletes")
rect.hclust(HC_ward, k = k, border = 2:6)
abline(h = 3, col = 'red')

# Plotting the cutted dendrograms
plot(HC_avg, xlab = "", ylab = "", yaxt = "n", main = "Athletes")
rect.hclust(HC_avg, k = k, border = 2:6)
abline(h = 3, col = 'red')


library(clValid)
metric_avg <- dunn(dist_mat, cut_avg)
print(paste("Dunn for avg method HC: ",metric_avg))


# k-means algorithm
# setting the random seed and the number of required clusters
set.seed(20)
k_clusters <- kmeans(decathlon.normal[2:11], k)

decathlon_k <- as.factor(k_clusters$cluster)
str(decathlon_k)

# visualize the k-means
library(factoextra)
fviz_cluster(k_clusters, data = decathlon.normal[2:11])