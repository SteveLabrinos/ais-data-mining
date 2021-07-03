# Created by: xaniwths

#first
x<-c(1,1,1,1)
y<-c(2,2,2,2)

#cosine
cos(x)
cos(y)

#correlation
cor(x,y)

#euclidean
euclidean <- function(x, y) sqrt(sum((x - y)^2))
euclidean(x, y)


#second
x<-c(0,1,0,1)
y<-c(1,0,1,0)

#cosine
cos(x)
cos(y)

#correlation
cor(x,y)

#euclidean
euclidean <- function(x, y) sqrt(sum((x - y)^2))
euclidean(x, y)

#jaccard
jaccard <- function(x, y) {
  intersection = length(intersect(x, y))
  union = length(x) + length(y) - intersection
  return (intersection/union)
}

jaccard(x, y)


#third
y<-c(0,-1,0,1)
y<-c(1,0,-1,0)

#cosine
cos(x)
cos(y)

#correlation
cor(x,y)

#euclidean
euclidean <- function(x, y) sqrt(sum((x - y)^2))
euclidean(x, y)