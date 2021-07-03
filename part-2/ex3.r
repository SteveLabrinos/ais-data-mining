# Created on: 29/6/21

#Dataset
df <- read.table("./input/OctopusF.txt", header = TRUE)

#Calculate descriptive measures
#Average
mean(df$Weight)

#Standard deviation
sd(df$Weight)

#Create histogram
hist(df$Weight, col="blue", breaks=5, xlab="Weight", main="Weight", las=1)

#Data regulation check
nWeight<-rnorm(length(df$Weight), mean=mean(df$Weight), sd=sd(df$Weight))
qqnorm(df$Weight, pch=19);qqline(nWeight)

#Confidence interval
t.test(df$Weight)