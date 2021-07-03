# Created by: xaniwths

#Insert rpart
library(rpart)

#load "kyphosis"
data(kyphosis)

#Dataset summary
summary(kyphosis)

#Boxplot for number variable
boxplot(kyphosis$Number, ylab = "?umber" , xlab = " Incidence of ?yphosis", main = "?umber of vertebrae involved",las=1)

#Find the outliers 
outliers <- boxplot.stats(kyphosis$Number)$out
outliers

#Which lines correspond to the specific data
out_ind <- which(kyphosis$Number %in% c(outliers))
out_ind

#Repeat with Identify function
plot(kyphosis$Number)
identify(kyphosis$Number)