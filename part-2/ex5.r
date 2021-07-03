# Created by: xaniwths

#Faithful dataset
faithful <- read.table("C:/faithfull.txt",header=TRUE)

#Linear regression model
eruption.lm = lm(eruptions ~ waiting, data=faithful)
coeffs = coefficients(eruption.lm)
coeffs 

#Next explosion estimation if the expexted time from last explosion is 80 mins
waiting = 80
duration = coeffs[1] + coeffs[2]*waiting 
duration 

#Coefficient of determination
summary(eruption.lm)$r.squared

#Statistically significant relationship between two variables
summary(eruption.lm)

#Create an 95% confidence interval for the eruption duration variable for a given standby time of 80 min
newdata = data.frame(waiting=80)
predict(eruption.lm, newdata, interval="confidence") 

#Create an 95% prediction of the eruption duration variable given a waiting time of 80 min
predict(eruption.lm, newdata, interval="predict") 

#Residual plot
eruption.res = resid(eruption.lm)
plot(faithful$waiting, eruption.res, ylab="Residuals", xlab="Waiting Time", main="Faithful Eruptions") 
abline(0, 0)

#Standardied Residuals plot
eruption.stdres = rstandard(eruption.lm)
plot(faithful$waiting, eruption.stdres, ylab="Standardized Residuals", xlab="Waiting Time", main="Faithful Eruptions") 
abline(0, 0) 

#Q-Q plot to examine the correlation between standardized residuals and normal scores
qqnorm(eruption.stdres, ylab="Standardized Residuals", xlab="Normal Scores", main="Faithful Eruptions") 
qqline(eruption.stdres)
