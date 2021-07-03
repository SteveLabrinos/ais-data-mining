# Title     : Correlation between different characteristics
# Objective : Examine the returns of a company to find correlations
# Created by: steve_lab
# Created on: 2/7/21

library(readxl)

# Load data into a DataFrame
mf <- read_excel( "./input/mf.xls")

# Using a correlation matrix between features
cor(mf[c("Complaint Code", "Manufacturing Plant" , "Dollar Claim Amount" , "Shift")])
# Using scatterplots matrices to vicualize the correlation
pairs(mf[c("Complaint Code", "Manufacturing Plant" , "Dollar Claim Amount" , "Shift")])

# Starting examination of all features
# Corellation between Claims and Shifts
dollar_shift <- table(
    mf$`Dollar Claim Amount`,
    mf$Shift)
head(dollar_shift)
boxplot(
    mf$`Dollar Claim Amount`~mf$Shift,
    pch = 1, method = "jitter",
    vertical = TRUE)

# Corellation between Claims and Complains
dollar_complaint <- table(
    mf$`Dollar Claim Amount`,
    mf$`Complaint Code`)
head(dollar_complaint)
boxplot(
    mf$`Dollar Claim Amount`~mf$`Complaint Code`,
    pch = 1,
    method = "jitter",
    vertical = TRUE)

# Corellation between Claims and Plants
dollar_plant <- table(
    mf$`Dollar Claim Amount`,
    mf$`Manufacturing Plant`)
head(dollar_plant)
boxplot(
    mf$`Dollar Claim Amount`~mf$`Manufacturing Plant`,
    pch = 1, method = "jitter",
    vertical = TRUE)

# Corellation between Complaints and Shifts
head(complaint_shift)
boxplot(
    mf$`Complaint Code`~mf$Shift,
    pch = 1,
    method = "jitter",
    vertical = TRUE)

# Corellation between Complaints and Plants
complaint_plant <- table(
    mf$`Complaint Code`,
    mf$`Manufacturing Plant`)
head(complaint_plant)
boxplot(
    mf$`Complaint Code`~mf$`Manufacturing Plant`,
    pch = 1, method = "jitter",
    vertical = TRUE)

# Performing a t-test for complaints and plats
t.test(
    mf$`Complaint Code`,
    mf$`Manufacturing Plant`,
    var.equal = TRUE, conf.level = 0.99)

# Performing a t-test for claims and plats
t.test(
    mf$`Dollar Claim Amount`,
    mf$`Manufacturing Plant`,
    var.equal = TRUE, conf.level = 0.98)
