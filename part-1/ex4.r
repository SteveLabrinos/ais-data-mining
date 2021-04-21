# Title     : ex4
# Objective : study M1, M2 classification models with ROC curve
# Created by: steve_lab
# Created on: 20/4/21

library("plotROC")
library("caret")
# Data array
True_Class <- c("+", "+", "-", "-", "+", "+", "-", "-", "+", "-")
M1 <- c(0.73, 0.69, 0.44, 0.55, 0.67, 0.47, 0.08, 0.15, 0.45, 0.35)
M2 <- c(0.61, 0.03, 0.68, 0.31, 0.45, 0.09, 0.38, 0.05, 0.01, 0.04)
posterior <- data.frame(True_class = True_Class, M1 = M1, M2 = M2)
# displaying data frame to the console
posterior
# i) Plotting both models displaying the ROC curve
# transorm data into long form to be suitable for gglpot
longtest <- melt_roc(posterior, "True_class", c("M1", "M2"))

# ROC curve Plotting
ggplot(longtest, aes(d = D, m = M, color = name)) +
  geom_roc() +
  style_roc(major.breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
            xlab = "False Positive Fraction",
            ylab = "True Positive Fraction",
            guide = TRUE,
            theme = theme_bw)

# ii) With threshold t = 0.5 calculate for M1 Pressision, Recall, F-measure
threshold <- 0.5
Pred.m1 <- as.factor(ifelse(posterior$M1 > threshold, "+", "-"))
Predicted.m1 <- ordered(Pred.m1, levels = c("+", "-"))
Actual.m1 <- ordered(posterior$True_class, levels = c("+", "-"))
confusion_matrix_1 <- confusionMatrix(table(Actual = Actual.m1, Predicted = Predicted.m1))
# getting presision (Sensitivity), accuracy (PPV), F-measure
presision_1 <- confusion_matrix_1$byClass['Sensitivity']
recall_1 <- confusion_matrix_1$byClass['Pos Pred Value']
f_measure_1 <- 2 * ((presision_1 * recall_1) / (presision_1 + recall_1))
print(paste('M1 Presision:', presision_1))
print(paste('M1 Recall:', recall_1))
print(paste('M1 F-measure:', f_measure_1))


# iii) With threshold t = 0.5 calculate for M2 Pressision, Recall, F-measure
threshold <- 0.5
Pred.m2 <- as.factor(ifelse(posterior$M2 > threshold, "+", "-"))
Predicted.m2 <- ordered(Pred.m2, levels = c("+", "-"))
Actual.m2 <- ordered(posterior$True_class, levels = c("+", "-"))
confusion_matrix_2 <- confusionMatrix(table(Actual = Actual.m2, Predicted = Predicted.m2))
# getting presision (Sensitivity), accuracy (PPV), F-measure
presision_2 <- confusion_matrix_2$byClass['Sensitivity']
recall_2 <- confusion_matrix_2$byClass['Pos Pred Value']
f_measure_2 <- 2 * ((presision_2 * recall_2) / (presision_2 + recall_2))
print(paste('M2 Presision:', presision_2))
print(paste('M2 Recall:', recall_2))
print(paste('M2 F-measure:', f_measure_2))

# iv) With threshold t = 0.1 calculate for M1 Pressision, Recall, F-measure
threshold <- 0.1
Pred.m3 <- as.factor(ifelse(posterior$M1 > threshold, "+", "-"))
Predicted.m3 <- ordered(Pred.m3, levels = c("+", "-"))
Actual.m3 <- ordered(posterior$True_class, levels = c("+", "-"))
confusion_matrix_3 <- confusionMatrix(table(Actual = Actual.m3, Predicted = Predicted.m3))
# getting presision (Sensitivity), accuracy (PPV), F-measure
presision_3 <- confusion_matrix_3$byClass['Sensitivity']
recall_3 <- confusion_matrix_3$byClass['Pos Pred Value']
f_measure_3 <- 2 * ((presision_3 * recall_3) / (presision_3 + recall_3))
print(paste('M1 (t=0.1) Presision:', presision_3))
print(paste('M1 (t=0.1) Recall:', recall_3))
print(paste('M1 (t=0.1) F-measure:', f_measure_3))


cf_1 <- as.table(confusion_matrix_1)
fpr_1 <- cf_1[2, 1] / sum(cf_1[2, 1:2])

cf_3 <- as.table(confusion_matrix_3)
fpr_3 <- cf_3[2, 1] / sum(cf_3[2, 1:2])

auc_1 <- recall_1 * (1 - fpr_1)
auc_3 <- recall_3 * (1 - fpr_3)

print(paste('M1 (t=0.5) Area Under Curve: ', auc_1))
print(paste('M1 (t=0.1) Area Under Curve: ', auc_3))