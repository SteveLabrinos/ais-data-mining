# Title     : Concreate data analysis
# Objective : Train two different neural networks and compare them
# Created by: steve_lab
# Created on: 29/6/21

# Simple normalize funtion
normalise <- function(x) {
  return( (x - min(x)) / (max(x) - min(x)) )
}


library("readxl")

# Load the file from the input folder
concrete <- read_excel("./input/Concrete_Data.xls")

# Showing the first rows to examine the DataFrame structure
head(concrete)

# Create a normalized DataFrame
n_concrete <- as.data.frame(lapply(concrete, normalise))
# Examining the statistics of the dependent characteristic
summary(n_concrete$Concrete)

# Loading caTools to split the data into traning and testing set
library(caTools)
set.seed(123)
split = sample.split(n_concrete$Concrete, SplitRatio = 0.75)
concrete_train = subset(n_concrete, split == TRUE)
concrete_test = subset(n_concrete, split == FALSE)

# Training a neural network
library(neuralnet)
concrete_model <- neuralnet(Concrete ~ Cement + Slag + Ash + Water + Superplasticizer + CoarseAggregate + FineAggregate+Age, data = concrete_train)
plot(concrete_model)

# Using the compute function to get the prediction results for our neural network
# using the first 8 characteristics, that are also used as input neurons
model_results <- compute(concrete_model, concrete_test[1:8])

# Getting the pridicted strength of the model
predicted_strength <- model_results$net.result
cor(predicted_strength, concrete_test$Concrete)

# Repeating the proccess using a neural network with 5 hidden neurons
concrete_model2 <- neuralnet(Concrete ~ Cement + Slag + Ash + Water + Superplasticizer + CoarseAggregate + FineAggregate+Age,
                             data = concrete_train, hidden = 5)
plot(concrete_model2)

# Using the compute function to get the prediction results for our neural network
# using the first 8 characteristics, that are also used as input neurons
model_results2 <- compute(concrete_model2, concrete_test[1:8])

# Getting the pridicted strength of the model
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$Concrete)

