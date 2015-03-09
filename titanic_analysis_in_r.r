

library(caret)

set.seed(1234)

titanicTrain <- read.csv(file = "train.csv", header = TRUE)
titanicTest <- read.csv(file = "test.csv", header = TRUE)

inTrain <- createDataPartition(titanicTrain$Survived,
                               p=0.7,
                               list = TRUE)

titanicCV <- titanicTrain[-inTrain, ]
titanicTrain <- titanicTrain[inTrain, ]