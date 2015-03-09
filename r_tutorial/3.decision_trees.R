# Tree based algorithm for classification

# Load training and testing data
train <- read.csv(file=file.choose(), header=TRUE)
test <- read.csv(file=file.choose(), header=TRUE)

# using rpart library : Recursive Partitioning and Regression Trees
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train, method="class")

# plot decision tree
plot(fit)
text(fit)

# plot fancy decision tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)

# make predictions
predictions <- predict(fit, test, type="class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = predictions)
write.csv(submit, file = "tree_based_prediction.csv", row.names = FALSE)

# override the default control parameters for rpart
# (example of overfitting)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Fare + Embarked,
             data = train, method = "class", control=rpart.control(minsplit = 2, cp = 0))
fancyRpartPlot(fit)
