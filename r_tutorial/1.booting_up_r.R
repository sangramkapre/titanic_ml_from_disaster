# Load training and testing data
train <- read.csv(file=file.choose(), header=TRUE)
test <- read.csv(file=file.choose(), header=TRUE)

table(train$Survived)
prop.table(table(train$Survived))

# everyone died prediction
test$Survived <- rep(0, 418)

# submission
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "they_all_die.csv", row.names=FALSE)
