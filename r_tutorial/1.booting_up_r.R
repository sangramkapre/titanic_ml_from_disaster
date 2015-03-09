# Load training data
train <- read.csv(file=file.choose(), header=TRUE, stringsAsFactors=FALSE)
test <- read.csv(file=file.choose(), header=TRUE, stringsAsFactors=FALSE)

table(train$Survived)
prop.table(table(train$Survived))

# everyone died prediction
test$Survived <- rep(0, 418)

# submission
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "they_all_die.csv", row.names=FALSE)
