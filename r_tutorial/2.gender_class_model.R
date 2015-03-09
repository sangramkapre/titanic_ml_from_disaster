# Load training and testing data
train <- read.csv(file=file.choose(), header=TRUE)
test <- read.csv(file=file.choose(), header=TRUE)

# disaster was famous for saving "women and children first"
summary(train$Sex)

# two way comparison for proportion table
prop.table(table(train$Sex, train$Survived))

# row wise proportion
prop.table(table(train$Sex, train$Survived), 1)

# as more females survived
test$Survived <- 0
test$Survived[test$Sex == "female"] <- 1

# check out Age variable
summary(train$Age)

# create a new variable Child (for Age < 18)
train$Child <- 0
train$Child[train$Age < 18] <- 1

# survival proportions for both gender and age
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# bin the Fare variable
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

# apply aggregate on Fare and Sex
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# new prediction based on Sex, Passenger class and Fare
test$Survived <- 0
test$Survived[test$Sex == "female"] <- 1
test$Survived[test$Sex == "female" & test$Pclass == 3 & test$Fare >= 20] <- 0

# submission
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "gender_class_model_predictions.csv", row.names=FALSE)
