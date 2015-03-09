# Feature Engineering

# Load training and testing data
train <- read.csv(file=file.choose(), header=TRUE)
test <- read.csv(file=file.choose(), header=TRUE)

# bagging
sample(1:10, replace = TRUE)

# random forests cannot handle missing values, manual work is needed

######## feature engineering ########
# extract titles from each name
test$Survived <- NA
combined <- rbind(train, test)
combined$Name <- as.character(combined$Name)
# extract title from name
combined$Title <- sapply(combined$Name, FUN = function(x) {strsplit(x, split='[,.]')[[1]][2]})
# strip off spaces from beginning of the titles
combined$Title <- sub(' ', '', combined$Title)
table(combined$Title)
# merge titles that are not very common
combined$Title[combined$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combined$Title[combined$Title %in% c('Capt','Don','Major','Sir')] <- 'Sir'
combined$Title[combined$Title %in% c('Dona','Lady','the Countess','Jonkheer')] <- 'Lady'
# convert back to factor
combined$Title <- factor(combined$Title)

# find out family size
combined$FamilySize <- combined$SibSp + combined$Parch + 1

# find a group of people belonging to the same family using surname
combined$Surname <- sapply(combined$Name, FUN = function(x) {strsplit(x, split='[,.]')[[1]][1]})
combined$FamilyId <- paste(as.character(combined$FamilySize), combined$Surname, sep = "")

# knock out small family size
combined$FamilyId[combined$FamilySize <= 2] <- 'Small'
table(combined$FamilyId)
famIDs <- data.frame(table(combined$FamilyId))
famIDs <- famIDs[famIDs$Freq <= 2, ]

# overwrite familyIDs that were not correctly identified
combined$FamilyId[combined$FamilyId %in% famIDs$Var1] <- 'Small'
combined$FamilyId <- factor(combined$FamilyId)

#####################################

summary(combined$Age)

# process Age variable to impute new values
library(rpart)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combined[!is.na(combined$Age),], method="anova")
combined$Age[is.na(combined$Age)] <- predict(Agefit, combined[is.na(combined$Age),])

summary(combined)
summary(combined$Embarked)

combined$Embarked[which(combined$Embarked == '')] = 'S'
combined$Embarked <- factor(combined$Embarked)

summary(combined$Fare)
combined$Fare[which(is.na(combined$Fare))] <- median(combined$Fare, na.rm=TRUE)

# random forest cannot handle factor variable with more than 32 levels
# reduce FamilyId's levels
combined$FamilyId2 <- combined$FamilyId
# change cut-off for small family from 2 to 3
combined$FamilyId2 <- as.character(combined$FamilyId2)
combined$FamilyId2[combined$FamilySize <= 3] <- 'Small'
combined$FamilyId2 <- factor(combined$FamilyId2)

# get back training and testing datasets from combined
train <- combined[1:891, ]
test <- combined[892:1309, ]

# apply randomForest
library(randomForest)
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass+Sex+Age+SibSp+Parch+Fare+
                        Embarked+Title+FamilySize+FamilyId2,
                    data=train, importance=TRUE, ntree=2000)

varImpPlot(fit)

# make predictions
predictions <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = predictions)
write.csv(submit, file = "firstRandomForestModel.csv", row.names = FALSE)

# using PARTY package
library(party)
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyId,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))

# make predictions again
predictions <- predict(fit, test, OOB = TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = predictions)
write.csv(submit, file = "finalRandomForestModel.csv", row.names = FALSE)
