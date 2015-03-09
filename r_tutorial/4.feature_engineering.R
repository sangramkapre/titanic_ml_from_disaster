# Feature Engineering

# Load training and testing data
train <- read.csv(file=file.choose(), header=TRUE)
test <- read.csv(file=file.choose(), header=TRUE)

# lets start with Name field
train$Name[1]

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

# get back training and testing datasets from combined
train <- combined[1:891, ]
test <- combined[892:1309, ]

# fit tree model to newly created data
fit <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+FamilySize+FamilyId,
             data = train, method = "class")

# make preditcions
predictions <- predict(fit, test, type="class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = predictions)
write.csv(submit, file = "predictions_after_feature_engineering.csv", row.names = FALSE)
