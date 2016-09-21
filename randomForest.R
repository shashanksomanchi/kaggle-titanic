library(ggplot2)
library(randomForest)
library(rpart)

set.seed(1)
train <- read.csv("./train.csv", stringsAsFactors = FALSE)
test <- read.csv("./test.csv", stringsAsFactors = FALSE)
extractFeatures <- function(data) {
  features <- c("Pclass", "Age", "Sex")
  fea <- data[,features]
  fea$Age[is.na(fea$Age)] <- median(fea$Age, na.rm=TRUE)
  fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm=TRUE)
  fea$Embarked[fea$Embarked == ""] = "S"
  fea$Sex <- as.factor(fea$Sex)
  fea$Embarked <- as.factor(fea$Embarked)
  return(fea)
}

rf <- randomForest(extractFeatures(train), as.factor(train$Survived), importance = TRUE)
submission <- data.frame(PassengerID = test$PassengerId)
submission$Survived <- predict(rf, extractFeatures(test))
imp <- importance(rf, type=1)

write.csv(submission, file="predictions.csv", row.names = FALSE)






