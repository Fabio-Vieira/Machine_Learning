#Random Forest do not accept NA's
data <- read.csv("credit-data.csv")
data$clientid <- NULL
summary(data)

#invalid_age <- data[data$age < 0 & !is.na(data$age),]

#Delete the entire column
#data$age <- NULL

#Delete just the invalid data
#data <- data[data$age > 0,]

#Manually fill in the blanks

#Calculate the mean of the variable
mean.age <- mean(data$age[data$age > 0], na.rm = T)

data$age <- ifelse(data$age < 0, mean.age, data$age)
summary(data$age)

#Missing data
data$age <- ifelse(is.na(data$age) == T, mean.age, data$age)
summary(data$age)

#Escalonating variables
data[,-4] <- scale(data[,-4])

#encoding the response
data$default <- factor(data$default, levels = c(0,1))

#Installing package
library(caTools)

set.seed(1)
divide <- sample.split(data$default, SplitRatio = 0.75)
trainning <- subset(data, divide == TRUE)
test <- subset(data, divide == FALSE)

library(randomForest)
Classifying <- randomForest(x = trainning[-4], y = trainning$default, ntree = 10)
prediction <- predict(Classifying, newdata = test[-4])
confusion_matrix <- table(test[,4],prediction)
confusion_matrix

library(caret)
confusionMatrix(confusion_matrix)
