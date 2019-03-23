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

#Installing package
library(caTools)

set.seed(1)
divide <- sample.split(data$default, SplitRatio = 0.75)
trainning <- subset(data, divide == TRUE)
test <- subset(data, divide == FALSE)

############################################################################
library(h2o)
h2o.init(nthreads = -1)
Classifying <- h2o.deeplearning(y = 'default', 
                                training_frame = as.h2o(trainning),
                                activation = 'Rectifier',
                                hidden = c(100),
                                epochs = 1000)
pred <- h2o.predict(Classifying, newdata = as.h2o(test[,-4]))
pred <- pred >= 0.5
pred <- as.vector(pred)
confusion_matrix <- table(test[,4], pred)

library(caret)
confusionMatrix(confusion_matrix)
