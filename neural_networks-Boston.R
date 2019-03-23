library(MASS)
library(caret)
library(neuralnet)

#Loading Boston data set from MASS package
DataFrame <- Boston
help("Boston")
str(DataFrame)
dim(DataFrame)
head(DataFrame)
summary(DataFrame)

#Histogram of response
hist(DataFrame$medv)

#Scaling all the variables
maxV <- apply(DataFrame, 2, max)
minV <- apply(DataFrame, 2, min)
DataFrame <- as.data.frame(scale(DataFrame, center = minV, scale = maxV - minV))

#Splitting the data set

ind <- sample(1:nrow(DataFrame), size = 400)
train <- DataFrame[ind,]
test <- DataFrame[-ind,]

allVars <- colnames(DataFrame) #all column names
predictorVars <- allVars[!allVars%in%"medv"] #all column names except the response
#Creating a formula, because neuralnet function does not accept y ~ . as formula
predictorVars <- paste(predictorVars, collapse = "+") #adding all column names
form <- as.formula(paste("medv ~", predictorVars, collapse = "+")) #creating a formula with medv and added predictor names

#In order to decide the number of hidden layers, I'll be using and answer from this post
#https://stats.stackexchange.com/questions/181/how-to-choose-the-number-of-hidden-layers-and-nodes-in-a-feedforward-neural-netw
alpha <- 2:10
MSE <- NULL
for(i in 1:9){
  nHidden <- nrow(train)/(alpha[i] * (13 + 1))
  neural <- neuralnet(formula = form, hidden = trunc(nHidden), linear.output = T,
                    data = train)

  #Predicting
  predictions <- compute(neural, test[,c(1:13)])
  #Unscaling values
  predictions <- predictions$net.result*(max(test$medv) - min(test$medv)) + (min(test$medv))
  actualValues <- (test$medv)*(max(test$medv) - min(test$medv)) + (min(test$medv)) 

  #Mean square error
  MSE[i] <- sum((predictions - actualValues)^2)/nrow(test)
  print(cbind(alpha[i], MSE[i])) #checking which alpha produces the least MSE
}

NNfit <- neuralnet(formula = form, hidden = 6, linear.output = T, data = train)
plot(NNfit)
#Predicting
predictions <- compute(NNfit, test[,c(1:13)])
#Unscaling values
predictions <- predictions$net.result*(max(test$medv) - min(test$medv)) + (min(test$medv))
actualValues <- (test$medv)*(max(test$medv) - min(test$medv)) + (min(test$medv)) 

plot(test$medv, predictions, col = 'blue', main = 'Real vs Predicted', pch = 1,
     cex = 0.9, type = 'p', xlab = 'Actual', ylab = 'Predicted')
abline(0, 1, col = 'black')
