house <- read.csv("kc_house_data.csv")
house$id <- NULL
house$date <- NULL
house$sqft_living15 <- NULL
house$sqft_lot15 <- NULL
house$sqft_basement <- NULL

house <- as.data.frame(cbind(house, array(NA, dim = c(nrow(house),60))), row.names = NULL)
colnames(house)<- c("price","bedrooms","bathrooms","sqft_living","sqft_lot","floors",
                    "waterfront","view","condition","grade","sqft_above","yr_built",
                    "yr_renovated","zipcode","lat","long","bedrooms2","bedrooms3",
                    "bedrooms4","bedrooms5","bathrooms2","bathrooms3","bathrooms4",
                    "bathrooms5","sqft_living2","sqft_living3","sqft_living4","sqft_living5",
                    "sqft_lot2","sqft_lot3","sqft_lot4","sqft_lot5","floors2","floors3",
                    "floors4","floors5","waterfront2","waterfront3","waterfront4","waterfront5",
                    "view2","view3","view4","view5","condition2","condition3","condition4",
                    "condition5","grade2","grade3","grade4","grade5","sqft_above2","sqft_above3",
                    "sqft_above4","sqft_above5","yr_built2","yr_built3","yr_built4","yr_built5",
                    "yr_renovated2","yr_renovated3","yr_renovated4","yr_renovated5","zipcode2",
                    "zipcode3","zipcode4","zipcode5","lat2","lat3","lat4","lat5","long2",
                    "long3","long4","long5")

#filling the data set with the variables to higher powers
k <- 1;h <- 16
for(i in 2:16){
  house[,h+k] <- house[,i]^2
  house[,h+k+1] <- house[,i]^3
  house[,h+(k+2)] <- house[,i]^4
  house[,h+(k+3)] <- house[,i]^5
  h <- k+h+3
}

#splitting the data set
library(caTools)
set.seed(1)
divide <- sample.split(house$price, SplitRatio = 0.70)
train <- subset(house, divide == TRUE)
test <- subset(house, divide == FALSE)

#fitting the model
reg <- lm(formula = price ~ ., data = train)
summary(reg)
predictions <- predict(reg, newdata = test[,-1])
#Mean absolute error
mean(abs(test[['price']] - predictions))

library(miscTools)
#calculating r-squared
cc <- rSquared(test[['price']], resid = test[['price']] - predictions)
