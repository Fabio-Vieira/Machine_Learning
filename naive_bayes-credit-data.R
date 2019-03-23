data <- read.csv("risco-credito.csv")
library(e1071)

classifying <- naiveBayes(x = data[,1:4], y = data[,5])
classifying

#história: boa, divida: alta, garantias: nenhuma, renda: > 35
#história: ruim, divida: alta, garantias: adequada, renda: < 15

historia <- c("boa", "ruim")
divida <- c("alta", "alta")
garantias <- c("nenhuma", "adequada")
renda <- c("acima_35", "0_15")

df <- data.frame(historia, divida, garantias, renda)

predicting <- predict(classifying, newdata = df[2,], "raw")
predicting
