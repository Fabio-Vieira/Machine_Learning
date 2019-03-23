data <- read.csv("risco-credito.csv")
summary(data)

data <- data[data$risco != "moderado",]

fit <- glm(formula = risco ~ ., family = binomial(), data = data)

historia <- c("boa", "ruim")
divida <- c("alta", "alta")
garantias <- c("nenhuma", "adequada")
renda <- c("acima_35", "0_15")

df <- data.frame(historia, divida, garantias, renda)

probab <- predict(fit, type = 'response', newdata = df)
probab

(response <- ifelse(probab > 0.5, "baixo", "alto"))
