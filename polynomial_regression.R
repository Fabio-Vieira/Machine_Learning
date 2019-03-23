data <- read.csv("plano-saude2.csv")
plot(data[,1], data[,2])

#simple linear regression
reg1 <- lm(formula = custo ~ idade, data = data)
summary(reg1)
cc1 <- summary(reg1)$r.squared

idade <- 40
df <- data.frame(idade)
prediction1 <- predict(reg1, newdata = df)

library(ggplot2)
ggplot() + geom_point(aes(x = data$idade, y = data$custo), colour = 'blue') +
  geom_line(aes(x = data$idade, y = predict(reg1, newdata = data[-2])), colour = 'red')

#polynomial regression
data2 <- as.data.frame(cbind(idade = data$idade, idade2 = (data$idade^2), 
                             idade3 = (data$idade^3), custo = data$custo))
reg2 <- lm(formula = custo ~ idade + idade2 + idade3, data = data2)
summary(reg2)  
cc2 <- summary(reg2)$r.squared
idade <- 40
idade2 <- 40^2
idade3 <- 40^3
df2 <- data.frame(cbind(idade = idade, idade2 = idade2, idade3 = idade3))
prediction2 <- predict(reg2, newdata = df2)

ggplot() + geom_point(aes(x = data2$idade, y = data2$custo), colour = 'blue') +
  geom_line(aes(x = data2$idade, y = predict(reg2, newdata = data2[-4])), colour = 'red')


